#' ggwordtree: a Google-style Word Tree built from ggplot2 (+ optional ggiraph)
#'
#' Pipeline (all tidyverse-friendly, each step returns a tibble):
#'   wt_build()  -> prefix tree of the words that follow (or precede) a root word
#'   wt_layout() -> x/y coordinates (in mm, so text never overlaps its siblings)
#'   wt_edges()  -> smooth connector paths between parent and child labels
#'   ggwordtree() / wt_girafe() -> plot it
#'
#' Only ggplot2, dplyr, tidyr, stringr, purrr are needed for the static plot.
#' ggiraph is only needed for the interactive version.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(ggplot2)
})

# 1. Build the tree -------------------------------------------------------

#' @param phrases character vector of sentences / free text responses
#' @param root    the word (or phrase) the tree grows from
#' @param direction "after" (default, like Google) or "before"
#' @param max_depth number of words to show beyond the root
#' @param min_n   drop branches seen fewer than this many times
wt_build <- function(phrases, root, direction = c("after", "before"),
                     max_depth = 4, min_n = 1) {
  direction <- match.arg(direction)
  root <- str_to_lower(root)
  root_words <- str_split(root, "\\s+")[[1]]
  k <- length(root_words)

  tokens <- phrases |>
    str_to_lower() |>
    str_remove_all("[^a-z0-9'\\s]") |>
    str_squish() |>
    str_split("\\s+")

  # for every occurrence of root, grab the words that follow / precede it
  paths <- map(tokens, function(w) {
    n <- length(w)
    if (n < k) return(NULL)
    starts <- which(map_lgl(seq_len(n - k + 1),
                            \(i) identical(w[i:(i + k - 1)], root_words)))
    map(starts, function(s) {
      if (direction == "after") {
        idx <- seq_len(max_depth) + s + k - 1
        out <- w[idx[idx <= n]]
      } else {
        idx <- s - seq_len(max_depth)           # walk backwards
        out <- w[idx[idx >= 1]]
      }
      out
    })
  }) |> list_flatten()

  if (length(paths) == 0) stop("Root '", root, "' not found in `phrases`.")

  # long format: one row per (path, depth) with the running prefix as an id
  long <- imap(paths, \(p, i) {
    if (length(p) == 0) return(NULL)
    tibble(path = i, depth = seq_along(p), word = p,
           id = map_chr(seq_along(p), \(d) paste(p[seq_len(d)], collapse = " ")),
           parent = map_chr(seq_along(p),
                            \(d) if (d == 1) "<root>" else paste(p[seq_len(d - 1)], collapse = " ")))
  }) |> list_rbind()

  nodes <- long |>
    count(id, parent, depth, word, name = "n") |>
    filter(n >= min_n)

  # drop orphans left behind by the min_n filter
  repeat {
    keep <- nodes |> filter(depth == 1 | parent %in% id)
    if (nrow(keep) == nrow(nodes)) break
    nodes <- keep
  }

  root_row <- tibble(id = "<root>", parent = NA_character_, depth = 0L,
                     word = root, n = length(paths))
  bind_rows(root_row, nodes) |>
    mutate(depth = as.integer(depth),
           # the phrase a reader would see when hovering
           phrase = map_chr(id, \(i) {
             if (i == "<root>") return(root)
             w <- str_split(i, " ")[[1]]
             if (direction == "after") paste(root, i) else paste(c(rev(w), root), collapse = " ")
           }),
           direction = direction)
}

# 2. Layout ---------------------------------------------------------------

#' @param size_range text size range in mm (geom_text units) for min / max count
#' @param text_scale global multiplier on text size
wt_layout <- function(nodes, size_range = c(3, 9), text_scale = 1,
                      x_gap = 14, y_pad = 1.35) {
  direction <- nodes$direction[1]

  nodes <- nodes |>
    mutate(size = scales::rescale(sqrt(n), to = size_range * text_scale,
                                  from = range(sqrt(n[depth > 0]))),
           size = if_else(depth == 0, max(size_range) * text_scale, size),
           width = str_length(word) * size * 0.52)   # ~mm, proportional font

  # x: each depth starts after the widest label of the previous depth
  col_x <- nodes |>
    group_by(depth) |> summarise(w = max(width), .groups = "drop") |>
    arrange(depth) |>
    mutate(x = lag(cumsum(w + x_gap), default = 0))
  nodes <- left_join(nodes, select(col_x, depth, x), by = "depth")

  # y: depth-first order (biggest branch first); leaves stack, parents centre
  kids <- split(nodes, nodes$parent)
  order_ids <- character()
  dfs <- function(id) {
    order_ids <<- c(order_ids, id)
    ch <- kids[[id]]
    if (!is.null(ch)) walk(ch$id[order(-ch$n, ch$word)], dfs)
  }
  dfs("<root>")
  nodes <- nodes[match(order_ids, nodes$id), ] |> mutate(ord = row_number())

  is_leaf <- !(nodes$id %in% nodes$parent)
  y <- rep(NA_real_, nrow(nodes))
  leaf_idx <- which(is_leaf)
  h <- nodes$size[leaf_idx] * y_pad
  y[leaf_idx] <- -cumsum(c(0, (head(h, -1) + h[-1]) / 2))   # stack downwards

  # fill parents bottom-up (deepest first) as the mean of child extremes
  for (d in rev(seq_len(max(nodes$depth)) - 1)) {
    for (i in which(nodes$depth == d & !is_leaf)) {
      cy <- y[nodes$parent == nodes$id[i] & !is.na(nodes$parent)]
      y[i] <- mean(range(cy))
    }
  }
  nodes$y <- y

  if (direction == "before") nodes$x <- -nodes$x - nodes$width  # mirror, text ends at the anchor
  nodes |> mutate(hjust = 0)
}

# 3. Edges ----------------------------------------------------------------

wt_edges <- function(layout, n_pts = 30) {
  dir <- layout$direction[1]
  child  <- layout |> filter(depth > 0)
  parent <- layout |> select(parent = id, px = x, py = y, pw = width)
  e <- left_join(child, parent, by = "parent")

  e <- e |>
    transmute(id, n, phrase,
              x0 = if (dir == "after") px + pw + 1.5 else px - 1.5,
              y0 = py,
              x1 = if (dir == "after") x - 1.5 else x + width + 1.5,
              y1 = y)

  e |>
    mutate(t = list(seq(0, 1, length.out = n_pts))) |>
    unnest(t) |>
    mutate(s = t * t * (3 - 2 * t),                 # smoothstep -> S-curve
           ex = x0 + (x1 - x0) * t,
           ey = y0 + (y1 - y0) * s)
}

# 4. Plot -----------------------------------------------------------------

#' Static or interactive word tree.
#' interactive = TRUE needs ggiraph; returns a girafe htmlwidget.
ggwordtree <- function(phrases, root, direction = "after", max_depth = 4,
                       min_n = 1, interactive = FALSE, text_scale = 1,
                       palette = c(root = "#1b4f72", node = "#34495e",
                                   edge = "#b0b7bd", hover = "#e67e22")) {
  nodes  <- wt_build(phrases, root, direction, max_depth, min_n)
  layout <- wt_layout(nodes, text_scale = text_scale)
  edges  <- wt_edges(layout)

  layout <- layout |>
    mutate(tooltip = sprintf("%s\n%d occurrence%s", phrase, n, if_else(n == 1, "", "s")),
           colour = if_else(depth == 0, "root", "node"))

  if (interactive) {
    if (!requireNamespace("ggiraph", quietly = TRUE))
      stop("Install ggiraph for interactive = TRUE")
    geom_edge  <- \(mapping, ...) ggiraph::geom_path_interactive(mapping = mapping, ...)
    geom_label <- \(mapping, ...) ggiraph::geom_text_interactive(mapping = mapping, ...)
  } else {
    drop_i <- \(m) m[setdiff(names(m), c("tooltip", "data_id"))]
    geom_edge  <- \(mapping, ...) geom_path(mapping = drop_i(mapping), ...)
    geom_label <- \(mapping, ...) geom_text(mapping = drop_i(mapping), ...)
  }

  p <- ggplot() +
    geom_edge(data = edges,
              mapping = aes(ex, ey, group = id, linewidth = n,
                  tooltip = phrase, data_id = id),
              colour = palette[["edge"]], lineend = "round", alpha = .8) +
    geom_label(data = layout,
               mapping = aes(x, y, label = word, size = size, colour = colour,
                   hjust = hjust, tooltip = tooltip, data_id = id),
               vjust = 0.5) +
    geom_blank(data = layout, aes(x = x + width, y = y)) +   # so the axis range includes label widths
    scale_size_identity() +
    scale_linewidth(range = c(.3, 2.5), guide = "none") +
    scale_colour_manual(values = c(root = palette[["root"]], node = palette[["node"]]),
                        guide = "none") +
    scale_x_continuous(expand = expansion(add = 8)) +
    scale_y_continuous(expand = expansion(add = 6)) +
    coord_fixed() +                                  # 1 unit = 1 mm both ways
    theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = NA))

  attr(p, "extent_mm") <- c(w = diff(range(layout$x, layout$x + layout$width)) + 16,
                            h = diff(range(layout$y)) + max(layout$size) * 2 + 12)
  p
}

#' Wrap a ggwordtree in a girafe widget with hover highlighting.
#' Hovering a word highlights it and its incoming branch; the tooltip shows
#' the full phrase and its count.
wt_girafe <- function(p, hover_colour = "#e67e22") {
  ext <- attr(p, "extent_mm") / 25.4
  ggiraph::girafe(
    ggobj = p, width_svg = ext[["w"]], height_svg = ext[["h"]],
    options = list(
      ggiraph::opts_hover(css = sprintf("fill:%s;stroke:%s;cursor:pointer;", hover_colour, hover_colour)),
      ggiraph::opts_hover_inv(css = "opacity:0.25;"),
      ggiraph::opts_tooltip(css = "background:#fff;border:1px solid #ccc;padding:4px 8px;font-size:12px;border-radius:3px;"),
      ggiraph::opts_toolbar(saveaspng = TRUE)
    ))
}
