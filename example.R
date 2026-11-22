source("R/wordtree.R")

phrases <- c(
  "I love data visualization with ggplot2",
  "I love tidyverse code",
  "I love data wrangling with dplyr",
  "I love data visualization in R",
  "I love making plots",
  "I love making maps with sf",
  "I hate debugging regex",
  "I hate debugging other people's code",
  "I hate missing data",
  "I love data science",
  "I love data visualization with ggplot2 and ggiraph",
  "I need data visualization help"
)

# static
p <- ggwordtree(phrases, root = "I", max_depth = 4)
ggsave("example.png", p, width = 9, height = 6, dpi = 150)

# interactive (needs ggiraph)
if (requireNamespace("ggiraph", quietly = TRUE)) {
  wt_girafe(ggwordtree(phrases, "I", interactive = TRUE))
}
