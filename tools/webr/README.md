# Testing ggwordtree headlessly with webR

Used to verify the ggiraph widget without a local R install (`npm i webr playwright` first).

1. `node mirror.mjs` - local mirror of repo.r-wasm.org (needed when webR's own downloads can't get through a proxy).
2. `node run.mjs` - installs ggplot2/ggiraph/tidyverse pieces in webR, runs `example.R`, writes the widget to `out/`.
3. `node shot.cjs` - opens `out/index.html` in Chromium, hovers a node, prints the tooltip and screenshots it.

Paths in these scripts are hard-coded for the sandbox they were written in; adjust before reuse.
