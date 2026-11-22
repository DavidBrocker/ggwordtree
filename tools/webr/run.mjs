import { WebR } from 'webr';
import fs from 'node:fs'; import path from 'node:path';
const webR = new WebR({ interactive: false });
await webR.init();
await webR.installPackages(['ggplot2','ggiraph','dplyr','tidyr','stringr','purrr','htmlwidgets'], { quiet: true, repos: ['http://127.0.0.1:8765/'] });
const root = '/home/user/ggwordtree';
await webR.FS.mkdir('/work'); await webR.FS.mkdir('/work/R'); await webR.FS.mkdir('/work/out');
await webR.FS.writeFile('/work/R/wordtree.R', fs.readFileSync(root + '/R/wordtree.R'));
await webR.FS.writeFile('/work/example.R', fs.readFileSync(root + '/example.R'));
const r = await webR.evalR(`
  res <- tryCatch({
    setwd('/work'); source('/work/example.R', echo = FALSE)
    w <- wt_girafe(ggwordtree(phrases, "I", interactive = TRUE))
    htmlwidgets::saveWidget(w, '/work/out/index.html', selfcontained = FALSE)
    c("ok", class(w)[1], list.files('/work/out', recursive = TRUE))
  }, error = function(e) c("ERROR", conditionMessage(e)))
  res`).catch(e => { console.log('evalR failed', e); return null; });
if (r) console.log((await r.toJs()).values);
// copy out
function walk(d) { const o = []; for (const f of webR.FS ? [] : []) ; return o; }
const list = (await (await webR.evalR(`list.files('/work/out', recursive=TRUE)`)).toJs()).values;
for (const f of list) {
  const data = await webR.FS.readFile('/work/out/' + f);
  const dst = '/tmp/claude-0/webr/out/' + f; fs.mkdirSync(path.dirname(dst), { recursive: true }); fs.writeFileSync(dst, data);
}
webR.close();
