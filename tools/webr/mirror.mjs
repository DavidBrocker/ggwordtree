import http from 'node:http';
import { execFile } from 'node:child_process';
http.createServer((req, res) => {
  execFile('curl', ['-s', '-f', '-m', '120', 'https://repo.r-wasm.org' + req.url], { encoding: 'buffer', maxBuffer: 1 << 29 }, (err, out) => {
    if (err) { res.writeHead(404); res.end(); return; }
    res.writeHead(200, { 'Access-Control-Allow-Origin': '*' }); res.end(out);
  });
}).listen(8765, '127.0.0.1', () => console.log('mirror up'));
