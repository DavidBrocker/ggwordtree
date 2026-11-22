const { chromium } = require('playwright');
(async () => {
  const b = await chromium.launch(); const p = await b.newPage({ viewport: { width: 1200, height: 800 } });
  const errs = []; p.on('pageerror', e => errs.push(String(e))); p.on('console', m => m.type()==='error' && errs.push(m.text()));
  await p.goto('file:///tmp/claude-0/webr/out/index.html'); await p.waitForTimeout(1500);
  const info = await p.evaluate(() => ({ svg: !!document.querySelector('svg'), ids: document.querySelectorAll('[data-id]').length,
    ex: [...document.querySelectorAll('text[data-id]')].slice(0,3).map(e => e.getAttribute('data-id')) }));
  console.log(JSON.stringify(info), 'errors:', errs.slice(0,3));
  const t = p.locator('text[data-id="love data"]').first();
  await t.hover(); await p.waitForTimeout(500);
  console.log('tooltip:', await p.evaluate(() => (document.querySelector('.tooltip-ggiraph, [class*=tooltip]')||{}).innerText));
  await p.screenshot({ path: '/tmp/claude-0/webr/hover.png' });
  await b.close();
})();
