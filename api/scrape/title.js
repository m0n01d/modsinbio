const cheerio = require('cheerio');
const axios = require('axios');

function scrape(req, res) {
  const url = req.query.url;
  if (!url || url.length < 1) return res.status(400).send('Url missing');

  axios
    .get(url)
    .then(result => {
      const html = result.data;
      const $ = cheerio.load(html);
      const title = $('title')
        .text()
        .replace(/\n/g, ' ')
        .replace(/\s{3,}/g, ' ')
        .trim();

      res.send(title);
    })
    .catch(e => res.status(400).send(e));
}

module.exports = scrape;
