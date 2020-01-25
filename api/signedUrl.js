const s3 = require('../config/s3')();

const isProd = process.env.NODE_ENV == 'production';

module.exports = function signedUrl(req, res) {
  // const Bucket = !isProd ? 'fooimgix' : 'prod.vertol.io'; @TODO
  const Bucket = 'dev-mods-in-bio';
  const { key, type } = req.query;
  // s3.head object to check if it exists, before overwriting...
  const url = s3.getSignedUrl('putObject', {
    Bucket,
    Key: key,
    ContentType: type,
  });

  res.send(url);
};
