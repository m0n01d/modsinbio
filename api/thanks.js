const s3 = require('../config/s3')();

const isProd = process.env.NODE_ENV == 'production';

module.exports = function signedUrl(req, res) {
  // const Bucket = !isProd ? 'fooimgix' : 'prod.vertol.io'; @TODO
  const Bucket = 'modsinbio-emails';
  const { key } = req.query;
  // s3.head object to check if it exists, before overwriting...
  const url = s3.putObject(
    {
      Bucket,
      Key: key,
      Body: key,
      ContentType: 'text/plain',
    },
    (err, data) => {
      if (err) {
        console.log(err);
        return res.setStatus(500).send(err);
      }
      return res.send('thanks');
    }
  );
};
