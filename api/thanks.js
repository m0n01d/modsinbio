const { S3 } = require('aws-sdk');

const {
  s3Config,
  thanks: { Bucket }
} = require('./_env');

const s3 = new S3(s3Config);

module.exports = _ => s3;

module.exports = function signedUrl(req, res) {
  const { key } = req.query;
  // s3.head object to check if it exists, before overwriting...
  s3.putObject(
    {
      Bucket,
      Key: key,
      Body: key,
      ContentType: 'text/plain'
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
