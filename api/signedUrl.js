const { S3 } = require('aws-sdk');

const {
  s3Config,
  signedUrl: { Bucket }
} = require('./_env');

const s3 = new S3(s3Config);

module.exports = function signedUrl(req, res) {
  const { key, type } = req.query;
  // s3.head object to check if it exists, before overwriting...
  const url = s3.getSignedUrl('putObject', {
    Bucket,
    Key: key,
    ContentType: type
  });

  res.send(url);
};
