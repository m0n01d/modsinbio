const isProd = process.env.NOW_GITHUB_COMMIT_REF == 'master';
// can't go by `process.env.NODE_ENV`

const productionConfig = {
  thanks: {
    Bucket: 'modsinbio-emails'
  },
  s3Config: {
    accessKeyId: process.env.PROD_S3_ACCESS_KEY,
    secretAccessKey: process.env.PROD_S3_SECRET_KEY,
    region: 'us-east-1'
  }
};

const devConfig = {
  thanks: {
    Bucket: 'modsinbio-emails'
  },
  signedUrl: {
    Bucket: 'dev-mods-in-bio'
  },
  s3Config: {
    accessKeyId: process.env.S3_ACCESS_KEY,
    secretAccessKey: process.env.S3_SECRET_KEY,
    region: 'us-east-1'
  },
  mailgunConfig: {
    apiKey: process.env.MAILGUN_API_KEY,
    domain: process.env.MAILGUN_DOMAIN
  }
};

module.exports = isProd ? productionConfig : devConfig;
