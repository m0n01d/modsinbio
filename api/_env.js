const isProd = process.env.NODE_ENV == 'production';

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
  }
};

module.exports = isProd ? productionConfig : devConfig;
