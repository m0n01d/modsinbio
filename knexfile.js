const databaseName = 'postgres';
const pg = require('pg');

const {
  NOW_GITHUB_COMMIT_REF,
  DATABASE_URL,
  PROD_DATABASE_URL,
  APP_ENV,
} = process.env;

const connection_url =
  APP_ENV == 'development'
    ? DATABASE_URL // LOCALHOST
    : NOW_GITHUB_COMMIT_REF == 'master'
    ? PROD_DATABASE_URL // DIGITAL_OCEAN
    : `${process.env.DATABASE_URL}?ssl=true`; // HEROKU

// @TODO set up envs in heroku and zeit and .env

module.exports = {
  client: 'pg',
  connection: connection_url,
  migrations: {
    directory: __dirname + '/db/migrations',
  },
};
