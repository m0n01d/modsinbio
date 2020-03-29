const databaseName = "postgres";
const pg = require("pg");

const {
  NOW_GITHUB_COMMIT_REF,
  DATABASE_URL,
  PROD_DATABASE_URL,
  APP_ENV
} = process.env;

const connection_url =
  APP_ENV == "development"
    ? DATABASE_URL // LOCALHOST
    : NOW_GITHUB_COMMIT_REF == "master"
    ? PROD_DATABASE_URL // DIGITAL_OCEAN -- @TODO
    : `${DATABASE_URL}?ssl=true`; // HEROKU

module.exports = {
  client: "pg",
  connection: connection_url,
  migrations: {
    directory: __dirname + "/db/migrations"
  }
};
