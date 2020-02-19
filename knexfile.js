const databaseName = 'postgres';
const pg = require('pg');

const connection_url =
  process.env.APP_ENV == 'development'
    ? process.env.DATABASE_URL
    : `${process.env.DATABASE_URL}?ssl=true` ||
      `postgres://postgres:@postgres:5432/${databaseName}`;

module.exports = {
  client: 'pg',
  connection: connection_url,
  migrations: {
    directory: __dirname + '/db/migrations',
  },
};
