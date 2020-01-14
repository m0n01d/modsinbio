const rasha = require('rasha');
const jwtConfig = require('../config/jwt');
const { User } = require('../db/schema');

module.exports = getJwks;

function getJwks(req, res) {
  const jwk = {
    ...rasha.importSync({ pem: jwtConfig.publicKey }),
    alg: 'RS256',
    use: 'sig',
    kid: jwtConfig.publicKey,
  };
  const jwks = {
    keys: [jwk],
  };
  res.setHeader('Content-Type', 'application/json');
  res.send(JSON.stringify(jwks, null, 2) + '\n');

  res.status(200).json(jwks);
}
