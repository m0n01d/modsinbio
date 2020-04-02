var fs = require('fs');
var fnv = require('fnv-plus');
const path = require('path');

// TODO: why does rsaPemToJwk work with a file but not with a variable?
exports.key = (
  process.env.AUTH_PRIVATE_KEY ||
  fs.readFileSync(path.resolve(__dirname, '../dev.private.pem')).toString()
).replace(/\\n/g, '\n');

exports.publicKey = (
  process.env.AUTH_PUBLIC_KEY ||
  fs.readFileSync(path.resolve(__dirname, '../dev.public.pem')).toString()
).replace(/\\n/g, '\n');

// Key Identifier – Acts as an ‘alias’ for the key
exports.kid = process.env.AUTH_KEY_ID || fnv.hash(this.publicKey, 128).hex();
