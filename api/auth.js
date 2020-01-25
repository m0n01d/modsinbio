const axios = require('axios');
const qs = require('qs');
const { User } = require('../db/schema');
const { REDIRECT_URI, CLIENT_ID, CLIENT_SECRET } = process.env;

module.exports = auth;

function auth(req, res) {
  const { code } = req.query;

  if (code) {
    const client_id = Number(CLIENT_ID);
    const client_secret = CLIENT_SECRET;
    const grant_type = 'authorization_code';
    const redirect_uri = REDIRECT_URI;
    const data = {
      client_id,
      client_secret,
      grant_type,
      redirect_uri,
      code,
    };
    const config = {
      url: 'https://api.instagram.com/oauth/access_token',
      data: qs.stringify(data),
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      method: 'POST',
    };
    axios(config)
      .then(response => {
        const { access_token, user_id } = response.data;

        return getUser({ access_token, user_id });
      })
      .then(loginUser)
      .then(user => {
        const tokenized = user.getJwt();
        const url = 'http://localhost:3000';
        return redirect(res, `${url}/app/authed?token=${tokenized}`);

        // return res.status(200).json(user.getUser());
      })
      .catch(e => {
        res.send(500);
      });
  }
}
function redirect(res, url) {
  res.writeHead(301, {
    Location: url,
  });
  return res.end();
}
function getUser({ access_token, user_id }) {
  return axios
    .get(
      `https://graph.instagram.com/${user_id}?fields=id,username&access_token=${access_token}`
    )
    .then(({ data }) => {
      const { username, id: instagram_id } = data;
      return { username, instagram_id };
    });
}

function loginUser(instaUser) {
  return new Promise((resolve, reject) => {
    return queryUser(instaUser, (_, user) => {
      if (!user) {
        return createUser(instaUser, resolve);
      }
      return resolve(user);
    });
  });
}

// TODO error handling
function queryUser({ username }, done) {
  return User.query()
    .where('username', username)
    .first()
    .then(function(user) {
      if (!user) {
        return done('Unknown user');
      }
      // if (!user.active) {
      //   return done('User is inactive');
      // }
      return done(null, user);
    });
}

function createUser({ username, instagram_id }, done) {
  return User.query()
    .insert({ username, instagram_id, bio: '' })
    .then(done);
}
/*


query username
check db for username
  if found
    login
  if not found
    add
  
  return userid and role
  
*/
