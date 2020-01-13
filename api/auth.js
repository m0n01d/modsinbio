const axios = require('axios');
const qs = require('qs');
module.exports = auth;
const { REDIRECT_URI, CLIENT_ID, CLIENT_SECRET } = process.env;
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
    console.log({ data });
    axios(config)
      .then(response => {
        console.log(response.data);
        const { access_token, user_id } = response.data;

        return getUser({ access_token, user_id });
      })
      .then(username => {
        res.send(username);
      })
      .catch(e => {
        console.error(e);
        res.send(500);
      });
  }
}

function getUser({ access_token, user_id }) {
  return axios
    .get(
      `https://graph.instagram.com/${user_id}?fields=id,username&access_token=${access_token}`
    )
    .then(({ data }) => {
      const user = data.username;
      return user;
    });
}

/*
query username
check db for username
  if found
    login
  if not found
    add
  
  
*/
