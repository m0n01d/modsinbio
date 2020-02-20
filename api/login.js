const { User } = require('../db/schema');

const sendEmail = require('./_sendEmail');

module.exports = login;

function login(req, res) {
  const { email } = req.body;

  if (!email) {
    return res.setStatus(404).res.end();
  }
  return loginOrCreate(email)
    .then(getJwt)
    .then(jwt => {
      sendAuthEmail({ email, jwt });
    })
    .then(_ => res.sendStatus(200))
    .catch(e => res.send(e));
}

// TODO error handling
function loginOrCreate(email) {
  return User.query()
    .where('email', email)
    .first()
    .then(user => {
      if (!user) {
        return createUser(email);
      }
      return user;
    });
}

function getJwt(user) {
  return Promise.resolve(user.getJwt());
}

function createUser(email) {
  return User.query().insert({ email });
}

function sendAuthEmail({ email, jwt }) {
  return sendEmail({
    html: `<p>${jwt}</p>`,
    to: email,
    subject: 'Login dummy',
  });
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
