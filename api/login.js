const toTemplate = require("lodash.template");

const fs = require("fs");
const path = require("path");

const rawTemplate = fs.readFileSync(
  path.resolve(process.cwd(), "emails", "login-email.txt")
);

const template = toTemplate(rawTemplate);
const { User } = require("../db/schema");

const sendEmail = require("./_sendEmail");

module.exports = login;

function login(req, res) {
  const { email } = req.body;

  if (!email) {
    return res.status(404).send();
  }
  return loginOrCreate(email)
    .then(getJwt)
    .then(jwt => {
      const { host } = req.headers;
      console.log({ host });
      const action_url = `https://${host}/app/authed?token=${jwt}`;
      return sendAuthEmail({ email, actionUrl });
    })
    .then(_ => res.status(200).send())
    .catch(e => {
      console.error(e);
      res.status(500).send(e);
    });
}

// TODO error handling
function loginOrCreate(email) {
  console.log("creating", email);
  return User.query()
    .where("email", email)
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

function sendAuthEmail({ email, actionUrl }) {
  const text = template({ action_url });
  return sendEmail({
    text,
    to: email,
    subject: "Sign in link"
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
