const mailgun = require("mailgun-js");

const api_key = "d1b89b4d6707182da68ac066bcbfcbd9-8b7bf2f1-7a633a40";
const DOMAIN = "sandbox8950b931a39144d1844aef02fc71ad11.mailgun.org";
// @TODO env vars here
const mg = mailgun({ apiKey: api_key, domain: DOMAIN });
module.exports = function sendEmail({
  html = null,
  text = null,
  from = `mods@modsinbio.com`,
  to,
  subject
}) {
  const data = {
    from,
    to: [to],
    subject,
    html,
    text
  };
  return new Promise((resolve, reject) => {
    mg.messages().send(data, function(error, body) {
      if (error) {
        return reject(error);
      }
      return resolve();
    });
  });
};
