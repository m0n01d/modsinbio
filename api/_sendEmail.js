const mailgun = require('mailgun-js');
const { mailgunConfig } = require('./_env');

const mg = mailgun(mailgunConfig);

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
