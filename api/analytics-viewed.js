const { User } = require('../db/schema');
const { raw } = require('objection');

module.exports = viewed;

function viewed(req, res) {
  const { id } = req.query;

  return User.query()
    .patch({ views: raw('views + 1') })
    .where('id', id)
    .then(
      _ => res.end(),
      _ => res.end()
      // @todo logs
    );
}
