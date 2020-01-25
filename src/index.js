import './main.css';
import { Elm } from './Main.elm';

require('./Elements/Openable')();
require('./Elements/LinkClick')();
// import * as serviceWorker from './serviceWorker';

const flags = {
  payload: localStorage.user ? JSON.parse(localStorage.user) : null,
};

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags,
});

app.ports.toJs.subscribe(function fromElm({ tag, payload }) {
  switch (tag) {
    case 'SAVE_USER': {
      localStorage.user = JSON.stringify(payload);
      return;
    }
    default:
      return;
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
// serviceWorker.unregister();
