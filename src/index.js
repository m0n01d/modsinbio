import './main.css';
import { Elm } from './Main.elm';
// import * as serviceWorker from './serviceWorker';

const OriginalXMLHttpRequest = window.XMLHttpRequest;
window.XMLHttpRequest = function() {
  const xhr = new OriginalXMLHttpRequest();
  const originalOpen = xhr.open;
  xhr.open = function(method, url, async) {
    if (!url.startsWith('/api')) {
      return originalOpen.apply(xhr, arguments);
    }

    [
      'status',
      'statusText',
      'responseText',
      'response',
      'setRequestHeader',
    ].forEach(prop => Object.defineProperty(xhr, prop, { writable: true }));

    //who cares about request headers here? :)
    xhr.setRequestHeader = () => {};

    xhr.send = body => {
      console.log({ body });
      const { tag, payload } = JSON.parse(body);
      console.log(tag, payload);

      const { code, responseText, json } = fakeResponse(tag, payload);

      xhr.status = code;
      xhr.statusText = `code:${code}`;
      console.log(json);
      xhr.response = xhr.responseText = responseText; // json ? JSON.parse(json) : responseText;
      xhr.dispatchEvent(new Event('load'));
    };
  };
  return xhr;
};

Elm.Main.init({
  node: document.getElementById('root'),
});

function fakeResponse(tag, payload) {
  console.log({ tag });
  switch (tag) {
    case 'AddLink': {
      const { title, description, url } = payload;
      const newLink = {
        id: `${Date.now()}`,
        title,
        description,
        urlString: url,
      };
      console.log({ newLink });
      const json = JSON.stringify(newLink);
      return { code: 200, responseText: json, json };
    }
    case 'SaveNewCategoryTitle': {
      return { code: 200, responseText: payload, json: null };
    }

    default:
      return { code: 500, responseText: null, json: null };
  }
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
// serviceWorker.unregister();
