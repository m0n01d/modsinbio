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
      const { tag, payload } = JSON.parse(body);

      const { code, responseText } = fakeResponse(tag, payload);

      xhr.status = code;
      xhr.statusText = `code:${code}`;
      xhr.response = xhr.responseText = responseText;
      xhr.dispatchEvent(new Event('load'));
    };
  };
  return xhr;
};

Elm.Main.init({
  node: document.getElementById('root'),
});

function fakeResponse(tag, payload) {
  switch (tag) {
    case 'AddLink': {
      const { title, description, url } = payload;
      const newLink = {
        id: `${Date.now()}`,
        title,
        description,
        urlString: url,
      };
      const json = JSON.stringify(newLink);
      return { code: 200, responseText: json };
    }
    case 'SaveNewCategoryTitle': {
      return { code: 200, responseText: payload };
    }

    case 'InitializeMyMods': {
      const res = db();
      return { code: 200, responseText: JSON.stringify(res) };
    }
    default:
      return { code: 500, responseText: null };
  }
}

function db() {
  const store = localStorage.myMods
    ? JSON.parse(localStorage.myMods)
    : {
        categories: [
          {
            mods: [
              {
                id: '1578238594346',
                urlString:
                  'https://www.flyinmiata.com/custom-turbo-system-na8-chassis.html',
                title: 'Flying miata turbo',
                description: 'go fast parts',
              },
            ],
            order: 1,
            title: 'Engine',
            id: 0,
          },
        ],
      };

  return store;
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
// serviceWorker.unregister();
