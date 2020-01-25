module.exports = function linkClick(app) {
  class LinkClick extends HTMLElement {
    constructor() {
      super();

      this._onClick = this._onClick.bind(this);
    }
    connectedCallback() {
      this.addEventListener('click', this._onClick);
    }

    _onClick() {
      this.dispatchEvent(new CustomEvent('LinkClicked'));
    }
  }
  return customElements.define('ui-link-click', LinkClick);
};
