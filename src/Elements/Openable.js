module.exports = function openable(app) {
  class Openable extends HTMLElement {
    constructor() {
      super();

      this._onActivation = this._onActivation.bind(this);
      this._onDeactivation = this._onDeactivation.bind(this);
    }
    connectedCallback() {
      this.$activator = this.querySelector('[Openable__activator]');
      this.$deactivator = this.querySelector('[Openable__deactivator]');

      const x = this.$deactivator && this.$deactivator.closest('ui-openable');
      if (x != this) {
        this.$deactivator = null;
      }
      this.$content = this.querySelector('[Openable__content]');

      this.$activator.addEventListener('click', this._onActivation);
      this.$deactivator &&
        this.$deactivator.addEventListener('click', this._onDeactivation);
    }

    _onActivation() {
      this.$activator.classList.add('hidden');
      this.$deactivator && this.$deactivator.classList.remove('hidden');
      this.$content.classList.remove('hidden');
    }
    _onDeactivation() {
      this.$activator.classList.remove('hidden');
      this.$deactivator && this.$deactivator.classList.add('hidden');
      this.$content.classList.add('hidden');
    }
  }
  return customElements.define('ui-openable', Openable);
};
