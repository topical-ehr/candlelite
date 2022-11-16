// Based on https://mdn.github.io/web-components-examples/popup-info-box-web-component/

// Using a custom element's shadow DOM to prevent
// Bulma's CSS from interfering... (e.g. the number class)

class PrismHighlightElement extends HTMLElement {
    constructor() {
        super();

        // Create a shadow root
        const shadow = this.attachShadow({ mode: 'open' });

        // Create elts
        const pre = document.createElement('pre');
        const code = document.createElement('code');
        pre.appendChild(code);
        shadow.appendChild(pre);

        // Add CSS
        const linkElem = document.createElement("link");
        linkElem.setAttribute("rel", "stylesheet");
        linkElem.setAttribute("href", "/prism/prism.css");
        shadow.appendChild(linkElem);

        this.update();

    }

    update() {
        const code = this.getAttribute('code');
        const lang = this.getAttribute('lang');

        if (!code || !lang) {
            return;
        }

        const codeElt = this.shadowRoot.querySelector("code");
        codeElt.setAttribute("class", "language-" + lang);
        codeElt.textContent = code;

        Prism.highlightElement(codeElt);
    }

    attributeChangedCallback(name, oldValue, newValue) {
        this.update();
    }
    static get observedAttributes() { return ['code', 'lang']; }

}

// Define the new element
customElements.define('prism-highlight', PrismHighlightElement);
