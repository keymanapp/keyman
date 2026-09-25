import { Banner } from "./banner.js";

export class HTMLBanner extends Banner {
  readonly container: ShadowRoot | HTMLElement;
  readonly type = 'html';

  constructor(contents?: string) {
    super();

    const bannerHost = this.getDiv();

    // Ensure any HTML styling applied for the banner contents only apply to the contents,
    // and not the banner's `position: 'relative'` hosting element.
    const div = document.createElement('div');
    div.style.userSelect = 'none';
    div.style.height = '100%';
    div.style.width = '100%';
    bannerHost.appendChild(div);

    // If possible, quarantine styling and JS for the banner contents within Shadow DOM.
    this.container = (div.attachShadow) ? div.attachShadow({mode: 'closed'}) : div;
    this.container.innerHTML = contents;
  }

  get innerHTML() {
    return this.container.innerHTML;
  }

  set innerHTML(raw: string) {
    this.container.innerHTML = raw;
  }
}