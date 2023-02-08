// Enables DOM types, but just for this one module.
///<reference lib="dom" />

import { ManagedPromise } from "@keymanapp/web-utils";

export default class DOMKeyboardSandbox {
  public readonly sandboxHost: HTMLIFrameElement;
  public sandbox(): Window {
    return this.sandboxHost.contentWindow;
  }

  private constructor() {
    const host = this.sandboxHost = document.createElement('iframe');
    host.style.display = 'none';
    host.style.width = '0px';
    host.style.height = '0px';
    host.tabIndex = -1;
    host.title = "Sandboxed keyboard loader";
    host.hidden = true;
  }

  public static buildSandbox(): Promise<DOMKeyboardSandbox> {
    // Promise:  because the iframe's document isn't instantly ready.
    const promise = new ManagedPromise<DOMKeyboardSandbox>();
    const instance = new DOMKeyboardSandbox();

    instance.sandboxHost.onload = () => promise.resolve(instance);
    instance.sandboxHost.onerror = () => promise.reject();

    // Need to insert the sandbox into the DOM before it can load or error!
    window.document.appendChild(instance.sandboxHost);

    return promise.corePromise;
  }
}