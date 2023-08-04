export class SingleInputCapture {
  private readonly handler: (event: TouchEvent) => boolean;
  private readonly baseElement: HTMLElement;

  private static readonly eventOptionConfig: AddEventListenerOptions = {
    capture: true,
    // While other
    once: true,
    passive: false,
    // While `abort` exists, it's too new to rely upon it for some of the older platforms we support.

  };

  constructor(element: HTMLElement, preventPropagation: boolean, captureCallback: () => void) {
    this.baseElement = element;

    this.handler = (event) => {
      captureCallback();
      if(preventPropagation) {
        event.stopPropagation();
        event.stopImmediatePropagation();
      }

      // Make extra-sure this is only called once; older versions of Chrome for Android don't support
      // the advanced options configuration that we prefer by default!
      this.cancel();

      return !preventPropagation;
    }

    try {
      // Not available in Android Chrome until version 49.  `capture`: 52, `once`: 55, `passive`: 51.
      this.baseElement.addEventListener('touchstart', this.handler, SingleInputCapture.eventOptionConfig);
    } catch (err) {
      this.baseElement.addEventListener('touchstart', this.handler, false);
    }
  }

  cancel() {
    this.baseElement.removeEventListener('touchstart', this.handler, SingleInputCapture.eventOptionConfig.capture);
  }
}