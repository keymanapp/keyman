namespace com.keyman.osk.embedded {
  export class SingleInputCapture {
    private readonly handler: (event: TouchEvent) => boolean;

    private static readonly eventOptionConfig: AddEventListenerOptions = {
      capture: true,
      // While other
      once: true,
      passive: false,
      // While `abort` exists, it's too new to rely upon it for some of the older platforms we support.

    };

    constructor(captureCallback: () => void) {
      this.handler = (event) => {
        captureCallback();
        event.stopPropagation();
        event.stopImmediatePropagation();

        // Make extra-sure this is only called once; older versions of Chrome for Android don't support
        // the advanced options configuration that we prefer by default!
        this.cancel();

        return true;
      }

      try {
        // Not available in Android Chrome until version 49.  `capture`: 52, `once`: 55, `passive`: 51.
        document.body.addEventListener('touchstart', this.handler, SingleInputCapture.eventOptionConfig);
      } catch (err) {
        document.body.addEventListener('touchstart', this.handler, true);
      }
    }

    cancel() {
      document.body.removeEventListener('touchstart', this.handler, SingleInputCapture.eventOptionConfig.capture);
    }
  }
}