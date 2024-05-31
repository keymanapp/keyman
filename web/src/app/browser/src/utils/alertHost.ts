/**
 * Encapsulates the emulated dialog used by KMW to signal alerts and keyboard-load
 * waiting messages to the user.
 *
 * The intent is for this to be a direct child element of `document.body`.
 */
export class AlertHost {
  /**
   * The "background" element - a full-page shim _behind_ the alert / wait box that assists
   * in emulating a modal state.
   *
   * This element uses `display: none` by default, and as the root element, this keeps the others
   * hidden by default as well.
   */
  private readonly bg: HTMLDivElement;

  /**
   * The "wait text" / "alert text" element - the element containing any message text associated
   * with the alert or wait being signaled.
   */
  private readonly lt: HTMLDivElement;

  /**
   * The "wait graphic" - the spinny-bar graphic used during keyboard-load waiting messages.
   */
  private readonly gr: HTMLDivElement;

  /**
   * The top-right 'close'/'x' button.
   */
  private readonly bx: HTMLDivElement;

  private dismiss?: () => void;
  private pending: boolean = false;

  // Formerly known as `this.waiting` when within `keyman.util`.
  public get rootElement(): HTMLDivElement {
    return this.bg;
  }

  /**
   *  Prepares, but does not display, the background and keyboard loading wait message box
   *  Should not be called before options are defined during initialization
   **/
  constructor() {
    // "background" - the clickable shim BEHIND the 'dialog'
    const bg = this.bg = document.createElement('div'),
          lb = document.createElement('div'), // 'wait box' - the main actual message box / 'dialog' element
          lt = this.lt = document.createElement('div'), // 'wait text' // or alert text - but the host for message text
          gr = this.gr = document.createElement('div'), // 'wait graphic' - the little 'please wait' spinny / faux progress bar
          bx = this.bx = document.createElement('div'); // the little 'close' top-right x button.

    bg.className='kmw-wait-background';
    lb.className='kmw-wait-box';
    this.dismiss=null;
    lt.className='kmw-wait-text';
    gr.className='kmw-wait-graphic';
    bx.className='kmw-alert-close';

    // Close alert if anywhere in box is touched, since close box is too small on mobiles
    lb.onmousedown = lb.onclick = (e) => {
      // Ignore if waiting, only handle for alert
      if(bx.style.display == 'block') {
        bg.style.display='none';
        if(this.dismiss) {
          this.dismiss();
        }
      }
    };

    lb.addEventListener('touchstart', lb.onclick, false);
    bg.onmousedown = bg.onclick = (e) => {
      e.preventDefault();
      e.stopPropagation();

    }
    bg.addEventListener('touchstart', bg.onclick, false);
    lb.appendChild(bx); // [0]
    lb.appendChild(lt); // [1]
    lb.appendChild(gr); // [2]
    bg.appendChild(lb);

    document.body.appendChild(bg);
  }

  /**
   * Customized wait display
   *
   * @param   {string|boolean}   s       displayed text
   */
  wait(s?: string) {
    // Keyboards loaded with page are initialized before the page is ready,
    // so cannot use the wait indicator (and don't need it, anyway)
    const bg = this.bg;
    if(typeof(bg) == 'undefined' || bg == null) {
      return;
    }

    if(s) {
      this.pending=true;
      // In practice, is typically:  "Installing keyboard", with line 2 = keyboard name.
      window.setTimeout(() => {
        if(this.pending) {
          window.scrollTo(0,0);
          this.bx.style.display='none';
          this.lt.className='kmw-wait-text';
          this.lt.innerHTML=s;
          this.gr.style.display='block';
          bg.style.display='block';
        }
      }, 1000);
    } else {
      if(this.pending) {
        this.lt.innerHTML='';
        this.pending=false;
        bg.style.display='none';
      }
    }
  }

  /**
   * Customized alert.
   *
   * @param     {string}        s       alert text
   * @param     {function()=}   fn      function to call when alert dismissed
   */
  alert(s: string, fn?: () => void): void {
    const bg = this.bg;
    this.bx.style.display='block';
    this.lt.className='kmw-alert-text';
    this.lt.innerHTML=s;
    this.gr.style.display='none';
    bg.style.display='block';
    this.dismiss = arguments.length > 1 ? fn : null;
  }

  shutdown() {
    this.bg.parentNode.removeChild(this.bg);
  }
}