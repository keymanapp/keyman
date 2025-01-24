import { DeviceSpec, ManagedPromise } from '@keymanapp/web-utils';
import { type InternalKeyboardFont as KeyboardFont } from 'keyman/engine/keyboard';

type FontFamilyStyleMap = {[family: string]: HTMLStyleElement};

export class StylesheetManager {
  private fontStyleDefinitions: { [os: string]: FontFamilyStyleMap} = {};
  private linkedSheets: {
    sheet: HTMLStyleElement,
    load: ManagedPromise<void>
  }[] = [];
  private fontPromises: Promise<FontFace | void>[] = [];
  private doCacheBusting: boolean;

  public readonly linkNode: Node;

  public get sheets(): readonly HTMLStyleElement[] {
    return this.linkedSheets.map((entry) => entry.sheet);
  }

  public constructor(linkNode?: Node, doCacheBusting?: boolean) {
    if(!linkNode) {
      let _ElemHead=document.getElementsByTagName('HEAD');
      if(_ElemHead.length > 0) {
        linkNode = _ElemHead[0];
      } else {
        linkNode = document.body; // Won't work on [old?] Chrome, ah well
      }
    }
    this.linkNode = linkNode;
    this.doCacheBusting = doCacheBusting || false;
  }

  linkStylesheet(sheet: HTMLStyleElement | HTMLLinkElement) {
    if(!(sheet instanceof HTMLLinkElement) && !sheet.innerHTML) {
      return;
    }

    const promise = new ManagedPromise();
    if(sheet instanceof HTMLLinkElement) {
      sheet.onload = () => promise.resolve();
    } else {
      // If it's an inline sheet, it's essentially already loaded.
      // The microtask delay this induces (for type compat) also
      // gives the browser time to apply the inlined-style.
      promise.resolve();
    }

    this.linkedSheets.push({
      sheet: sheet,
      load: promise
    });
    this.linkNode.appendChild(sheet);
  }

  /**
   * Provides a `Promise` that resolves when all currently-linked stylesheets have loaded.
   * Any change to the set of linked sheets after the initial call will be ignored.
   */
  async allLoadedPromise(): Promise<void> {
    const allPromises = this.linkedSheets.map((entry) => entry.load.corePromise);
    if(Promise.allSettled) {
      // allSettled - Chrome 76 / Safari 13
      // Delays for settling (either then OR catch) for ALL promises.
      await Promise.allSettled(allPromises)
    } else {
      // all - Chrome 32
      // If an error happens, .all instantly resolves regardless of state of
      // other Promises.
      await Promise.all(allPromises);
    }
  }

  /**
   * Build a stylesheet with a font-face CSS descriptor for the embedded font appropriate
   * for the browser being used
   *
   * @param    {Object}  fd            keymanweb font descriptor (internal format; should be preprocessed)
   * @param    {string}  fontPathRoot  Should correspond to `this.keyman.options['fonts']`
   **/
  addStyleSheetForFont(fd: KeyboardFont, fontPathRoot: string, os?: DeviceSpec.OperatingSystem) {
    // Test if a valid font descriptor
    if(!fd) {
      return null;
    }

    if(typeof(fd.files) == 'undefined') {
      return null;
    }

    const fontKey = fd.family;
    let source: string;

    let i, ttf='', woff='', fList=[], data='';

    // TODO: 22 Aug 2014: check that font path passed from cloud is actually used!

    if(!os) {
      os = DeviceSpec.OperatingSystem.Other; // as a fallback option.
    }

    // Do not add a new font-face style sheet if already added for this font
    const fontStyleMap = this.fontStyleDefinitions[os] = this.fontStyleDefinitions[os] || {};

    if(fontStyleMap[fontKey]) {
      const sheet = fontStyleMap[fontKey];

      if(!sheet.parentNode) {
        this.linkStylesheet(sheet);
      }
      return null;
    }

    if(typeof(fd.files) == 'string') {
      fList[0]=fd.files;
    } else {
      fList=fd.files;
    }

    for(i=0;i<fList.length;i++) {
      if(fList[i].toLowerCase().indexOf('data:font') == 0) {
        data = fList[i];
      }
      if(fList[i].toLowerCase().indexOf('.otf') > 0) ttf=fList[i];
      if(fList[i].toLowerCase().indexOf('.ttf') > 0) ttf=fList[i];
      if(fList[i].toLowerCase().indexOf('.woff') > 0) woff=fList[i];
    }

    // Font path qualified to support page-relative fonts (build 347)
    if(ttf != '' && (ttf.indexOf('/') < 0))  {
      ttf = fontPathRoot+ttf;
    }

    if(woff != '' && (woff.indexOf('/') < 0)) {
      woff = fontPathRoot+woff;
    }

    // Build the font-face definition according to the browser being used
    // The OSK will similarly remove double-quotes from font-family names. (#13018, #13022)
    var s='@font-face {\nfont-family:"'
      + fd.family.replace(/\u0022/g, '') + '";\nfont-style:normal;\nfont-weight:normal;\n';

    // Build the font source string according to the browser,
    // but return without adding the style sheet if the required font type is unavailable

    // Modern browsers: use WOFF, TTF and fallback finally to SVG. Don't provide EOT
    if(data) {
      // For inline-defined fonts:
      const formatStartIndex = 'data:font/'.length;
      const format = data.substring(formatStartIndex, data.indexOf(';', formatStartIndex));
      s +=`src:url('${data}'), format('${format}');`;
    } else {
      // Note:  encodeURI("'") == "'", but encodeURI('"') == "%22".
      if(os == DeviceSpec.OperatingSystem.iOS) {
        if(ttf != '') {
          if(this.doCacheBusting) {
            ttf = this.cacheBust(ttf);
          }
          source = "url(\""+encodeURI(ttf)+"\") format('truetype')";
        }
      } else {
        if(woff != '') {
          source = "url(\""+encodeURI(woff)+"\"') format('woff')";
        }

        if(ttf != '') {
          source = "url(\""+encodeURI(ttf)+"\") format('truetype')";
        }
      }

      if(!source) {
        return null;
      }
  
      s += 'src:'+source+';';
    }

    s=s+'\n}\n';

    const sheet = createStyleSheet(s);
    fontStyleMap[fontKey] = sheet;

    /* https://developer.mozilla.org/en-US/docs/Web/API/CSS_Font_Loading_API
     * Compat:  Chrome 35... _just_ on the unupdated-Android 5.0 threshold.
     *
     * Note:  this could probably wholesale-replace the stylesheet!
     * Would need: `document.fonts.add(fontFace)` - does not have to wait for the load() Promise.
     *
     * For now, we're using this solely to detect when the font has been succesfully loaded.
     */
    const fontFace = new FontFace(fd.family, source);

    let loadPromise = fontFace.load();
    const clearPromise = () => {
      this.fontPromises = this.fontPromises.filter((entry) => entry != loadPromise);
    }
    this.fontPromises.push(loadPromise.then(clearPromise).catch(clearPromise));

    this.linkStylesheet(sheet);

    return sheet;
  }

  private cacheBust(uri: string) {
    // Our WebView version directly sets the keyboard path, and it may replace the file
    // after KMW has loaded.  We need cache-busting to prevent the new version from
    // being ignored.
    return uri + "?v=" + (new Date()).getTime(); /*cache buster*/
  }

  /**
   * Add a reference to an external stylesheet file
   *
   * @param   {string}  href   path to stylesheet file
   */
  linkExternalSheet(href: string, force?: boolean): HTMLStyleElement {
    try {
      if(!force && document.querySelector("link[href="+JSON.stringify(href)+"]") != null) {
        // We've already linked this stylesheet, don't do it again
        return null;
      }
    } catch(e) {
      // We've built an invalid href, somehow?
      return null;
    }

    const linkElement=document.createElement('link');
    linkElement.type='text/css';
    linkElement.rel='stylesheet';
    linkElement.href=href;

    this.linkStylesheet(linkElement);
    return linkElement;
  }

  public unlink(stylesheet: HTMLStyleElement) {
    const index = this.linkedSheets.findIndex((entry) => entry.sheet == stylesheet);
    if(index > -1) {
      const tuple = this.linkedSheets.splice(index, 1);
      // Ensure we don't leave `await`s that were waiting on the stylesheet hanging.
      tuple[0].load.resolve();
      stylesheet.parentNode.removeChild(stylesheet);
      return true;
    }

    return false;
  }

  public unlinkAll() {
    for(let tuple of this.linkedSheets) {
      const sheet = tuple.sheet;
      if(sheet.parentNode) {
        sheet.parentNode.removeChild(sheet);
      }
      // Clear out any lingering `await`s.
      tuple.load.resolve();
    }

    this.linkedSheets.splice(0, this.linkedSheets.length);
  }
}

/**
 * Add a stylesheet to a page programmatically, for use by the OSK, the UI or the page creator
 *
 * @param       {string}        s             style string
 * @return      {Object}                      returns the object reference
 **/
export function createStyleSheet(styleString: string): HTMLStyleElement {
  var _ElemStyle: HTMLStyleElement = <HTMLStyleElement>document.createElement<'style'>('style');

  _ElemStyle.type = 'text/css';
  _ElemStyle.appendChild(document.createTextNode(styleString));

  return _ElemStyle;
}