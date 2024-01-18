import { DeviceSpec, ManagedPromise } from '@keymanapp/web-utils';
import { type InternalKeyboardFont as KeyboardFont } from '@keymanapp/keyboard-processor';

type FontFamilyStyleMap = {[family: string]: HTMLStyleElement};

export class StylesheetManager {
  private fontStyleDefinitions: { [os: string]: FontFamilyStyleMap} = {};
  private linkedSheets: HTMLStyleElement[] = [];
  private doCacheBusting: boolean;

  public readonly linkNode: Node;

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

  linkStylesheet(sheet: HTMLStyleElement) {
    this.linkedSheets.push(sheet);
    this.linkNode.appendChild(sheet);
  }

  /**
   * Provides a `Promise` that resolves when all currently-linked stylesheets have loaded.
   * Any change to the set of linked sheets after the initial call will be ignored.
   */
  async allLoadedPromise() {
    const promises: Promise<void>[] = [];

    for(const sheetElem of this.linkedSheets) {
      // Based on https://stackoverflow.com/a/21147238
      if(sheetElem.sheet?.cssRules) {
        promises.push(Promise.resolve());
      } else {
        const promise = new ManagedPromise<void>();
        sheetElem.addEventListener('load', () => promise.resolve());
        promises.push(promise.corePromise);
      }
    }

    await Promise.all(promises);
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
      return;
    }

    if(typeof(fd.files) == 'undefined') {
      return;
    }

    const fontKey = fd.family;

    let i, ttf='', woff='', eot='', svg='', fList=[];
    let data: string;

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
      return;
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
      if(fList[i].toLowerCase().indexOf('.eot') > 0) eot=fList[i];
      if(fList[i].toLowerCase().indexOf('.svg') > 0) svg=fList[i];
    }

    // Font path qualified to support page-relative fonts (build 347)
    if(ttf != '' && (ttf.indexOf('/') < 0))  {
      ttf = fontPathRoot+ttf;
    }

    if(woff != '' && (woff.indexOf('/') < 0)) {
      woff = fontPathRoot+woff;
    }

    if(eot != '' && (eot.indexOf('/') < 0)) {
      eot = fontPathRoot+eot;
    }

    if(svg != '' && (svg.indexOf('/') < 0)) {
      svg = fontPathRoot+svg;
    }

    // Build the font-face definition according to the browser being used
    var s='@font-face {\nfont-family:'
      + fd.family + ';\nfont-style:normal;\nfont-weight:normal;\n';

    // Build the font source string according to the browser,
    // but return without adding the style sheet if the required font type is unavailable

    // Modern browsers: use WOFF, TTF and fallback finally to SVG. Don't provide EOT
    if(data) {
      // For inline-defined fonts:
      const formatStartIndex = 'data:font/'.length;
      const format = data.substring(formatStartIndex, data.indexOf(';', formatStartIndex));
      s +=`src:url('${data}'), format('${format}');`;
    } else if(os == DeviceSpec.OperatingSystem.iOS) {
      if(ttf != '') {
        if(this.doCacheBusting) {
          ttf = this.cacheBust(ttf);
        }
        s=s+'src:url(\''+ttf+'\') format(\'truetype\');';
      } else {
        return null;
      }
    } else {
      var s0 = [];

      if(os == DeviceSpec.OperatingSystem.Android) {
        // Android 4.2 and 4.3 have bugs in their rendering for some scripts
        // with embedded ttf or woff.  svg mostly works so is a better initial
        // choice on the Android browser.
        if(svg != '') {
          s0.push("url('"+svg+"') format('svg')");
        }

        if(woff != '') {
          s0.push("url('"+woff+"') format('woff')");
        }

        if(ttf != '') {
          s0.push("url('"+ttf+"') format('truetype')");
        }
      } else {
        if(woff != '') {
          s0.push("url('"+woff+"') format('woff')");
        }

        if(ttf != '') {
          s0.push("url('"+ttf+"') format('truetype')");
        }

        if(svg != '') {
          s0.push("url('"+svg+"') format('svg')");
        }
      }

      if(s0.length == 0) {
        return null;
      }

      s += 'src:'+s0.join(',')+';';
    }

    s=s+'\n}\n';

    const sheet = createStyleSheet(s);
    fontStyleMap[fontKey] = sheet;

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
  linkExternalSheet(href: string): void {
    try {
      if(document.querySelector("link[href="+JSON.stringify(href)+"]") != null) {
        // We've already linked this stylesheet, don't do it again
        return;
      }
    } catch(e) {
      // We've built an invalid href, somehow?
      return;
    }

    const linkElement=document.createElement('link');
    linkElement.type='text/css';
    linkElement.rel='stylesheet';
    linkElement.href=href;

    this.linkStylesheet(linkElement);
  }

  public unlink(stylesheet: HTMLStyleElement) {
    const index = this.linkedSheets.indexOf(stylesheet);
    if(index > -1) {
      this.linkedSheets.splice(index, 1);
      stylesheet.parentNode.removeChild(stylesheet);
      return true;
    }

    return false;
  }

  public unlinkAll() {
    for(let sheet of this.linkedSheets) {
      if(sheet.parentNode) {
        sheet.parentNode.removeChild(sheet);
      }
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