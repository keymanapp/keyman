/// <reference path="oskKey.ts" />

namespace com.keyman.osk {
  let Codes = com.keyman.text.Codes;

  export class OSKBaseKey extends OSKKey {
    private capLabel: HTMLDivElement;

    constructor(spec: OSKKeySpec, layer: string) {
      super(spec, layer);
    }

    getId(): string {
      // Define each key element id by layer id and key id (duplicate possible for SHIFT - does it matter?)
      return this.spec.elementID;
    }

    getCoreId(): string {
      return this.spec.coreID;
    }

    // Produces a small reference label for the corresponding physical key on a US keyboard.
    private generateKeyCapLabel(): HTMLDivElement {
      // Create the default key cap labels (letter keys, etc.)
      var x = Codes.keyCodes[this.spec.baseKeyID];
      switch(x) {
        // Converts the keyman key id code for common symbol keys into its representative ASCII code.
        // K_COLON -> K_BKQUOTE
        case 186: x=59; break;
        case 187: x=61; break;
        case 188: x=44; break;
        case 189: x=45; break;
        case 190: x=46; break;
        case 191: x=47; break;
        case 192: x=96; break;
        // K_LBRKT -> K_QUOTE
        case 219: x=91; break;
        case 220: x=92; break;
        case 221: x=93; break;
        case 222: x=39; break;
        default:
          // No other symbol character represents a base key on the standard QWERTY English layout.
          if(x < 48 || x > 90) {
            x=0;
          }
      }

      if(x > 0) {
        let q = document.createElement('div');
        q.className='kmw-key-label';
        q.innerHTML=String.fromCharCode(x);
        return q;
      } else {
        // Keyman-only virtual keys have no corresponding physical key.
        return null;
      }
    }

    private processSubkeys(btn: KeyElement, vkbd: VisualKeyboard) {
      // Add reference to subkey array if defined
      var bsn: number, bsk=btn['subKeys'] = this.spec['sk'];
      // Transform any special keys into their PUA representations.
      for(bsn=0; bsn<bsk.length; bsn++) {
        if(bsk[bsn]['sp'] == '1' || bsk[bsn]['sp'] == '2') {
          var oldText=bsk[bsn]['text'];
          bsk[bsn]['text']=this.renameSpecialKey(oldText, vkbd);
        }

        // If a subkey doesn't have a defined layer property, copy it from the base key's layer by default.
        if(!bsk[bsn].layer) {
          bsk[bsn].layer = btn.key.layer
        }
      }

      // If a subkey array is defined, add an icon
      var skIcon = document.createElement('div');
      skIcon.className='kmw-key-popup-icon';
      //kDiv.appendChild(skIcon);
      btn.appendChild(skIcon);
    }

    construct(vkbd: VisualKeyboard, displayUnderlying: boolean, rowStyle: CSSStyleDeclaration, totalPercent: number): {element: HTMLDivElement, percent: number} {
      let spec = this.spec;
      let isDesktop = vkbd.device.formFactor == "desktop"

      let kDiv = document.createElement('div');
      kDiv.className='kmw-key-square';

      let ks=kDiv.style;
      ks.width=this.objectGeometry(vkbd, spec['widthpc']);

      let originalPercent = totalPercent;

      let btnEle = document.createElement('div');
      let btn = this.btn = link(btnEle, new KeyData(this, spec['id']));

      // Set button class
      this.setButtonClass();

      // Set key and button positioning properties.
      if(!isDesktop) {
        // Regularize interkey spacing by rounding key width and padding (Build 390)
        ks.left=this.objectGeometry(vkbd, totalPercent+spec['padpc']);
        if(!vkbd.isStatic) {
          ks.bottom=rowStyle.bottom;
        }

        let parsedRowStyle = new ParsedLengthStyle(rowStyle.height);
        if(parsedRowStyle.absolute) {
          ks.height=rowStyle.height;  // must be specified in px for rest of layout to work correctly
        } else {
          ks.height='100%';
        }

        if(!vkbd.isStatic) {
          // Set distinct phone and tablet button position properties
          btn.style.left=ks.left;
          btn.style.width=ks.width;
        }
      } else {
        ks.marginLeft=this.objectGeometry(vkbd, spec['padpc']);
      }

      totalPercent=totalPercent+spec['padpc']+spec['widthpc'];

      // Add the (US English) keycap label for layouts requesting display of underlying keys
      let keyCap = this.capLabel = this.generateKeyCapLabel();
      if(keyCap) {
        btn.appendChild(keyCap);
        keyCap.style.display = displayUnderlying ? 'block' : 'none';
      }

      // Define each key element id by layer id and key id (duplicate possible for SHIFT - does it matter?)
      btn.id=this.getId();

      // Define callbacks to handle key touches: iOS and Android tablets and phones
      // TODO: replace inline function calls??
      if(!vkbd.isStatic && !vkbd.device.touchable) {
        // Highlight key while mouse down or if moving back over originally selected key
        btn.onmouseover=btn.onmousedown=vkbd.mouseOverMouseDownHandler; // Build 360

        // Remove highlighting when key released or moving off selected element
        btn.onmouseup=btn.onmouseout=vkbd.mouseUpMouseOutHandler; //Build 360
      }

      // Make sure the key text is the element's first child - processSubkeys()
      // will add an extra element if subkeys exist, which can interfere with
      // keyboard/language name display on the space bar!
      btn.appendChild(this.label = this.generateKeyText(vkbd));

      // Handle subkey-related tasks.
      if(typeof(spec['sk']) != 'undefined' && spec['sk'] != null) {
        this.processSubkeys(btn, vkbd);
      } else {
        btn['subKeys']=null;
      }

      // Add text to button and button to placeholder div
      kDiv.appendChild(btn);

      // Prevent user selection of key captions
      //t.style.webkitUserSelect='none';

      // The 'return value' of this process.
      return {element: kDiv, percent: totalPercent - originalPercent};
    }

    objectGeometry(vkbd: VisualKeyboard, v: number): string {
      if(vkbd.usesFixedWidthScaling) {
        return (Math.round(v*100)/100)+'px';
      } else {
        return v + '%'; // round to 2 decimal places, making css more readable
      }
    }

    public refreshLayout(vkbd: VisualKeyboard) {
      super.refreshLayout(vkbd);

      let util = com.keyman.singleton.util;
      const device = vkbd.device;
      const resizeLabels = (device.OS == 'iOS' && device.formFactor == 'phone' && util.landscapeView());

      // Rescale keycap labels on iPhone (iOS 7)
      if(resizeLabels && this.capLabel) {
        this.capLabel.style.fontSize = '6px';
      }
    }
  }
}