import { ActiveSubKey } from '@keymanapp/keyboard-processor';
import OSKKey from '../../../keyboard-layout/oskKey.js';
import { KeyData, KeyElement, link } from '../../../keyElement.js';
import VisualKeyboard from '../../../visualKeyboard.js';

// Typing is to ensure that the keys specified below actually are on the type...
// and to gain Intellisense if more need to be added.

export default class OSKSubKey extends OSKKey {
  constructor(spec: ActiveSubKey, layer: string) {
    if(typeof(layer) != 'string' || layer == '') {
      throw "The 'layer' parameter for subkey construction must be properly defined.";
    }

    super(spec, layer);
  }

  getId(): string {
    // Create (temporarily) unique ID by prefixing 'popup-' to actual key ID
    return 'popup-'+this.layer+'-'+this.spec['id'];
  }

  construct(osk: VisualKeyboard, baseKey: KeyElement, topMargin: boolean): HTMLDivElement {
    let spec = this.spec;

    let kDiv=document.createElement('div');
    let tKey = osk.getDefaultKeyObject();
    let ks=kDiv.style;

    kDiv.className='kmw-key-square-ex';
    if(topMargin) {
      ks.marginTop='5px';
    }

    if(typeof spec['width'] != 'undefined') {
      ks.width=(spec['width']*baseKey.offsetWidth/100)+'px';
    } else {
      ks.width=baseKey.offsetWidth+'px';
    }
    ks.height=baseKey.offsetHeight+'px';

    let btnEle=document.createElement('div');
    let btn = this.btn = link(btnEle, new KeyData(this, spec['id']));

    this.setButtonClass();
    btn.id = this.getId();

    // Must set button size (in px) dynamically, not from CSS
    let bs=btn.style;
    bs.height=ks.height;
    bs.lineHeight=baseKey.style.lineHeight;
    bs.width=ks.width;

    // Must set position explicitly, at least for Android
    bs.position='absolute';

    btn.appendChild(this.label = this.generateKeyText(osk));
    kDiv.appendChild(btn);

    return this.square = kDiv;
  }

  public allowsKeyTip(): boolean {
    return false;
  }
}