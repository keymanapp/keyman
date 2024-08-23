import { ActiveSubKey } from 'keyman/engine/keyboard';
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
    return 'popup-'+this.spec.elementID;
  }

  construct(osk: VisualKeyboard, baseKey: KeyElement, width: number, topMargin: boolean): HTMLDivElement {
    let spec = this.spec;

    let kDiv=document.createElement('div');
    let ks=kDiv.style;

    kDiv.className='kmw-key-square-ex';
    if(topMargin) {
      ks.marginTop='5px';
    }

    ks.width=width+'px';
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