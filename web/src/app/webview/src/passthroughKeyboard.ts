import { DeviceSpec, Keyboard, KeyEvent, ManagedPromise } from 'keyman/engine/keyboard';

import { HardKeyboard, processForMnemonicsAndLegacy } from 'keyman/engine/main';

export default class PassthroughKeyboard extends HardKeyboard {
  readonly baseDevice: DeviceSpec;
  public activeKeyboard: Keyboard;

  constructor(baseDevice: DeviceSpec) {
    super();
    this.baseDevice = baseDevice;
  }

  /**
   *  API endpoint for hardware keystroke events from WebView-external keyboards
   *  (such as external keyboards for a phone used with the Android app)
   *
   *  @param  {number}  code   key identifier
   *  @param  {number}  shift  shift state (0x01=left ctrl 0x02=right ctrl 0x04=left alt 0x08=right alt
   *                                        0x10=shift 0x20=ctrl 0x40=alt)
   *  @param  {number}  lstates lock state (0x0200=no caps 0x0400=num 0x0800=no num 0x1000=scroll 0x2000=no scroll locks)
   *  @return {Promise} false when KMW _has_ fully handled the event and true when not.
   **/
  raiseKeyEvent(code: number, shift: number, lstates: number): Promise<Boolean> {
    if(code == 0) {
      return Promise.resolve(false);
    }

    // Check the virtual key
    let Lkc: KeyEvent = new KeyEvent({
      Lmodifiers: shift,
      vkCode: code,
      Lcode: code,
      Lstates: lstates,
      LisVirtualKey: true,
      kName: '',
      device: this.baseDevice,
      isSynthetic: false       // is not an OSK keystroke.
    });

    // 'us' is our default base layout; our distributed engine doesn't support
    // changing the assumed base keyboard for mnemonics.
    Lkc = processForMnemonicsAndLegacy(Lkc, this.activeKeyboard, 'us');

    const promise = new ManagedPromise<Boolean>();

    try {
      this.emit('keyevent', Lkc, (result, error) => {
        if(error) {
          promise.reject(error);
        } else {
          promise.resolve(!result || result.triggerKeyDefault);
        }
      });
    } catch (err) {
      promise.reject(err);
    }

    return promise.corePromise;
  }
}