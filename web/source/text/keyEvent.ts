namespace com.keyman.text {
  /**
   * This class is defined within its own file so that it can be loaded by code outside of KMW without
   * having to actually load the entirety of KMW.
   */
  export class KeyEvent {
    Ltarg: HTMLElement;
    Lcode: number;
    Lstates: number;
    LmodifierChange?: boolean;
    Lmodifiers: number;
    LisVirtualKey: boolean;
    vkCode: number;
    kName: string;
    kLayer?: string;
    kNextLayer?: string;
  };
}