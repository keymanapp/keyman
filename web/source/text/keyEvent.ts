namespace com.keyman.text {
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
  };
}