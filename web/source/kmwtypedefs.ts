class KeyEvent {
    Ltarg: HTMLElement;
    Lcode: number;
    Lstates: number;
    LmodifierChange: boolean;
    Lmodifiers: number;
    LisVirtualKeyCode: boolean;
    LisVirtualKey: boolean;
    vkCode: number
};

class LegacyKeyEvent {
  Ltarg: HTMLElement;
  Lcode: number;
  Lmodifiers: number;
  LisVirtualKey: number;
}

class KeyInformation {
    vk: boolean;
    code: number;
    modifiers: number;
}

class StyleCommand {
    cmd: string;
    state: number;

    constructor(c: string, s:number) {
      this.cmd = c;
      this.state = s;
    }
  }