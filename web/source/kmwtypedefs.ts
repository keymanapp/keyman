class KeyEvent {
    Ltarg: any;
    Lcode: any;
    Lstates: any;
    LmodifierChange: any;
    Lmodifiers: any;
    LisVirtualKeyCode: any;
    LisVirtualKey: any;
    vkCode: number
  };

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