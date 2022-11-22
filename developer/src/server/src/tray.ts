import os = require('os');

class TrayStub {
  public start(localPort: number, ngrokAddress: string) {}
  public restart(localPort: number, ngrokAddress: string) {}
  public shutdown() {};
};

let tray = new TrayStub();

if(os.platform() == 'win32') {
  const Win32Tray = require('./win32-tray');
  tray = new Win32Tray();
}

export default tray;
