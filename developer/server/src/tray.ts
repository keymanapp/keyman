import os = require('os');
import path = require("path");

const WindowsTrayicon = os.platform() == 'win32' ? require("./trayicon/index") : null;

// TODO: this is a Windows-only tray icon. There are a number of
// cross-platform solutions but none of them are wonderful. We
// should replace this when we find a decent one.

export default class Tray {
  private readonly signals = ['exit']; //, 'SIGINT', 'uncaughtException'];
  private myTrayApp: any;

  public start() {
    if(os.platform() == 'win32') {
      this.myTrayApp = new WindowsTrayicon({
        title: "Keyman Developer Server",
        icon: path.resolve(__dirname, "icon.ico"),
        menu: [
          /*{
            id: "item-id-stop",
            caption: "Stop Keyman Developer Server"
          },*/
          {
            id: "item-id-exit",
            caption: "Exit Keyman Developer Server"
          }
        ]
      });

      this.myTrayApp.item((id: string) => {
        //console.log(`Menu id selected=${id}`);
        switch (id) {
          case 'item-id-stop': //
            break;
          case 'item-id-exit':
            process.exit(0);
        }
      });

      //console.log('starting tray icon');
      this.signals.forEach(signal => process.on(signal, this.shutdown.bind(this)));
    }
  }

  public shutdown() {
    if(this.myTrayApp) {
      //console.debug('Closing down tray icon');
      this.myTrayApp.exit();
      this.myTrayApp = null;
    }
  }
}