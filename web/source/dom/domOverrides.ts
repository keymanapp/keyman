namespace com.keyman.dom {
  text.prediction.LanguageProcessor.prototype.canEnable = function(): boolean {
    let keyman = com.keyman.singleton;

    if(keyman.util.getIEVersion() == 10) {
      console.warn("KeymanWeb cannot properly initialize its WebWorker in this version of IE.");
      return false;
    } else if(keyman.util.getIEVersion() < 10) {
      console.warn("WebWorkers are not supported in this version of IE.");
      return false;
    }

    return true;
  }
}