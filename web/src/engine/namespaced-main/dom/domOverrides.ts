namespace com.keyman.dom {
  text.prediction.LanguageProcessor.prototype.canEnable = function(): boolean {
    if(typeof Worker != 'function') {
      console.warn("WebWorkers are not supported by this browser.");
      return false;
    }

    return true;
  }
}