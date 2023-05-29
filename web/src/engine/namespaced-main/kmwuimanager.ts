namespace com.keyman {
  export class UIManager {
    keyman: KeymanBase;

    constructor(keyman: KeymanBase) {
      this.keyman = keyman;
    }

    /**
     * Function     doLoad
     * Scope        Private
     * @return      {boolean}
     * Description  Execute UI initialization code after loading the UI
     *              // Appears to be unused; could be eliminated?  Though, doUnload IS used.
     */
    doLoad() {
      var p={};
      return this.keyman.util.callEvent('kmw.loaduserinterface',p);
    }

    /**
     * Function     doUnload
     * Scope        Private
     * @return      {boolean}
     * Description  Execute UI cleanup code before unloading the UI (may not be required?)
     */
    doUnload = function() {
      var p={};
      return this.keyman.util.callEvent('kmw.unloaduserinterface',p);
    }
  }
}