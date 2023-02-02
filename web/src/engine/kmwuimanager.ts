namespace com.keyman {

  export class UIState {
    ['activated']: boolean;
    ['activationPending']: boolean;

    constructor(pending: boolean, activated: boolean) {
      this['activationPending'] = pending;
      this['activated'] = activated;
    }
  }

  export class UIManager {
    keyman: KeymanBase;

    isActivating: boolean = false;    // ActivatingKeymanWebUI - is the KeymanWeb DIV in process of being clicked on?
    justActivated: boolean = false;   // JustActivatedKeymanWebUI - focussing back to control after KeymanWeb UI interaction

    constructor(keyman: KeymanBase) {
      this.keyman = keyman;
    }

    /**
     * Function     getUIState
     * Scope        Public   
     * @return      {Object.<string,boolean>}
     * Description  Return object with activation state of UI:
     *                activationPending (bool):   KMW being activated
     *                activated         (bool):   KMW active    
     */    
    getUIState(): UIState {
      return new UIState(this.isActivating, this.justActivated);
    }

    /**
     * Set or clear the IsActivatingKeymanWebUI flag (exposed function)
     * 
     * @param       {(boolean|number)}  state  Activate (true,false)
     */
    setActivatingUI(state: boolean|number) {
      this.isActivating = state ? true : false;
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