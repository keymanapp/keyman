namespace com.keyman.dom {

  // Defines DOM-related utility functions that are not reliant on KMW's internal state.
  export class Utils {
    /**
     * Finds the `OutputTarget` associated with the specified element, or the currently-active element if not specified.
     * @param Lelem The element corresponding to the desired `OutputTarget`
     */
    static getOutputTarget(Lelem?: HTMLElement): text.OutputTarget {
      if(!Lelem) {
        // Since this may be used to test modularly, we can't depend on the existence of the KMW global.
        let keyman = com.keyman['singleton'];
        if(keyman) {
          Lelem = keyman.domManager.getLastActiveElement();
        }
        
        if(!Lelem) {
          // If we're trying to find an active target but one doesn't exist, just return null.
          return null;
        }
      }

      // If we were provided an element or found an active element but it's improperly attached, that should cause an error.
      if(Lelem._kmwAttachment && Lelem._kmwAttachment.interface) {
        return Lelem._kmwAttachment.interface;
      } else {
        throw new Error("OSK could not find element output target data!");
      }
    }
    
    /**
     * Function     getAbsoluteX
     * Scope        Public
     * @param       {Object}    Pobj        HTML element
     * @return      {number}               
     * Description  Returns x-coordinate of Pobj element absolute position with respect to page
     */
    static getAbsoluteX(Pobj: HTMLElement): number { // I1476 - Handle SELECT overlapping END
      var Lobj: HTMLElement

      if(!Pobj) {
        return 0;
      }
      
      var Lcurleft = Pobj.offsetLeft ? Pobj.offsetLeft : 0;
      Lobj = Pobj;   	// I2404 - Support for IFRAMEs

      if (Lobj.offsetParent) {
        while (Lobj.offsetParent) {
          Lobj = Lobj.offsetParent as HTMLElement;
          Lcurleft += Lobj.offsetLeft;
        }

        // On mobile devices, the OSK uses 'fixed' - this requires some extra offset work to handle.
        let Ldoc = Lobj.ownerDocument;
        if(Lobj.style.position == 'fixed' && Ldoc && Ldoc.scrollingElement) {
          Lcurleft += Ldoc.scrollingElement.scrollLeft;
        }
      }
      // Correct position if element is within a frame (but not if the controller is in document within that frame)
      // We used to reference a KMW state variable `this.keyman._MasterDocument`, but it was only ever set to `window.document`.
      if(Lobj && Lobj.ownerDocument && (Pobj.ownerDocument != window.document)) {
        var Ldoc=Lobj.ownerDocument;   // I2404 - Support for IFRAMEs

        if(Ldoc && Ldoc.defaultView && Ldoc.defaultView.frameElement) {
          return Lcurleft + Utils.getAbsoluteX(<HTMLElement>Ldoc.defaultView.frameElement) - Ldoc.documentElement.scrollLeft;
        }
      }
      return Lcurleft;
    }

    /**
     * Function     getAbsoluteY
     * Scope        Public
     * @param       {Object}    Pobj        HTML element
     * @return      {number}               
     * Description  Returns y-coordinate of Pobj element absolute position with respect to page
     */  
    static getAbsoluteY(Pobj: HTMLElement): number {
      var Lobj: HTMLElement

      if(!Pobj) {
        return 0;
      }
      var Lcurtop = Pobj.offsetTop ? Pobj.offsetTop : 0;
      Lobj = Pobj;  // I2404 - Support for IFRAMEs

      if (Lobj.ownerDocument && Lobj instanceof Lobj.ownerDocument.defaultView.HTMLElement) {
        while (Lobj.offsetParent) {
          Lobj = Lobj.offsetParent as HTMLElement;
          Lcurtop += Lobj.offsetTop;
        }

        // On mobile devices, the OSK uses 'fixed' - this requires some extra offset work to handle.
        let Ldoc = Lobj.ownerDocument;
        if(Lobj.style.position == 'fixed' && Ldoc && Ldoc.scrollingElement) {
          Lcurtop += Ldoc.scrollingElement.scrollTop;
        }
      }

      // Correct position if element is within a frame (but not if the controller is in document within that frame)
      // We used to reference a KMW state variable `this.keyman._MasterDocument`, but it was only ever set to `window.document`.
      if(Lobj && Lobj.ownerDocument && (Pobj.ownerDocument != window.document)) {
        var Ldoc=Lobj.ownerDocument;   // I2404 - Support for IFRAMEs

        if(Ldoc && Ldoc.defaultView && Ldoc.defaultView.frameElement) {
          return Lcurtop + Utils.getAbsoluteY(<HTMLElement>Ldoc.defaultView.frameElement);
        }
      }
      return Lcurtop;
    }

    /**
     * Checks the type of an input DOM-related object while ensuring that it is checked against the correct prototype,
     * as class prototypes are (by specification) scoped upon the owning Window.
     * 
     * See https://stackoverflow.com/questions/43587286/why-does-instanceof-return-false-on-chrome-safari-and-edge-and-true-on-firefox
     * for more details.
     * 
     * @param {Element|Event}   Pelem       An element of the web page or one of its IFrame-based subdocuments.
     * @param {string}          className   The plain-text name of the expected Element type.
     * @return {boolean}
     */
    static instanceof(Pelem: Event|EventTarget, className: string): boolean {
      // We must write special checks for our custom-defined element types!
      if(className == "TouchAliasElement") {
        if(this.instanceof(Pelem, "HTMLDivElement")) {
          let div = <HTMLDivElement> Pelem;

          // We should probably implement a slightly more robust check, but this should get us started well enough.
          return div['base'] !== undefined;
        } else {
          return false;
        }
      }

      var scopedClass;

      if(!Pelem) {
        // If we're bothering to check something's type, null references don't match
        // what we're looking for. 
        return false;
      }
      if (Pelem['Window']) { // Window objects contain the class definitions for types held within them.  So, we can check for those.
        return className == 'Window';
      } else if (Pelem['defaultView']) { // Covers Document.
        scopedClass = Pelem['defaultView'][className];
      } else if(Pelem['ownerDocument']) {
        scopedClass = (Pelem as Node).ownerDocument.defaultView[className];
      } else if(Pelem['target']) {
        var event = Pelem as Event;

        if(this.instanceof(event.target, 'Window')) {
          scopedClass = event.target[className]; 
        } else if(this.instanceof(event.target, 'Document')) {
          scopedClass = (event.target as Document).defaultView[className];
        } else if(this.instanceof(event.target, 'HTMLElement')) {
          scopedClass = (event.target as HTMLElement).ownerDocument.defaultView[className];
        }
      }

      if(scopedClass) {
        return Pelem instanceof scopedClass;
      } else {
        return false;
      }
    }

    static forceScroll(element: HTMLInputElement | HTMLTextAreaElement) {
      // Needed to allow ./build_dev_resources.sh to complete;
      // only executes when com.keyman.DOMEventHandlers is defined.
      //
      // We also bypass this whenever operating in the embedded format.
      if(com && com.keyman && com.keyman['DOMEventHandlers'] && !com.keyman['singleton']['isEmbedded']) {
        let DOMEventHandlers = com.keyman['DOMEventHandlers'];

        let selectionStart = element.selectionStart;
        let selectionEnd = element.selectionEnd;

        DOMEventHandlers.states._IgnoreBlurFocus = true;
        //Forces scrolling; the re-focus triggers the scroll, at least.
        element.blur();
        element.focus();
        DOMEventHandlers.states._IgnoreBlurFocus = false;

        // On Edge, it appears that the blur/focus combination will reset the caret position
        // under certain scenarios during unit tests.  So, we re-set it afterward.
        element.selectionStart = selectionStart;
        element.selectionEnd = selectionEnd;
      }
    }
  }
}