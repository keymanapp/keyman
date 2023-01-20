namespace com.keyman.dom {
  // NOTE:
  // - instanceOf -> element-wrappers, now called nestedInstanceOf
  // - forceScroll -> element-wrappers, but I believe it's only ever called from there.

  // Defines DOM-related utility functions that are not reliant on KMW's internal state.
  export class Utils {
    /**
     * Finds the `OutputTarget` associated with the specified element, or the currently-active element if not specified.
     * @param Lelem The element corresponding to the desired `OutputTarget`
     */
    static getOutputTarget(Lelem?: HTMLElement): dom.targets.OutputTarget {
      if(!Lelem) {
        // Since this may be used to test modularly, we can't depend on the existence of the KMW global.
        let keyman = com.keyman['singleton'];
        if(keyman) {
          Lelem = keyman.domManager.lastActiveElement;
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

    // /**
    //  * Function     getAbsoluteX
    //  * Scope        Public
    //  * @param       {Object}    Pobj        HTML element
    //  * @return      {number}
    //  * Description  Returns x-coordinate of Pobj element absolute position with respect to page
    //  */
    // static getAbsoluteX(Pobj: HTMLElement): number { // I1476 - Handle SELECT overlapping END
    //   var Lobj: HTMLElement

    //   if(!Pobj) {
    //     return 0;
    //   }

    //   var Lcurleft = Pobj.offsetLeft ? Pobj.offsetLeft : 0;
    //   Lobj = Pobj;   	// I2404 - Support for IFRAMEs

    //   if (Lobj.offsetParent) {
    //     while (Lobj.offsetParent) {
    //       Lobj = Lobj.offsetParent as HTMLElement;
    //       Lcurleft += Lobj.offsetLeft;
    //     }

    //     // On mobile devices, the OSK uses 'fixed' - this requires some extra offset work to handle.
    //     let Ldoc = Lobj.ownerDocument;
    //     if(Lobj.style.position == 'fixed' && Ldoc && Ldoc.scrollingElement) {
    //       Lcurleft += Ldoc.scrollingElement.scrollLeft;
    //     }
    //   }
    //   // Correct position if element is within a frame (but not if the controller is in document within that frame)
    //   // We used to reference a KMW state variable `this.keyman._MasterDocument`, but it was only ever set to `window.document`.
    //   if(Lobj && Lobj.ownerDocument && (Pobj.ownerDocument != window.document)) {
    //     var Ldoc=Lobj.ownerDocument;   // I2404 - Support for IFRAMEs

    //     if(Ldoc && Ldoc.defaultView && Ldoc.defaultView.frameElement) {
    //       return Lcurleft + Utils.getAbsoluteX(<HTMLElement>Ldoc.defaultView.frameElement) - Ldoc.documentElement.scrollLeft;
    //     }
    //   }
    //   return Lcurleft;
    // }

    // /**
    //  * Function     getAbsoluteY
    //  * Scope        Public
    //  * @param       {Object}    Pobj        HTML element
    //  * @return      {number}
    //  * Description  Returns y-coordinate of Pobj element absolute position with respect to page
    //  */
    // static getAbsoluteY(Pobj: HTMLElement): number {
    //   var Lobj: HTMLElement

    //   if(!Pobj) {
    //     return 0;
    //   }
    //   var Lcurtop = Pobj.offsetTop ? Pobj.offsetTop : 0;
    //   Lobj = Pobj;  // I2404 - Support for IFRAMEs

    //   if (Lobj.ownerDocument && Lobj instanceof Lobj.ownerDocument.defaultView.HTMLElement) {
    //     while (Lobj.offsetParent) {
    //       Lobj = Lobj.offsetParent as HTMLElement;
    //       Lcurtop += Lobj.offsetTop;
    //     }

    //     // On mobile devices, the OSK uses 'fixed' - this requires some extra offset work to handle.
    //     let Ldoc = Lobj.ownerDocument;
    //     if(Lobj.style.position == 'fixed' && Ldoc && Ldoc.scrollingElement) {
    //       Lcurtop += Ldoc.scrollingElement.scrollTop;
    //     }
    //   }

    //   // Correct position if element is within a frame (but not if the controller is in document within that frame)
    //   // We used to reference a KMW state variable `this.keyman._MasterDocument`, but it was only ever set to `window.document`.
    //   if(Lobj && Lobj.ownerDocument && (Pobj.ownerDocument != window.document)) {
    //     var Ldoc=Lobj.ownerDocument;   // I2404 - Support for IFRAMEs

    //     if(Ldoc && Ldoc.defaultView && Ldoc.defaultView.frameElement) {
    //       return Lcurtop + Utils.getAbsoluteY(<HTMLElement>Ldoc.defaultView.frameElement);
    //     }
    //   }
    //   return Lcurtop;
    // }
  }
}