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
  }
}