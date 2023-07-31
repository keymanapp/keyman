/**
 * Function     getAbsoluteX
 * Scope        Public
 * @param       {Object}    Pobj        HTML element
 * @return      {number}
 * Description  Returns x-coordinate of Pobj element absolute position with respect to page
 */
export function getAbsoluteX(Pobj: HTMLElement): number { // I1476 - Handle SELECT overlapping END
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
      return Lcurleft + getAbsoluteX(<HTMLElement>Ldoc.defaultView.frameElement) - Ldoc.documentElement.scrollLeft;
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
export function getAbsoluteY(Pobj: HTMLElement): number {
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
      return Lcurtop + getAbsoluteY(<HTMLElement>Ldoc.defaultView.frameElement);
    }
  }
  return Lcurtop;
}