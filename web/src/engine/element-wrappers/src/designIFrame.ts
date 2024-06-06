import OutputTarget from './outputTarget.js';

class SelectionCaret {
  node: Node;
  offset: number;

  constructor(node: Node, offset: number) {
    this.node = node;
    this.offset = offset;
  }
}

class SelectionRange {
  start: SelectionCaret;
  end: SelectionCaret;

  constructor(start: SelectionCaret, end: SelectionCaret) {
    this.start = start;
    this.end = end;
  }
}

class StyleCommand {
  cmd: string;
  stateType: number;
  cache: string|boolean;

  constructor(c: string, s:number) {
    this.cmd = c;
    this.stateType = s;
  }
}

export default class DesignIFrame extends OutputTarget<{}> {
  root: HTMLIFrameElement;
  doc: Document;
  docRoot: HTMLElement;

  commandCache: StyleCommand[];

  constructor(ele: HTMLIFrameElement) {
    super();
    this.root = ele;

    if(ele.contentWindow && ele.contentWindow.document && ele.contentWindow.document.designMode == 'on') {
      this.doc = ele.contentWindow.document;
      this.docRoot = ele.contentWindow.document.documentElement;
    } else {
      throw "Specified IFrame is not in design-mode!";
    }
  }

  get isSynthetic(): boolean {
    return false;
  }

  getElement(): HTMLIFrameElement {
    return this.root;
  }

  focus(): void {
    this.doc.defaultView.focus(); // I3363 (Build 301)
  }

  isSelectionEmpty(): boolean {
    if(!this.hasSelection()) {
      return true;
    }

    return this.doc.getSelection().isCollapsed;
  }

  hasSelection(): boolean {
    let Lsel = this.doc.getSelection();
    let outerSel = document.getSelection();

    // If the outer doc's selection matches, we're active.
    if(outerSel.anchorNode == Lsel.anchorNode && outerSel.focusNode == Lsel.focusNode) {
      return true;
    } else {
      // Problem:  for testing, we can't enforce the ideal (ie: first) condition.
      // Technically, the IFrame _will_ always have its own internal selection, though... so... it kinda works?
      return true;
    }
  }

  clearSelection(): void {
    if(this.hasSelection()) {
      let Lsel = this.doc.getSelection();

      if(!Lsel.isCollapsed) {
        Lsel.deleteFromDocument();  // I2134, I2192
      }
    } else {
      console.warn("Attempted to clear an unowned Selection!");
    }
  }

  invalidateSelection(): void { /* No cache maintenance needed here, partly because
                                  * it's impossible to cache a Selection; it mutates.
                                  */  }

  getCarets(): SelectionRange {
    let Lsel = this.doc.getSelection();
    let code = Lsel.anchorNode.compareDocumentPosition(Lsel.focusNode);

    if(Lsel.isCollapsed) {
      let caret = new SelectionCaret(Lsel.anchorNode, Lsel.anchorOffset);
      return new SelectionRange(caret, caret);
    } else {
      let anchor = new SelectionCaret(Lsel.anchorNode, Lsel.anchorOffset);
      let focus = new SelectionCaret(Lsel.focusNode, Lsel.focusOffset);

      if(anchor.node == focus.node) {
        code = (focus.offset - anchor.offset > 0) ? 2 : 4;
      }

      if(code & 2) {
        return new SelectionRange(anchor, focus);
      } else { // Default
        // can test against code & 4 to ensure Focus is before anchor, though.
        return new SelectionRange(focus, anchor);
      }
    }
  }

  getDeadkeyCaret(): number {
    return this.getTextBeforeCaret().kmwLength();
  }

  getTextBeforeCaret(): string {
    if(!this.hasSelection()) {
      return this.getText();
    }

    let caret = this.getCarets().start;

    if(caret.node.nodeType != 3) {
      return ''; // Must be a text node to provide a context.
    }

    return caret.node.textContent.substr(0, caret.offset);
  }

  getSelectedText(): string {
    // TODO:  figure out the proper implementation.
    // KMW 16 and before behavior may be maintained by just returning the empty string.
    return '';
  }

  getTextAfterCaret(): string {
    if(!this.hasSelection()) {
      return '';
    }

    let caret = this.getCarets().end;

    if(caret.node.nodeType != 3) {
      return ''; // Must be a text node to provide a context.
    }

    return caret.node.textContent.substr(caret.offset);
  }

  getText(): string {
    return this.docRoot.innerText;
  }

  deleteCharsBeforeCaret(dn: number) {
    if(!this.hasSelection() || dn <= 0) {
      return;
    }

    let start = this.getCarets().start;

    // Bounds-check on the number of chars to delete.
    if(dn > start.offset) {
      dn = start.offset;
    }

    if(start.node.nodeType != 3) {
      console.warn("Deletion of characters requested without available context!");
      return; // No context to delete characters from.
    }

    let range = this.doc.createRange();
    let dnOffset = start.offset - start.node.nodeValue.substr(0, start.offset)._kmwSubstr(-dn).length;

    range.setStart(start.node, dnOffset);
    range.setEnd(start.node, start.offset);

    this.adjustDeadkeys(-dn);
    range.deleteContents();
    // No need to reposition the caret - the DOM will auto-move the selection accordingly, since
    // we didn't use the selection to delete anything.
  }

  insertTextBeforeCaret(s: string) {
    if(!this.hasSelection()) {
      return;
    }

    let start = this.getCarets().start;
    let delta = s._kmwLength();
    let Lsel = this.doc.getSelection();

    if(delta == 0) {
      return;
    }

    this.adjustDeadkeys(delta);

    // While Selection.extend() was really nice for this, IE didn't support it whatsoever.
    // However, IE (11, at least) DID support setting selections via ranges, so we were still
    // able to manage the caret properly.
    //
    // TODO:  double-check that it was only IE-motivated, re-implement with Selection.extend().
    let finalCaret = this.root.ownerDocument.createRange();

    if(start.node.nodeType == 3) {
      let textStart = <Text> start.node;
      textStart.insertData(start.offset, s);
      finalCaret.setStart(textStart, start.offset + s.length);
    } else {
      // Create a new text node - empty control
      var n = this.doc.createTextNode(s);

      let range = this.doc.createRange();
      range.setStart(start.node, start.offset);
      range.collapse(true);
      range.insertNode(n);
      finalCaret.setStart(n, s.length);
    }

    finalCaret.collapse(true);
    Lsel.removeAllRanges();
    try {
      Lsel.addRange(finalCaret);
    } catch(e) {
      // Chrome (through 4.0 at least) throws an exception because it has not synchronised its content with the selection.
      // scrollIntoView synchronises the content for selection
      start.node.parentElement.scrollIntoView();
      Lsel.addRange(finalCaret);
    }
    Lsel.collapseToEnd();
  }

  handleNewlineAtCaret(): void {
    // TODO:  Implement.
    //
    // As it turns out, we never had an implementation for handling newline inputs from the OSK for this element type.
    // At least this way, it's more explicit.
    //
    // Note:  consult "// Create a new text node - empty control" case in insertTextBeforeCaret -
    // this helps to handle the browser-default implementation of newline handling.  In particular,
    // entry of the first character after a newline.
    //
    // If raw newlines are entered into the HTML, but as with usual HTML, they're interpreted as excess whitespace and
    // have no effect.  We need to add DOM elements for a functional newline.
  }

  protected setTextAfterCaret(s: string) {
    if(!this.hasSelection()) {
      return;
    }

    let caret = this.getCarets().end;
    let delta = s._kmwLength();

    if(delta == 0) {
      return;
    }

    // This is designed explicitly for use in direct-setting operations; deadkeys
    // will be handled after this method.

    if(caret.node.nodeType == 3) {
      let textStart = <Text> caret.node;
      textStart.replaceData(caret.offset, textStart.length, s);
    } else {
      // Create a new text node - empty control
      var n = caret.node.ownerDocument.createTextNode(s);

      let range = this.root.ownerDocument.createRange();
      range.setStart(caret.node, caret.offset);
      range.collapse(true);
      range.insertNode(n);
    }
  }

  /**
   * Function     saveProperties
   * Scope        Private
   * Description  Build and create list of styles that can be applied in iframes
   */
  saveProperties() {
    // Formerly _CacheCommands.
    var _CacheableCommands=[
      new StyleCommand('backcolor',1), new StyleCommand('fontname',1), new StyleCommand('fontsize',1),
      new StyleCommand('forecolor',1), new StyleCommand('bold',0), new StyleCommand('italic',0),
      new StyleCommand('strikethrough',0), new StyleCommand('subscript',0),
      new StyleCommand('superscript',0), new StyleCommand('underline',0)
    ];

    if(this.doc.defaultView) {
      _CacheableCommands.push(new StyleCommand('hilitecolor',1));
    }

    for(var n=0; n < _CacheableCommands.length; n++) { // I1511 - array prototype extended
      let cmd = _CacheableCommands[n];
      //KeymanWeb._Debug('Command:'+_CacheableCommands[n][0]);
      if(cmd.stateType == 1) {
        cmd.cache = this.doc.queryCommandValue(cmd.cmd);
      } else {
        cmd.cache = this.doc.queryCommandState(cmd.cmd);
      }
    }
    this.commandCache = _CacheableCommands;
  }

  /**
   * Function     restoreProperties
   * Scope        Private
   * Description  Restore styles in IFRAMEs (??)
   */
  restoreProperties(_func?: () => void): void {
    // Formerly _CacheCommandsReset.
    if(!this.commandCache) {
      console.error("No command cache exists to restore!");
    }

    for(var n=0; n < this.commandCache.length; n++) { // I1511 - array prototype extended
      let cmd = this.commandCache[n];

      //KeymanWeb._Debug('ResetCacheCommand:'+_CacheableCommands[n][0]+'='+_CacheableCommands[n][2]);
      if(cmd.stateType == 1) {
        if(this.doc.queryCommandValue(cmd.cmd) != cmd.cache) {
          if(_func) {
            _func();
          }
          this.doc.execCommand(cmd.cmd, false, <string> cmd.cache);
        }
      } else if(this.doc.queryCommandState(cmd.cmd) != cmd.cache) {
        if(_func) {
          _func();
        }
        //KeymanWeb._Debug('executing command '+_CacheableCommand[n][0]);
        this.doc.execCommand(cmd.cmd, false, null);
      }
    }
  }

  doInputEvent() {
    // Root = the iframe, the outermost component and the one we were originally told to attach to.
    this.dispatchInputEventOn(this.root);
  }
}