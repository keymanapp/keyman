//
//  InputViewController.swift
//  KMEI
//
//  Created by Gabriel Wong on 2017-09-29.
//  Copyright © 2017 SIL International. All rights reserved.
//

import AudioToolbox
import UIKit

public enum GlobeKeyTapBehaviour {
  case switchToNextKeyboard
  case switchToNextInputMethod
  case doNothing
}

public enum MenuBehaviour {
  case showAlways
  case showIfMultipleKeyboards
  case showNever
}

private class CustomInputView: UIInputView, UIInputViewAudioFeedback {
  var setFrame: CGRect = CGRect.zero
  var keymanWeb: KeymanWebViewController!

  // Constraints dependent upon the device's current rotation state.
  // For now, should be mostly upon keymanWeb.view.heightAnchor.
  var portraitConstraint: NSLayoutConstraint?
  var landscapeConstraint: NSLayoutConstraint?

  init(frame: CGRect, innerVC: KeymanWebViewController!, inputViewStyle: UIInputView.Style) {
    super.init(frame: frame, inputViewStyle: inputViewStyle)
    self.setFrame = frame
    self.keymanWeb = innerVC
  }

  required init?(coder: NSCoder) {
    super.init(coder: coder)
  }

  public var enableInputClicksWhenVisible: Bool {
    get {
      // Implemented as noted by https://developer.apple.com/documentation/uikit/uidevice/1620050-playinputclick.
      return true
    }
  }

  override var intrinsicContentSize: CGSize {
    /*
     * This function is the motivating reason for this class to exist as-is.  If we return the default value
     * for this property, we cannot properly control the keyboard's scale in a manner consistent across both
     * use cases: in-app and system-wide.
     */
    return self.setFrame.size
  }

  // Allows us to intercept value assignments to keep `intrinsicContentSize` properly updated.
  override var frame: CGRect {
    get {
      return super.frame
    }

    set(value) {
      super.frame = value

      // Store the originally-intended value, just in case iOS changes it later without our consent.
      self.setFrame = value
    }
  }

  func setConstraints() {
    let innerView = keymanWeb.view!

    var guide: UILayoutGuide
    var conditionalGuide: Bool = false

    if #available(iOSApplicationExtension 11.0, *) {
      guide = self.safeAreaLayoutGuide
      conditionalGuide = true
    } else {
      guide = self.layoutMarginsGuide
    }

    // Fallback on earlier versions
    innerView.topAnchor.constraint(equalTo:    guide.topAnchor).isActive = true
    innerView.bottomAnchor.constraint(equalTo: guide.bottomAnchor).isActive = true
    if conditionalGuide {
      innerView.leftAnchor.constraint(equalTo:   guide.leftAnchor).isActive = true
      innerView.rightAnchor.constraint(equalTo:  guide.rightAnchor).isActive = true
    } else {
      innerView.leftAnchor.constraint(equalTo: self.leftAnchor).isActive = true
      innerView.rightAnchor.constraint(equalTo: self.rightAnchor).isActive = true
    }

    // Allow these to be broken if/as necessary to resolve layout issues.
    var kbdWidthConstraint: NSLayoutConstraint
    if conditionalGuide {
      kbdWidthConstraint = innerView.widthAnchor.constraint(equalTo: guide.widthAnchor)
    } else {
      kbdWidthConstraint = innerView.widthAnchor.constraint(equalTo: self.widthAnchor)
    }
    kbdWidthConstraint.priority = .defaultHigh
    kbdWidthConstraint.isActive = true

    // Cannot be met by the in-app keyboard, but helps to 'force' height for the system keyboard.
    let portraitHeight = innerView.heightAnchor.constraint(equalToConstant: keymanWeb.constraintTargetHeight(isPortrait: true))
    portraitHeight.identifier = "Height constraint for portrait mode"
    portraitHeight.priority = .defaultHigh
    let landscapeHeight = innerView.heightAnchor.constraint(equalToConstant: keymanWeb.constraintTargetHeight(isPortrait: false))
    landscapeHeight.identifier = "Height constraint for landscape mode"
    landscapeHeight.priority = .defaultHigh

    portraitConstraint = portraitHeight
    landscapeConstraint = landscapeHeight
    // .isActive will be set according to the current portrait/landscape perspective.
  }

  override func updateConstraints() {
    super.updateConstraints()

    // Keep the constraints up-to-date!  They should vary based upon the selected keyboard.
    let userData = Storage.active.userDefaults
    let alwaysShow = userData.bool(forKey: Key.optShouldShowBanner)

    var hideBanner = true
    if alwaysShow || Manager.shared.isSystemKeyboard || keymanWeb.activeModel {
      hideBanner = false
    }
    let topBarDelta = hideBanner ? 0 : InputViewController.topBarHeight

    // Sets height before the constraints, as it's the height constraint that triggers OSK resizing.
    keymanWeb.setBannerHeight(to: Int(InputViewController.topBarHeight))

    portraitConstraint?.constant = topBarDelta + keymanWeb.constraintTargetHeight(isPortrait: true)
    landscapeConstraint?.constant = topBarDelta + keymanWeb.constraintTargetHeight(isPortrait: false)

    // Activate / deactivate layout-specific constraints.
    if InputViewController.isPortrait {
      landscapeConstraint?.isActive = false
      portraitConstraint?.isActive = true
    } else {
      portraitConstraint?.isActive = false
      landscapeConstraint?.isActive = true
    }
  }
}

// ---------------------------

open class InputViewController: UIInputViewController, KeymanWebDelegate {
  public var menuCloseButtonTitle: String?
  public var isInputClickSoundEnabled = true
  public var globeKeyTapBehaviour = GlobeKeyTapBehaviour.switchToNextKeyboard
  public var menuBehaviour = MenuBehaviour.showAlways

  var _isSystemKeyboard: Bool
  var isSystemKeyboard: Bool {
    return _isSystemKeyboard;
  }
  
  // Constraints dependent upon the device's current rotation state.
  // For now, should be mostly upon keymanWeb.view.heightAnchor.
  var portraitConstraint: NSLayoutConstraint?
  var landscapeConstraint: NSLayoutConstraint?

  private var keymanWeb: KeymanWebViewController
  
  private var swallowBackspaceTextChange: Bool = false
  private var swallowContextChangeCount: Int = 0

  open class var isPortrait: Bool {
    return UIScreen.main.bounds.width < UIScreen.main.bounds.height
  }

  open class var topBarHeight: CGFloat {
    let scaling = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: self.isPortrait)

    return scaling?.bannerHeight ?? 38 // default for iPhone SE, older/smaller devices
  }

  open override var hasFullAccess: Bool {
    if #available(iOS 11.0, *) {
      // Nice and straight-forward here!
      return super.hasFullAccess
    }
    return Storage.shared != nil
  }

  private var keyboardListCount: Int {
    return Storage.active.userDefaults.userKeyboards?.count ?? 0
  }

  var expandedHeight: CGFloat {
    return keymanWeb.keyboardHeight + activeTopBarHeight
  }
  
  public convenience init() {
    // iOS will call this constructor to initialize the system keyboard app extension.
    // It's safe and there will only ever be one active instance of this class within process scope.
    // See https://developer.apple.com/library/archive/documentation/General/Conceptual/ExtensibilityPG/ExtensionOverview.html
    self.init(forSystem: true)
  }
  
  public convenience init(forSystem: Bool) {
    // In-app uses of the keyboard should call this constructor for simplicity, setting `forSystem`=`false`.
    self.init(nibName: nil, bundle: nil)
    _isSystemKeyboard = forSystem
  }

  public override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    // Must set within this constructor, even if we override it immediately after in the convenience inits.
    _isSystemKeyboard = true
    keymanWeb = KeymanWebViewController(storage: Storage.active)
    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
    
    addChild(keymanWeb)
  }

  public required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  open override func updateViewConstraints() {
    resetKeyboardState()

    super.updateViewConstraints()
  }
  
  open override func loadView() {
    let baseView = CustomInputView(frame: CGRect.zero, innerVC: keymanWeb, inputViewStyle: .keyboard)
    baseView.backgroundColor = Colors.keyboardBackground

    // TODO: If the following line is enabled, the WKWebView does not respond to touch events
    // Can figure out why one day maybe
    baseView.translatesAutoresizingMaskIntoConstraints = false
    baseView.autoresizingMask = UIView.AutoresizingMask.flexibleHeight.union(.flexibleWidth)

    keymanWeb.delegate = self

    baseView.addSubview(keymanWeb.view)

    //view = baseView
    inputView = baseView
  }

  open override func viewDidLoad() {
    super.viewDidLoad()

    keymanWeb.resetKeyboardState()
    setInnerConstraints()

    let activeUserDef = Storage.active.userDefaults
    let standardUserDef = UserDefaults.standard
    let activeDate = (activeUserDef.object(forKey: Key.synchronizeSWKeyboard) as? [Date])?[0]
    let standardDate = (standardUserDef.object(forKey: Key.synchronizeSWKeyboard) as? [Date])?[0]

    let shouldSynchronize: Bool
    if let standardDate = standardDate,
       let activeDate = activeDate {
      shouldSynchronize = standardDate != activeDate
    } else if activeDate == nil {
      shouldSynchronize = false
    } else {
      shouldSynchronize = true
    }

    if (!Manager.shared.didSynchronize || shouldSynchronize) && Storage.shared != nil {
      Manager.shared.synchronizeSWKeyboard()
      if Manager.shared.currentKeyboardID != nil || Manager.shared.shouldReloadKeyboard {
        Manager.shared.shouldReloadKeyboard = true
        reload()
      }
      Manager.shared.didSynchronize = true
      standardUserDef.set(activeUserDef.object(forKey: Key.synchronizeSWKeyboard),
                          forKey: Key.synchronizeSWKeyboard)
      standardUserDef.synchronize()
    }
  }

  open override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)

    if Manager.shared.shouldReloadKeyboard {
      self.reload()
    }
  }

  open override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)

    // When using the system keyboard, sets the system-initialized version of the keyboard
    // as Manager.shared's inputViewController.
    Manager.shared.inputViewController = self

    setOuterConstraints()
    inputView?.setNeedsUpdateConstraints()

    keymanWeb.verifyLoaded()
  }

  open override func viewWillDisappear(_ animated: Bool) {
    super.viewWillDisappear(animated)
    // Necessary for existing infrastructure to resend info for the keyboard after reloading
    // as system keyboard.  Do NOT perform if in-app, as this unnecessarily resets the WebView.
    if(Manager.shared.isSystemKeyboard) {
      Manager.shared.shouldReloadKeyboard = true
    }
  }

  open override func textDidChange(_ textInput: UITextInput?) {
    // Swallows self-triggered calls from emptying the context due to keyboard rules
    if self.swallowBackspaceTextChange && textDocumentProxy.documentContextBeforeInput == nil {
      self.swallowBackspaceTextChange = false
      return
    }

    // What the actual heck, Apple.  Apparently, in system-keyboard mode, this is also self-triggered if we erase back to a newline.
    // Refer to https://github.com/keymanapp/keyman/pull/2770 for context.
    if self.swallowBackspaceTextChange && Manager.shared.isSystemKeyboard && textDocumentProxy.documentContextBeforeInput == "\n" {
      self.swallowBackspaceTextChange = false
      return
    }

    // Currently motivated by the need to shift caret position to perform
    // right-deletions.
    if self.swallowContextChangeCount > 0 {
      self.swallowContextChangeCount -= 1
      return
    }
    
    let contextBeforeInput = textDocumentProxy.documentContextBeforeInput ?? ""
    let contextAfterInput = textDocumentProxy.documentContextAfterInput ?? ""
    let context = "\(contextBeforeInput)\(contextAfterInput)"

    let newRange: Range<String.Index>
    if let range = context.range(of: contextBeforeInput) {
      newRange = range.upperBound..<range.upperBound
    } else {
      newRange = context.startIndex..<context.startIndex
    }

    setContextState(text: context, range: NSRange(newRange, in: context))
    // Within the app, this is triggered after every keyboard input.
    // We should NOT call .resetContext() here for this reason.
  }
  
  func insertText(_ keymanWeb: KeymanWebViewController, numCharsToLeftDelete: Int, newText: String, numCharsToRightDelete: Int) {
    if keymanWeb.isSubKeysMenuVisible {
      return
    }

    if isInputClickSoundEnabled {
      UIDevice.current.playInputClick()

      // Disable input click sound for 0.1 second to ensure it plays for single key stroke.
      isInputClickSoundEnabled = false
      perform(#selector(self.enableInputClickSound), with: nil, afterDelay: 0.1)
    }

    if numCharsToLeftDelete <= 0 {
      // A full-context deletion will report numCharsToDelete == 0 and won't
      // otherwise delete selected text.
      if #available(iOSApplicationExtension 11.0, *) {
        if let selected = textDocumentProxy.selectedText {
          if selected.count > 0 {
            textDocumentProxy.deleteBackward()
          }
        }
      }
    } else {
      for _ in 0..<numCharsToLeftDelete {
        let oldContext = textDocumentProxy.documentContextBeforeInput ?? ""
        textDocumentProxy.deleteBackward()
        let newContext = textDocumentProxy.documentContextBeforeInput ?? ""
        let unitsDeleted = oldContext.utf16.count - newContext.utf16.count
        if unitsDeleted > 1 {
          if !InputViewController.isSurrogate(oldContext.utf16.last!) {
            let lowerIndex = oldContext.utf16.index(oldContext.utf16.startIndex,
                                                    offsetBy: newContext.utf16.count)
            let upperIndex = oldContext.utf16.index(lowerIndex, offsetBy: unitsDeleted - 1)
            let remnant = String(oldContext.utf16[lowerIndex..<upperIndex]) ?? ""
            textDocumentProxy.insertText(remnant)
          }
        }

        // Refer to `func textDidChange()` and https://github.com/keymanapp/keyman/pull/2770 for context.
        if textDocumentProxy.documentContextBeforeInput == nil ||
           (textDocumentProxy.documentContextBeforeInput == "\n" && Manager.shared.isSystemKeyboard) {
          if(self.swallowBackspaceTextChange) {
            // A single keyboard processing command should never trigger two of these in a row;
            // only one output function will perform deletions.

            // This should allow us to debug any failures of this assumption.
            // So far, only occurs when debugging a breakpoint during a touch event on BKSP,
            // so all seems good.
            log.verbose("Failed to swallow a recent textDidChange call!")
          }
          self.swallowBackspaceTextChange = true
          break
        }
      }
    }

    if !newText.isEmpty {
      textDocumentProxy.insertText(newText)
    }

    // Should never change throughout the next loop; right-deletions only!
    let oldLeftContext = textDocumentProxy.documentContextBeforeInput ?? ""
    var pointsToDelete = numCharsToRightDelete
    while pointsToDelete > 0 {
      let oldContext = textDocumentProxy.documentContextAfterInput ?? ""
      self.swallowContextChangeCount += 1
      // Asynchronously triggers a context change.
      textDocumentProxy.adjustTextPosition(byCharacterOffset: 1)
      let newContext = textDocumentProxy.documentContextAfterInput ?? ""

      var failsafeCount = 0
      while (textDocumentProxy.documentContextBeforeInput ?? "" != oldLeftContext)
            && failsafeCount < numCharsToRightDelete { // mild breakage > locked kbd
        // While adjustTextPosition makes cluster-based jumps...
        // deleteBackward does not.  So, we 'force' it.
        textDocumentProxy.deleteBackward()
        failsafeCount += 1
      }

      // Determine the removed codepoint count.  Also, find the index
      // of the first character that shouldn't be removed, if possible.
      let unitsDeleted = oldContext.utf16.count - newContext.utf16.count
      var pointsDeleted = 0

      // Our current index within the deleted context.
      var deletedIndex = oldContext.utf16.startIndex
      // Marks the first index we wish to NOT remove.
      var remnantIndex: String.Index? = nil
      // Marks the first index that was NOT removed by deleteBackward.
      let undeletedStartIndex = oldContext.utf16.index(deletedIndex, offsetBy: unitsDeleted)

      while deletedIndex < undeletedStartIndex {
        if(InputViewController.isSurrogate(oldContext.utf16[deletedIndex])) {
          // Check - is it truly a surrogate pair?
          let pairedIndex = oldContext.utf16.index(after: deletedIndex)
          if(InputViewController.isSurrogate(oldContext.utf16[pairedIndex])) {
            // If so, pre-emptively increase the index - the pair will count
            // as a single character as a result.
            deletedIndex = pairedIndex
          }
        }
        pointsDeleted += 1
        deletedIndex = oldContext.utf16.index(after: deletedIndex)

        // Intended end of deletion found!
        if pointsDeleted == pointsToDelete {
          remnantIndex = deletedIndex
          break // There's nothing to be gained by further loop iterations.
        }
      }

      if let remnantIndex = remnantIndex, remnantIndex < undeletedStartIndex {
        // We need to restore some of the deleted text!
        let remnant = String(oldContext.utf16[remnantIndex..<undeletedStartIndex]) ?? ""
        textDocumentProxy.insertText(remnant)

        // Must place the caret back in its correct position!
        // Unfortunately, Apple's adjustTextPosition is definitely cluster-based.

        var leftContext = textDocumentProxy.documentContextBeforeInput ?? ""
        // Ideally, we want to stop at the exact position, but the lack of
        // granularity on adjustTextPosition may not cooperate with that goal.
        while(leftContext.count > oldLeftContext.count) {
          textDocumentProxy.adjustTextPosition(byCharacterOffset: -remnant.count)
          leftContext = textDocumentProxy.documentContextBeforeInput ?? ""
        }

        // FAILSAFE, non-ideal behavior below.
        //
        // It's probably (?) better to keep the caret at the end of a cluster that
        // previously started before the caret, even if right-deletions add new
        // characters to the cluster.  I think.
        //
        // Khmer example:  if right-deletes leave a joeung-S that can attach
        // to a main consonant to the left of my cursor, it will 'snap' into
        // a cluster.  It'd be more natural to have that cluster on the left,
        // so I can add more chars to the cluster if desired.
        //
        // It's also easier to immediately backspace the extra chars if needed
        // this way, rather than forcing a caret reposition.
        if(leftContext.count < oldLeftContext.count) {
          log.debug("Could not reposition caret perfectly after a right-delete!")
          textDocumentProxy.adjustTextPosition(byCharacterOffset: 1)
        }
      }

      // We've handled a set of code points; make sure we mark our progress!
      pointsToDelete -= pointsDeleted
    }
  }

  func menuKeyUp(_ keymanWeb: KeymanWebViewController) {
    if isSystemKeyboard {
      if keymanWeb.isKeyboardMenuVisible {
        return
      }

      switch globeKeyTapBehaviour {
      case .switchToNextKeyboard:
        if let nextIndex = Manager.shared.switchToNextKeyboard(), nextIndex <= 0 {
          advanceToNextInputMode()
        }
      case .switchToNextInputMethod:
        advanceToNextInputMode()
      case .doNothing:
        break
      }

      // If we allow the system keyboard to show no banners, this line is needed
      // for variable system keyboard height.
      updateShowBannerSetting()
    } else { // Use in-app keyboard behavior instead.
      if !(Manager.shared.currentResponder?.showKeyboardPicker() ?? false) {
        _ = Manager.shared.switchToNextKeyboard
      }
    }
  }

  // Needed due to protection level on the `keymanWeb` property
  func updateShowBannerSetting() {
    keymanWeb.updateShowBannerSetting()
  }

  func menuKeyHeld(_ keymanWeb: KeymanWebViewController) {
    if isSystemKeyboard {
      switch menuBehaviour {
      case .showAlways,
           .showIfMultipleKeyboards where keyboardListCount > 1:
        keymanWeb.showKeyboardMenu(self, closeButtonTitle: menuCloseButtonTitle)
      case .showIfMultipleKeyboards, // keyboardListCount() <= 1
      .showNever:
        break
      }
    }
  }

  // These require the view to appear - parent and our relationship with it must exist!
  private func setOuterConstraints() {
    var baseWidthConstraint: NSLayoutConstraint
    if #available(iOSApplicationExtension 11.0, *) {
      baseWidthConstraint = self.inputView!.widthAnchor.constraint(equalTo: parent!.view.safeAreaLayoutGuide.widthAnchor)
    } else {
      //baseWidthConstraint = self.inputView!.widthAnchor.constraint(equalTo: parent!.view.layoutMarginsGuide.widthAnchor)
      baseWidthConstraint = self.inputView!.widthAnchor.constraint(equalTo: parent!.view.widthAnchor)
    }
    baseWidthConstraint.priority = UILayoutPriority(rawValue: 999)
    baseWidthConstraint.isActive = true
  }

  public var isTopBarActive: Bool {
    let userData = Storage.active.userDefaults
    let alwaysShow = userData.bool(forKey: Key.optShouldShowBanner)

    if alwaysShow || Manager.shared.isSystemKeyboard || keymanWeb.activeModel {
      return true
    }

    return false
  }

  public var activeTopBarHeight: CGFloat {
    // If 'isSystemKeyboard' is true, always show the top bar.
    return isTopBarActive ? CGFloat(InputViewController.topBarHeight) : 0
  }
  
  public var kmwHeight: CGFloat {
    return keymanWeb.keyboardHeight
  }

  func clearModel() {
    keymanWeb.activeModel = false
  }

  private func setInnerConstraints() {
    let iv = self.inputView as! CustomInputView
    iv.setConstraints()

    self.updateViewConstraints()
    fixLayout()
  }
  
  func fixLayout() {
    view.setNeedsLayout()
    view.layoutIfNeeded()
  }
  
  open override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
    super.viewWillTransition(to: size, with: coordinator)

    coordinator.animateAlongsideTransition(in: nil, animation: {
      _ in
        self.updateViewConstraints()
        self.fixLayout()
    }, completion: {
      _ in
        self.updateViewConstraints()
        self.fixLayout()
    })
  }

  @objc func enableInputClickSound() {
    isInputClickSoundEnabled = true
  }

  private class func isSurrogate(_ c: unichar) -> Bool {
    return UTF16.isLeadSurrogate(c) || UTF16.isTrailSurrogate(c)
  }

  // KeymanWebViewController maintenance methods
  func reload() {
    keymanWeb.reloadKeyboard()
  }
  
  func setKeyboard(_ kb: InstallableKeyboard) {
    keymanWeb.setKeyboard(kb)
  }
    
  func registerLexicalModel(_ lm: InstallableLexicalModel) {
    keymanWeb.registerLexicalModel(lm)
  }
  
  func deregisterLexicalModel(_ lm: InstallableLexicalModel) {
    keymanWeb.deregisterLexicalModel(lm)
  }
  
  func showHelpBubble() {
    keymanWeb.showHelpBubble()
  }
  
  func showHelpBubble(afterDelay delay: TimeInterval) {
    keymanWeb.showHelpBubble(afterDelay: delay)
  }

  func clearText() {
    setContextState(text: nil, range: NSRange(location: 0, length: 0))
    keymanWeb.resetContext()
    log.info("Cleared text.")
  }
  
  func resetContext() {
    if self.swallowContextChangeCount <= 0 {
      keymanWeb.resetContext()
    }
  }

  internal func setSentryState(enabled: Bool) {
    keymanWeb.setSentryState(enabled: enabled)
  }
 
  func setContextState(text: String?, range: NSRange) {
    // Check for any LTR or RTL marks at the context's start; if they exist, we should
    // offset the selection range.
    let characterOrderingChecks = [ "\u{200e}" /*LTR*/, "\u{202e}" /*RTL 1*/, "\u{200f}" /*RTL 2*/ ]
    var offsetPrefix = false;
    
    let context = text ?? ""
    
    for codepoint in characterOrderingChecks {
      if(context.hasPrefix(codepoint)) {
        offsetPrefix = true;
        break;
      }
    }
    
    var selRange = range;
    if(offsetPrefix) { // If we have a character ordering mark, offset range location to hide it.
      selRange = NSRange(location: selRange.location - 1, length: selRange.length)
    }
    
    keymanWeb.setText(context)
    if range.location != NSNotFound {
      keymanWeb.setCursorRange(selRange)
    }
  }
  
  func resetKeyboardState() {
    keymanWeb.resetKeyboardState()
  }

  func endEditing(_ force: Bool) {
    keymanWeb.view.endEditing(force)
  }
  
  func dismissKeyboardMenu() {
    keymanWeb.dismissKeyboardMenu()
  }
  
  open func setBannerImage(to path: String) {
    keymanWeb.setBannerImage(to: path)
  }
}
