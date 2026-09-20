//
//  InputViewController.swift
//  KMEI
//
//  Created by Gabriel Wong on 2017-09-29.
//  Copyright © 2017 SIL International. All rights reserved.
//

import AudioToolbox
import UIKit
import os.log

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

  public func destroy() {
    // In app-extension mode, there are scenarios in which this class does not properly
    // deallocate! We need to help that process along. In particular, doing this allows
    // us to guarantee that the WebView is allowed to be GC'd, even when Apple fails to GC
    // this (`CustomInputView`) instance - which actually happens. (Refer to #12216.)
    keymanWeb.removeFromParent()
    keymanWeb.destroy()
    keymanWeb = nil
  }

  public var enableInputClicksWhenVisible: Bool {
    get {
      // Implemented as noted by https://developer.apple.com/documentation/uikit/uidevice/1620050-playinputclick.
      return true
    }
  }

  override var intrinsicContentSize: CGSize {
    /*
     * This function is the motivating reason for this class to exist as-is. If we return the default value
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
    os_log("CustomInputView setConstraints", log: KeymanEngineLogger.ui, type: .info)
    let innerView = keymanWeb.view!
    let guide = self.safeAreaLayoutGuide

    // Fallback on earlier versions
    innerView.topAnchor.constraint(equalTo:    guide.topAnchor).isActive = true
    innerView.bottomAnchor.constraint(equalTo: guide.bottomAnchor).isActive = true

    innerView.leftAnchor.constraint(equalTo:   guide.leftAnchor).isActive = true
    innerView.rightAnchor.constraint(equalTo:  guide.rightAnchor).isActive = true

    // Allow these to be broken if/as necessary to resolve layout issues.
    let kbdWidthConstraint = innerView.widthAnchor.constraint(equalTo: guide.widthAnchor)

    kbdWidthConstraint.priority = .defaultHigh
    kbdWidthConstraint.isActive = true

    self.buildKeyboardHeightConstraints(bannerHeight: InputViewController.topBarHeight)
  }

  /**
   * Due to new custom keyboard height as chosen by the user.
   * The value for the new keyboard height originates from KeyboardHeightViewController.
   */
  func keyboardHeightChanged() {
    os_log("CustomInputView keyboardHeightChanged", log: KeymanEngineLogger.ui, type: .info)

    // deactivate constraints for both orientations (though one should already be inactive)
    landscapeConstraint?.isActive = false
    portraitConstraint?.isActive = false

    // rebuild both portrait and landscape constraints
    self.buildKeyboardHeightConstraints(bannerHeight: InputViewController.topBarHeight)

    // activate constraints for the current orientation
    if InputViewController.isPortrait {
      portraitConstraint?.isActive = true
    } else {
      landscapeConstraint?.isActive = true
    }

    self.setNeedsLayout()
  }

  private func buildKeyboardHeightConstraints(bannerHeight: CGFloat) {
    os_log("CustomInputView buildKeyboardHeightConstraints", log: KeymanEngineLogger.ui, type: .info)
    let innerView = keymanWeb.view!

    // Cannot be met by the in-app keyboard, but helps to 'force' height for the system keyboard.
    let portraitHeightConstraint = innerView.heightAnchor.constraint(equalToConstant: bannerHeight + keymanWeb.readKeyboardHeight(isPortrait: true)!)
    portraitHeightConstraint.identifier = "Height constraint for portrait mode"
    portraitHeightConstraint.priority = .defaultHigh

    let landscapeHeightConstraint = innerView.heightAnchor.constraint(equalToConstant: bannerHeight + keymanWeb.readKeyboardHeight(isPortrait: false)!)
    landscapeHeightConstraint.identifier = "Height constraint for landscape mode"
    landscapeHeightConstraint.priority = .defaultHigh

    portraitConstraint = portraitHeightConstraint
    landscapeConstraint = landscapeHeightConstraint
    // .isActive will be set according to the current portrait/landscape perspective.
  }

  override func updateConstraints() {
    super.updateConstraints()

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
    return _isSystemKeyboard
  }

  // Constraints dependent upon the device's current rotation state.
  // For now, should be mostly upon keymanWeb.view.heightAnchor.
  var portraitConstraint: NSLayoutConstraint?
  var landscapeConstraint: NSLayoutConstraint?

  var outerWidthConstraint: NSLayoutConstraint?

  private var keymanWeb: KeymanWebViewController
  private var swallowBackspaceTextChange: Bool = false

  open class var isPortrait: Bool {
    return UIScreen.main.bounds.width < UIScreen.main.bounds.height
  }

  open class var topBarHeight: CGFloat {
    let scaling = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: self.isPortrait)

    return scaling?.bannerHeight ?? 38 // default for iPhone SE, older/smaller devices
  }

  private var keyboardListCount: Int {
    let activeUserDef = Storage.active.userDefaults
    return activeUserDef.userKeyboards?.count ?? 0
  }

  var expandedHeight: CGFloat {
    return keymanWeb.keyboardSize.height + InputViewController.topBarHeight
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

    let message = self.hasFullAccess ? "hasFullAccess: true" : "hasFullAccess: false"
    os_log("%{public}s", log: KeymanEngineLogger.settings, type: .default, message)
    SentryManager.breadcrumb(message)

    addChild(keymanWeb)
  }

  public required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  deinit {
    inputView?.removeFromSuperview()
    (inputView as? CustomInputView)?.destroy()
    inputView = nil
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
      if Manager.shared.currentKeyboardID != nil || keymanWeb.shouldReload {
        keymanWeb.shouldReload = true
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
    self.reloadIfNeeded()
  }

  open override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)

    // When using the system keyboard, sets the system-initialized version of the keyboard
    // as Manager.shared.inputViewController.
    Manager.shared.inputViewController = self

    setOuterConstraints()
    inputView?.setNeedsUpdateConstraints()

    keymanWeb.verifyLoaded()
  }

  open override func viewWillDisappear(_ animated: Bool) {
    super.viewWillDisappear(animated)
    // Necessary for existing infrastructure to resend info for the keyboard after reloading
    // as system keyboard. Do NOT perform if in-app, as this unnecessarily resets the WebView.
    if(Manager.shared.isSystemKeyboard) {
      keymanWeb.shouldReload = true
    }
  }

  open override func viewDidDisappear(_ animated: Bool) {
    super.viewDidDisappear(animated)

    if outerWidthConstraint != nil {
      outerWidthConstraint?.isActive = false
      self.inputView?.removeConstraint(self.outerWidthConstraint!)
      outerWidthConstraint = nil
    }
  }

  open override func textDidChange(_ textInput: UITextInput?) {
    // Swallows self-triggered calls from emptying the context due to keyboard rules
    if self.swallowBackspaceTextChange && textDocumentProxy.documentContextBeforeInput == nil {
      self.swallowBackspaceTextChange = false
      return
    }

    // Apparently, in system-keyboard mode, this is also self-triggered if we erase back to a newline.
    // Refer to https://github.com/keymanapp/keyman/pull/2770 for context.
    if self.swallowBackspaceTextChange && Manager.shared.isSystemKeyboard && textDocumentProxy.documentContextBeforeInput == "\n" {
      self.swallowBackspaceTextChange = false
      return
    }

    let contextBeforeInput = textDocumentProxy.documentContextBeforeInput ?? ""
    let selection = textDocumentProxy.selectedText ?? ""
    let contextAfterInput = textDocumentProxy.documentContextAfterInput ?? ""
    let context = "\(contextBeforeInput)\(selection)\(contextAfterInput)"
    let bLength = contextBeforeInput.unicodeScalars.count
    let sLength = selection.unicodeScalars.count
    setContextState(text: context, range: NSMakeRange(bLength, sLength))
    // Within the app, this is triggered after every keyboard input.
    // We should NOT call .resetContext() here for this reason.
  }

  // Pre-condition: no text is selected. As this is currently only called by `insertText`
  // below, this condition is met.
  func sendContextUpdate() {
    let preCaretContext = textDocumentProxy.documentContextBeforeInput ?? ""
    let postCaretContext = textDocumentProxy.documentContextAfterInput ?? ""

    let updater = { (_ before: String, _ after: String) -> Void in
      let contextWindowText = "\(before)\(after)"

      let range = NSRange(location: before.unicodeScalars.count, length: 0)

      self.setContextState(text: contextWindowText, range: range, doSync: true)
    }

    updater(preCaretContext, postCaretContext)

    if preCaretContext == "" {
      DispatchQueue.main.asyncAfter(deadline: .now() + 0.033) {
        let preCaretAsyncContext = self.textDocumentProxy.documentContextBeforeInput ?? ""
        let postCaretAsyncContext = self.textDocumentProxy.documentContextAfterInput ?? ""

        updater(preCaretAsyncContext, postCaretAsyncContext)
      }
    }
  }

  func deleteSelection() -> Bool {
    if let selected = textDocumentProxy.selectedText, selected.count > 0 {
      let beforeManipulation = textDocumentProxy.documentContextBeforeInput ?? ""

      textDocumentProxy.insertText("\u{200c}")
      textDocumentProxy.deleteBackward()

      let afterManipulation = textDocumentProxy.documentContextBeforeInput ?? ""

      if beforeManipulation != afterManipulation {
        os_log(.error, log: KeymanEngineLogger.engine, "Could not cleanly execute backspace for selected text")
      }

      return true
    }
    return false
  }

  func insertText(_ keymanWeb: KeymanWebViewController, numCharsToDelete: Int, newText: String) {
    if isInputClickSoundEnabled {
      UIDevice.current.playInputClick()

      isInputClickSoundEnabled = false
      perform(#selector(self.enableInputClickSound), with: nil, afterDelay: 0.1)
    }

    let deletedSelection = self.deleteSelection()

    if numCharsToDelete <= 0 || deletedSelection {
      textDocumentProxy.insertText(newText)
      sendContextUpdate()
      return
    }

    if numCharsToDelete > 0 && textDocumentProxy.documentContextBeforeInput == nil {
      textDocumentProxy.deleteBackward()
      sendContextUpdate()
      return
    }

    for _ in 0..<numCharsToDelete {
      let oldContext = textDocumentProxy.documentContextBeforeInput ?? ""
      textDocumentProxy.deleteBackward()
      let newContext = textDocumentProxy.documentContextBeforeInput ?? ""
      let unitsDeleted = oldContext.utf16.count - newContext.utf16.count
      let unitsInPoint = InputViewController.isSurrogate(oldContext.utf16.last ?? 0) ? 2 : 1

      if unitsDeleted > unitsInPoint {
        let lowerIndex = oldContext.utf16.index(oldContext.utf16.startIndex,
                                                offsetBy: newContext.utf16.count)
        let upperIndex = oldContext.utf16.index(lowerIndex, offsetBy: unitsDeleted - unitsInPoint)
        textDocumentProxy.insertText(String(oldContext[lowerIndex..<upperIndex]))
      }

      if textDocumentProxy.documentContextBeforeInput == nil ||
         (textDocumentProxy.documentContextBeforeInput == "\n" && Manager.shared.isSystemKeyboard) {
        if(self.swallowBackspaceTextChange) {
          os_log("Failed to swallow a recent textDidChange call!", log: KeymanEngineLogger.ui, type: .default)
        }
        self.swallowBackspaceTextChange = true
      }
    }

    if !newText.isEmpty {
      textDocumentProxy.insertText(newText)
    }

    sendContextUpdate()
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
    } else {
      if !(Manager.shared.currentResponder?.showKeyboardPicker() ?? false) {
        _ = Manager.shared.switchToNextKeyboard
      }
    }
  }

  func updateSpacebarText() {
    keymanWeb.updateSpacebarText()
  }

  func menuKeyHeld(_ keymanWeb: KeymanWebViewController) {
    if isSystemKeyboard {
      switch menuBehaviour {
      case .showAlways,
           .showIfMultipleKeyboards where keyboardListCount > 1:
        keymanWeb.showKeyboardMenu(self, closeButtonTitle: menuCloseButtonTitle)
      case .showIfMultipleKeyboards,
      .showNever:
        break
      }
    }
  }

  private func setOuterConstraints() {
    guard outerWidthConstraint == nil else {
      outerWidthConstraint!.isActive = true
      return
    }
    outerWidthConstraint = self.inputView!.widthAnchor.constraint(equalTo: parent!.view.safeAreaLayoutGuide.widthAnchor)
    outerWidthConstraint!.priority = UILayoutPriority(rawValue: 999)
    outerWidthConstraint!.isActive = true
  }

  public var kmwHeight: CGFloat {
    return keymanWeb.keyboardSize.height
  }

  func clearModel() {
    keymanWeb.activeModel = false
  }

  private func setInnerConstraints() {
    let customInputView = self.inputView as! CustomInputView
    customInputView.setConstraints()

    self.updateViewConstraints()
    fixLayout()
  }

  func keyboardHeightChanged() {
    os_log("InputViewController keyboardHeightChanged", log: KeymanEngineLogger.ui, type: .debug)
    if let customInputView = self.inputView as? CustomInputView {
      customInputView.keyboardHeightChanged()
    }
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
  public func reload() {
    keymanWeb.reloadKeyboard()
  }

  func reloadIfNeeded() {
    if keymanWeb.shouldReload {
      reload()
      keymanWeb.shouldReload = false
    }
  }

  func setKeyboard(_ kb: InstallableKeyboard) throws {
    try keymanWeb.setKeyboard(kb)
  }

  public func setShouldReload() {
    keymanWeb.shouldReload = true
  }

  internal var shouldReload: Bool {
    return keymanWeb.shouldReload
  }

  func registerLexicalModel(_ lm: InstallableLexicalModel) throws {
    try keymanWeb.registerLexicalModel(lm)
  }

  func deregisterLexicalModel(_ lm: InstallableLexicalModel) {
    keymanWeb.deregisterLexicalModel(lm)
  }

  func showHelpBubble() {
    keymanWeb.showHelpBubble()
  }

  func dismissHelpBubble() {
    keymanWeb.dismissHelpBubble()
  }

  func showHelpBubble(afterDelay delay: TimeInterval) {
    keymanWeb.showHelpBubble(afterDelay: delay)
  }

  internal func enforceKeyboardSize() {
    keymanWeb.resizeKeyboard()
  }

  func clearText() {
    setContextState(text: nil, range: NSRange(location: 0, length: 0))
    keymanWeb.resetContext()
    let message = "Cleared text."
    os_log("%{public}s", log: KeymanEngineLogger.ui, type: .info, message)
    SentryManager.breadcrumb(message)
  }

  func resetContext() {
    keymanWeb.resetContext()
  }

  internal func setSentryState(enabled: Bool) {
    keymanWeb.setSentryState(enabled: enabled)
  }

  func setContextState(text: String?, range: NSRange, doSync: Bool = false) {
    var offsetPrefix = false

    let context = trimDirectionalMarkPrefix(text)
    if context.count != (text?.count ?? 0) {
      offsetPrefix = true
    }

    var selRange = range
    if(offsetPrefix) {
      selRange = NSRange(location: selRange.location - 1, length: selRange.length)
    }

    keymanWeb.setContext(text: context, range: selRange, doSync: doSync)
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
