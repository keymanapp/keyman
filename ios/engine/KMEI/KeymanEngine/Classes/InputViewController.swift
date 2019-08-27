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

open class InputViewController: UIInputViewController, KeymanWebDelegate {
  var menuCloseButtonTitle: String?
  var isInputClickSoundEnabled = true
  var globeKeyTapBehaviour = GlobeKeyTapBehaviour.switchToNextKeyboard
  var menuBehaviour = MenuBehaviour.showAlways

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

  open class var isPortrait: Bool {
    return UIScreen.main.bounds.width < UIScreen.main.bounds.height
  }

  open class var topBarHeight: Int {
    if InputViewController.isPortrait {
      return 41
    }
    return UIDevice.current.userInterfaceIdiom == .phone ? 34 : 39
  }

  open override var hasFullAccess: Bool {
    return Storage.shared != nil
  }

  private var keyboardListCount: Int {
    return Storage.active.userDefaults.userKeyboards?.count ?? 0
  }

  private var expandedHeight: CGFloat {
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
    
    addChildViewController(keymanWeb)
  }

  public required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  open override func updateViewConstraints() {
    resetKeyboardState()

    // Activate / deactivate layout-specific constraints.
    if InputViewController.isPortrait {
      landscapeConstraint?.isActive = false
      portraitConstraint?.isActive = true
    } else {
      portraitConstraint?.isActive = false
      landscapeConstraint?.isActive = true
    }
    
    keymanWeb.setBannerHeight(to: InputViewController.topBarHeight)

    super.updateViewConstraints()
  }
  
  open override func loadView() {
    let bgColor = UIColor(red: 210.0 / 255.0, green: 214.0 / 255.0, blue: 220.0 / 255.0, alpha: 1.0)
    let baseView = UIInputView(frame: CGRect.zero, inputViewStyle: .keyboard)

    baseView.backgroundColor = bgColor

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
      if Manager.shared.currentKeyboardID != nil {
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
    // Just seeing if it's actually called.
    super.viewWillAppear(animated)
  }

  open override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)

    // When using the system keyboard, sets the system-initialized version of the keyboard
    // as Manager.shared's inputViewController.
    Manager.shared.inputViewController = self

    setOuterConstraints()
    inputView?.setNeedsUpdateConstraints()
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
  
  func insertText(_ keymanWeb: KeymanWebViewController, numCharsToDelete: Int, newText: String) {
    if keymanWeb.isSubKeysMenuVisible {
      return
    }

    if isInputClickSoundEnabled {
      DispatchQueue.global(qos: .default).async { AudioServicesPlaySystemSound(0x450) }

      // Disable input click sound for 0.1 second to ensure it plays for single key stroke.
      isInputClickSoundEnabled = false
      perform(#selector(self.enableInputClickSound), with: nil, afterDelay: 0.1)
    }

    if numCharsToDelete <= 0 {
      textDocumentProxy.insertText(newText)
      return
    }

    for _ in 0..<numCharsToDelete {
      let oldContext = textDocumentProxy.documentContextBeforeInput ?? ""
      textDocumentProxy.deleteBackward()
      let newContext = textDocumentProxy.documentContextBeforeInput ?? ""
      let unitsDeleted = oldContext.utf16.count - newContext.utf16.count
      if unitsDeleted > 1 {
        if !InputViewController.isSurrogate(oldContext.utf16.last!) {
          let lowerIndex = oldContext.utf16.index(oldContext.utf16.startIndex,
                                                  offsetBy: newContext.utf16.count)
          let upperIndex = oldContext.utf16.index(lowerIndex, offsetBy: unitsDeleted - 1)
          textDocumentProxy.insertText(String(oldContext[lowerIndex..<upperIndex]))
        }
      }
      
      if textDocumentProxy.documentContextBeforeInput == nil {
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

    if !newText.isEmpty {
      textDocumentProxy.insertText(newText)
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
    } else { // Use in-app keyboard behavior instead.
      if !(Manager.shared.currentResponder?.showKeyboardPicker() ?? false) {
        _ = Manager.shared.switchToNextKeyboard
      }
    }
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

  public var activeTopBarHeight: CGFloat {
    // If 'isSystemKeyboard' is true, always show the top bar.
    return isSystemKeyboard ? CGFloat(InputViewController.topBarHeight) : 0
  }
  
  public var kmwHeight: CGFloat {
    return keymanWeb.keyboardHeight
  }

  private func setInnerConstraints() {
    let container = keymanWeb.view!

    if #available(iOSApplicationExtension 11.0, *) {
      container.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor).isActive = true
      container.bottomAnchor.constraint(equalTo:view.safeAreaLayoutGuide.bottomAnchor).isActive = true
      container.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor).isActive = true
      container.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor).isActive = true

      // Allow these to be broken if/as necessary to resolve layout issues.
      let kbdWidthConstraint = container.widthAnchor.constraint(equalTo: view.safeAreaLayoutGuide.widthAnchor)
      kbdWidthConstraint.priority = .defaultHigh
      kbdWidthConstraint.isActive = true
    } else {
      // Fallback on earlier versions
      container.topAnchor.constraint(equalTo:view.layoutMarginsGuide.topAnchor).isActive = true
      container.bottomAnchor.constraint(equalTo:view.layoutMarginsGuide.bottomAnchor).isActive = true
      container.leftAnchor.constraint(equalTo:view.layoutMarginsGuide.leftAnchor).isActive = true
      container.rightAnchor.constraint(equalTo:view.layoutMarginsGuide.rightAnchor).isActive = true

      // Allow these to be broken if/as necessary to resolve layout issues.
      let kbdWidthConstraint = container.widthAnchor.constraint(equalTo: view.layoutMarginsGuide.widthAnchor)
      kbdWidthConstraint.priority = .defaultHigh
      kbdWidthConstraint.isActive = true
    }
    
    // Cannot be met by the in-app keyboard, but helps to 'force' height for the system keyboard.
    let portraitHeight = container.heightAnchor.constraint(equalToConstant: keymanWeb.constraintTargetHeight(isPortrait: true))
    portraitHeight.priority = .defaultHigh
    let landscapeHeight = container.heightAnchor.constraint(equalToConstant: keymanWeb.constraintTargetHeight(isPortrait: false))
    landscapeHeight.priority = .defaultHigh

    portraitConstraint = portraitHeight
    landscapeConstraint = landscapeHeight
    // .isActive will be set according to the current portrait/landscape perspective.

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
    keymanWeb.resetContext()
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
