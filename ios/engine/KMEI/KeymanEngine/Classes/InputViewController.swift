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

  open var topBarImageView: UIImageView?

  var isTopBarEnabled: Bool
  var keymanWeb: KeymanWebViewController

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
    return keymanWeb.keyboardHeight +
      (isTopBarEnabled ? CGFloat(InputViewController.topBarHeight) : 0)
  }

  public override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    // Fix:  `isTopBarEnabled` was initialized here from Manager.shared, but that reference no longer exists here.
    isTopBarEnabled = true
    keymanWeb = KeymanWebViewController(storage: Storage.active)
    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
    
    addChildViewController(keymanWeb)
  }

  public required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  open override func updateViewConstraints() {
    resetKeyboardState()
    super.updateViewConstraints()
  }
  
  open override func loadView() {
    let bgColor = UIColor(red: 210.0 / 255.0, green: 214.0 / 255.0, blue: 220.0 / 255.0, alpha: 1.0)

    let baseView = UIView()
    baseView.backgroundColor = bgColor

    // TODO: If the following line is enabled, the WKWebView does not respond to touch events
    // Can figure out why one day maybe
    baseView.translatesAutoresizingMaskIntoConstraints = false
    baseView.autoresizingMask = UIViewAutoresizing.flexibleHeight.union(.flexibleWidth)

    keymanWeb.delegate = self

    // Fixes debugging issue - views added later are moved to the front.
    topBarImageView?.removeFromSuperview()
    topBarImageView = UIImageView()
    topBarImageView!.translatesAutoresizingMaskIntoConstraints = false
    topBarImageView!.backgroundColor = UIColor.gray
    baseView.addSubview(topBarImageView!)

    baseView.addSubview(keymanWeb.view)

    view = baseView
  }

  open override func viewDidLoad() {
    super.viewDidLoad()

    keymanWeb.resetKeyboardState()
    setConstraints()

    let activeUserDef = Storage.active.userDefaults
    let standardUserDef = UserDefaults.standard
    let activeDate = (activeUserDef.object(forKey: Key.synchronizeSWKeyboard) as? [Date])?[safe: 0]
    let standardDate = (standardUserDef.object(forKey: Key.synchronizeSWKeyboard) as? [Date])?[safe: 0]

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
    //Manager.shared.isSystemKeyboard = true
    super.viewDidAppear(animated)
    setConstraints()
    inputView?.setNeedsUpdateConstraints()
  }

  open override func viewWillDisappear(_ animated: Bool) {
    Manager.shared.isSystemKeyboard = false
    super.viewWillDisappear(animated)
  }

  open override func textDidChange(_ textInput: UITextInput?) {
    let contextBeforeInput = textDocumentProxy.documentContextBeforeInput ?? ""
    let contextAfterInput = textDocumentProxy.documentContextAfterInput ?? ""
    let context = "\(contextBeforeInput)\(contextAfterInput)"

    let newRange: Range<String.Index>
    if let range = context.range(of: contextBeforeInput) {
      newRange = range.upperBound..<range.upperBound
    } else {
      newRange = context.startIndex..<context.startIndex
    }

    Manager.shared.setText(context)
    Manager.shared.setSelectionRange(NSRange(newRange, in: context), manually: false)
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
    }

    if !newText.isEmpty {
      textDocumentProxy.insertText(newText)
    }
  }

  func menuKeyUp(_ keymanWeb: KeymanWebViewController) {
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
  }

  func menuKeyHeld(_ keymanWeb: KeymanWebViewController) {
    switch menuBehaviour {
    case .showAlways,
         .showIfMultipleKeyboards where keyboardListCount > 1:
      keymanWeb.showKeyboardMenu(self, closeButtonTitle: menuCloseButtonTitle)
    case .showIfMultipleKeyboards, // keyboardListCount() <= 1
    .showNever:
      break
    }
  }

  func hideKeyboard(_ keymanWeb: KeymanWebViewController) {
    // Is not implemented for this class, nor is resumeKeyboard().
    // This class is used for System-level keyboards and would not have a
    // practical way to be resumed after dismissal.
    dismissKeyboard()
  }

  private func setConstraints() {
    // Top Bar temporarily perma-set here for debugging during development on the big keyboard refactor.
    isTopBarEnabled = true
    //isTopBarEnabled = false

    let topBarHeight = isTopBarEnabled ? InputViewController.topBarHeight : 0

    let topBar = topBarImageView!
    let container = keymanWeb.view!
    
    // Establish a consistent set of constraints for the top bar.
    if #available(iOSApplicationExtension 11.0, *) {
      topBar.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor).isActive = true
      topBar.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor).isActive = true
      topBar.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor).isActive = true

      // Allow this one to be broken if/as necessary to resolve layout issues.
      let topBarWidthConstraint = topBar.widthAnchor.constraint(equalTo: view.safeAreaLayoutGuide.widthAnchor)
      topBarWidthConstraint.priority = UILayoutPriority(rawValue: 999)
      topBarWidthConstraint.isActive = true
    } else {
      topBar.topAnchor.constraint(equalTo: view.layoutMarginsGuide.topAnchor).isActive = true
      topBar.leftAnchor.constraint(equalTo: view.layoutMarginsGuide.leftAnchor).isActive = true
      topBar.rightAnchor.constraint(equalTo: view.layoutMarginsGuide.rightAnchor).isActive = true
      
      // Allow this one to be broken if/as necessary to resolve layout issues.
      let topBarWidthConstraint = topBar.widthAnchor.constraint(equalTo: view.layoutMarginsGuide.widthAnchor)
      topBarWidthConstraint.priority = UILayoutPriority(rawValue: 999)
      topBarWidthConstraint.isActive = true
    }

    topBar.heightAnchor.constraint(equalToConstant: CGFloat(topBarHeight)).isActive = true

    // Establishes a set of constraints for the keyboard's container, supporting autoresizing of
    // the keyboard's WebView via its constraints.
    container.topAnchor.constraint(equalTo:topBar.bottomAnchor).isActive = true

    if #available(iOSApplicationExtension 11.0, *) {
      container.bottomAnchor.constraint(equalTo:view.safeAreaLayoutGuide.bottomAnchor).isActive = true
      container.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor).isActive = true
      container.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor).isActive = true

      // Allow these to be broken if/as necessary to resolve layout issues.
      let kbdWidthConstraint = container.widthAnchor.constraint(equalTo: view.safeAreaLayoutGuide.widthAnchor)
      kbdWidthConstraint.priority = UILayoutPriority(rawValue: 999)
      kbdWidthConstraint.isActive = true

      let kbdHeightConstraint = container.heightAnchor.constraint(equalToConstant: CGFloat(200))//container.heightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.heightAnchor)
      // Cannot be met, but helps to 'force' height for the system keyboard.
      kbdHeightConstraint.priority = UILayoutPriority(rawValue: 999)
      kbdHeightConstraint.isActive = true;
    } else {
      // Fallback on earlier versions
      container.bottomAnchor.constraint(equalTo:view.layoutMarginsGuide.bottomAnchor).isActive = true
      container.leftAnchor.constraint(equalTo:view.layoutMarginsGuide.leftAnchor).isActive = true
      container.rightAnchor.constraint(equalTo:view.layoutMarginsGuide.rightAnchor).isActive = true

      // Allow these to be broken if/as necessary to resolve layout issues.
      let kbdWidthConstraint = container.widthAnchor.constraint(equalTo: view.layoutMarginsGuide.widthAnchor)
      kbdWidthConstraint.priority = UILayoutPriority(rawValue: 999)
      kbdWidthConstraint.isActive = true

      let kbdHeightConstraint = container.heightAnchor.constraint(equalToConstant: CGFloat(200))//container.heightAnchor.constraint(equalTo: view.layoutMarginsGuide.heightAnchor)
      // Cannot be met, but helps to 'force' height for the system keyboard.
      kbdHeightConstraint.priority = UILayoutPriority(rawValue: 999)
      kbdHeightConstraint.isActive = true;
    }

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
        self.fixLayout()
    }, completion: {
      _ in
        self.fixLayout()
        self.updateViewConstraints()
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
  
  func showHelpBubble() {
    keymanWeb.showHelpBubble()
  }
  
  func showHelpBubble(afterDelay delay: TimeInterval) {
    keymanWeb.showHelpBubble(afterDelay: delay)
  }
  
  func setCursorRange(_ range: NSRange) {
    keymanWeb.setCursorRange(range)
  }
  
  func setText(_ text: String?) {
    keymanWeb.setText(text)
  }
  
  func resetKeyboardState() {
    keymanWeb.resetKeyboardState()
  }
}
