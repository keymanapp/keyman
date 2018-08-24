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

  var kmInputView: UIView! {
    return Manager.shared.keymanWeb.view
  }

  open var topBarImageView: UIImageView?
  var barHeightConstraints: [NSLayoutConstraint] = []
  var barWidthConstraints: [NSLayoutConstraint] = []
  var containerView: UIView?
  var containerHeightConstraints: [NSLayoutConstraint] = []
  var containerWidthConstraints: [NSLayoutConstraint] = []
  var heightConstraint: NSLayoutConstraint!
  var isTopBarEnabled: Bool

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
    return Manager.shared.keyboardHeight +
      (isTopBarEnabled ? CGFloat(InputViewController.topBarHeight) : 0)
  }

  public override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    isTopBarEnabled = Manager.shared.isSystemKeyboardTopBarEnabled
    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
  }

  public required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  open override func updateViewConstraints() {
    func addConstraints(_ constraints: [NSLayoutConstraint]) {
      for constraint in constraints {
        if !view.constraints.contains(constraint) {
          view.addConstraint(constraint)
        }
      }
    }

    Manager.shared.updateViewConstraints()

    let topBarHeight = isTopBarEnabled ? CGFloat(InputViewController.topBarHeight) : 0
    barHeightConstraints[0].constant = topBarHeight
    addConstraints(barHeightConstraints)

    barWidthConstraints[0].constant = Manager.shared.keyboardWidth
    addConstraints(barWidthConstraints)

    containerHeightConstraints[0].constant = Manager.shared.keyboardHeight
    addConstraints(containerHeightConstraints)

    containerWidthConstraints[0].constant = Manager.shared.keyboardWidth
    addConstraints(containerWidthConstraints)

    heightConstraint.constant = expandedHeight
    if !view.constraints.contains(heightConstraint) {
      view.addConstraint(heightConstraint)
    }

    // After superview is resized, tell the
    // manager to resize the keyboard view again
    // because it loses its size?
    Manager.shared.updateViewConstraints()

    super.updateViewConstraints()
  }

  open override func viewDidLoad() {
    super.viewDidLoad()

    let bgColor = UIColor(red: 210.0 / 255.0, green: 214.0 / 255.0, blue: 220.0 / 255.0, alpha: 1.0)
    view.backgroundColor = bgColor

    // TODO: If the following line is enabled, the WKWebView does not respond to touch events
    // Can figure out why one day maybe
    //view.translatesAutoresizingMaskIntoConstraints = false

    Manager.shared.inputViewDidLoad()
    Manager.shared.keymanWebDelegate = self

    topBarImageView?.removeFromSuperview()
    topBarImageView = UIImageView()
    topBarImageView!.translatesAutoresizingMaskIntoConstraints = false
    topBarImageView!.backgroundColor = UIColor.gray
    view.addSubview(topBarImageView!)

    containerView?.subviews.forEach { $0.removeFromSuperview() }
    containerView?.removeFromSuperview()
    containerView = UIView()
    containerView!.translatesAutoresizingMaskIntoConstraints = false
    containerView!.backgroundColor = bgColor
    containerView!.addSubview(kmInputView)
    view.addSubview(containerView!)
  }

  open override func viewDidAppear(_ animated: Bool) {
    Manager.shared.isSystemKeyboard = true
    super.viewDidAppear(animated)
    setConstraints()
    inputView?.setNeedsUpdateConstraints()

    //TODO: find out why this is actually happening
    if let containerView = self.containerView {
      if containerView.subviews.isEmpty {
        Manager.shared.keymanWebDelegate = self
        containerView.addSubview(kmInputView)
      }
    }
  }

  open override func viewWillDisappear(_ animated: Bool) {
    Manager.shared.isSystemKeyboard = false
    super.viewWillDisappear(animated)
  }

  open override func willRotate(to toInterfaceOrientation: UIInterfaceOrientation, duration: TimeInterval) {
    Manager.shared.inputViewWillRotate(to: toInterfaceOrientation, duration: duration)
    super.willRotate(to: toInterfaceOrientation, duration: duration)
  }

  open override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
    super.viewWillTransition(to: size, with: coordinator)
    coordinator.animateAlongsideTransition(in: nil, animation: nil, completion: {
      _ in
      self.updateViewConstraints()
    })
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
    if Manager.shared.isSubKeysMenuVisible {
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
    if Manager.shared.isKeyboardMenuVisible {
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
      Manager.shared.showKeyboardMenu(self, closeButtonTitle: menuCloseButtonTitle)
    case .showIfMultipleKeyboards, // keyboardListCount() <= 1
    .showNever:
      break
    }
  }

  func hideKeyboard(_ keymanWeb: KeymanWebViewController) {
    dismissKeyboard()
  }

  private func setConstraints() {
    let topBarHeight = isTopBarEnabled ? InputViewController.topBarHeight : 0
    let viewsDict = ["bar": topBarImageView!, "container": containerView!]
    let screenWidth = UIScreen.main.bounds.width

    barHeightConstraints = NSLayoutConstraint.constraints(
      withVisualFormat: "V:[bar(\(topBarHeight))]", metrics: nil, views: viewsDict)
    barWidthConstraints = NSLayoutConstraint.constraints(
      withVisualFormat: "H:[bar(\(Int(screenWidth)))]", metrics: nil, views: viewsDict)

    let barVerticalPositionConstraints = NSLayoutConstraint.constraints(
      withVisualFormat: "V:|-0-[bar]", metrics: nil, views: viewsDict)
    let barHorizontalPositionConstraints = NSLayoutConstraint.constraints(
      withVisualFormat: "H:|-0-[bar]", metrics: nil, views: viewsDict)

    view.addConstraints(barHeightConstraints)
    view.addConstraints(barWidthConstraints)
    view.addConstraints(barVerticalPositionConstraints)
    view.addConstraints(barHorizontalPositionConstraints)

    containerHeightConstraints = NSLayoutConstraint.constraints(
      withVisualFormat: "V:[container(\(Manager.shared.keyboardHeight))]",
      metrics: nil, views: viewsDict)

    containerWidthConstraints = NSLayoutConstraint.constraints(
      withVisualFormat: "H:[container(\(UInt(screenWidth)))]", metrics: nil, views: viewsDict)
    let containerVericalPositionConstraints = NSLayoutConstraint.constraints(
      withVisualFormat: "V:[container]-0-|", metrics: nil, views: viewsDict)
    let containerHorizontalPositionConstraints = NSLayoutConstraint.constraints(
      withVisualFormat: "H:|-0-[container]", metrics: nil, views: viewsDict)

    view.addConstraints(containerHeightConstraints)
    view.addConstraints(containerWidthConstraints)
    view.addConstraints(containerVericalPositionConstraints)
    view.addConstraints(containerHorizontalPositionConstraints)

    heightConstraint = NSLayoutConstraint(item: view,
                                          attribute: .height,
                                          relatedBy: .equal,
                                          toItem: nil,
                                          attribute: .notAnAttribute,
                                          multiplier: 0.0,
                                          constant: expandedHeight)
    heightConstraint.priority = UILayoutPriority(rawValue: 999)
  }

  @objc func enableInputClickSound() {
    isInputClickSoundEnabled = true
  }

  private class func isSurrogate(_ c: unichar) -> Bool {
    return UTF16.isLeadSurrogate(c) || UTF16.isTrailSurrogate(c)
  }
}
