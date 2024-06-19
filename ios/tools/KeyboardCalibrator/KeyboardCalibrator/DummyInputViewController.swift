//
//  DummyInputViewController.swift
//  DefaultKeyboardHost
//
//  Created by Joshua Horton on 1/13/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit

private class CustomInputView: UIInputView {
  var height: CGFloat
  var inset: CGFloat
  var innerView: UIView!
  var insetView: UIView!

  var heightLabel: UILabel!
  var insetHeightLabel: UILabel!

  init(height: CGFloat, inset: CGFloat) {
    self.height = height
    self.inset = inset
    super.init(frame: CGRect.zero, inputViewStyle: .keyboard)
  }

  override var intrinsicContentSize: CGSize {
    get {
      return CGSize(width: UIScreen.main.bounds.width, height: height > 0 ? height + inset : 100)
    }
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  func setConstraints() {
    var guide: UILayoutGuide

    guide = self.safeAreaLayoutGuide

    if height != 0 {
      innerView.heightAnchor.constraint(equalToConstant: height).isActive = true
    } else {
      innerView.heightAnchor.constraint(equalTo: guide.heightAnchor).isActive = true
    }

    innerView.widthAnchor.constraint(equalTo: guide.widthAnchor).isActive = true
    innerView.leftAnchor.constraint(equalTo: guide.leftAnchor).isActive = true
    insetView.widthAnchor.constraint(equalTo: guide.widthAnchor).isActive = true

    innerView.bottomAnchor.constraint(equalTo: insetView.topAnchor).isActive = true

    insetView.heightAnchor.constraint(equalToConstant: inset).isActive = true
    insetView.bottomAnchor.constraint(equalTo: guide.bottomAnchor).isActive = true
  }

  func load() {
    innerView = UIView()
    innerView.backgroundColor = .red
    innerView.translatesAutoresizingMaskIntoConstraints = false

    heightLabel = UILabel(frame: CGRect(x: 0, y: 0, width: 200, height: 21))
    heightLabel.text = "Hello World"

    innerView.addSubview(heightLabel)

    self.addSubview(innerView)

    insetView = UIView()
    insetView.backgroundColor = .green
    insetView.translatesAutoresizingMaskIntoConstraints = false

    insetHeightLabel = UILabel(frame: CGRect(x: 0, y: 0, width: 200, height: 21))
    insetHeightLabel.text = "(0.0, \(inset))"

    insetView.addSubview(insetHeightLabel)

    self.addSubview(insetView)
  }

  override func layoutSubviews() {
    super.layoutSubviews()

    if innerView.bounds.size != CGSize.zero {
      heightLabel.text = "\(innerView.bounds.size)"
    } else {
      heightLabel.text = "\(intrinsicContentSize)"
    }
  }
}

class DummyInputViewController: UIInputViewController {
  var kbdHeight: CGFloat
  var insetHeight: CGFloat

  // The app group used to access common UserDefaults settings.
  static let appGroup = "group.horton.kmtesting"

  static var keyboardHeightDefault: CGFloat {
    get {
      let userDefaults = UserDefaults(suiteName: DummyInputViewController.appGroup)!
      return (userDefaults.object(forKey: "height") as? CGFloat) ?? 0
    }

    set(value) {
      let userDefaults = UserDefaults(suiteName: DummyInputViewController.appGroup)!
      userDefaults.set(value, forKey: "height")
    }
  }

  @IBOutlet var nextKeyboardButton: UIButton!

  var asSystemKeyboard: Bool = false

  // This is the one used to initialize the keyboard as the app extension, marking "system keyboard" mode.
  convenience init() {
    // Retrieve kbd height from app group!
    let userDefaults = UserDefaults(suiteName: DummyInputViewController.appGroup)!
    let height = userDefaults.float(forKey: "height")

    self.init(height: CGFloat(height))
    asSystemKeyboard = true
  }

  init(height: CGFloat, inset: CGFloat = 0) {
    kbdHeight = height
    insetHeight = inset

    super.init(nibName: nil, bundle: nil)
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  open override func loadView() {
    let baseView = CustomInputView(height: kbdHeight, inset: insetHeight)

    baseView.backgroundColor = .blue
    baseView.translatesAutoresizingMaskIntoConstraints = false

    self.inputView = baseView
  }

  open override func viewDidLoad() {
    let baseView = self.inputView as! CustomInputView

    // Perform custom UI setup here
    baseView.load()
    baseView.setConstraints()

    // Adds a very basic "Next keyboard" button to ensure we can always swap keyboards, even on iPhone SE.
    self.nextKeyboardButton = UIButton(type: .system)

    self.nextKeyboardButton.setTitle(NSLocalizedString("Next Keyboard", comment: "Title for 'Next Keyboard' button"), for: [])
    self.nextKeyboardButton.sizeToFit()
    self.nextKeyboardButton.translatesAutoresizingMaskIntoConstraints = false

    self.nextKeyboardButton.addTarget(self, action: #selector(handleInputModeList(from:with:)), for: .allTouchEvents)

    self.view.addSubview(self.nextKeyboardButton)

    var guide: UILayoutGuide
    guide = self.view.safeAreaLayoutGuide
    self.nextKeyboardButton.isHidden = !self.needsInputModeSwitchKey || !asSystemKeyboard

    self.nextKeyboardButton.leftAnchor.constraint(equalTo: guide.leftAnchor).isActive = true
    self.nextKeyboardButton.bottomAnchor.constraint(equalTo: guide.bottomAnchor).isActive = true
  }

  override func viewWillLayoutSubviews() {
      super.viewWillLayoutSubviews()
  }
}
