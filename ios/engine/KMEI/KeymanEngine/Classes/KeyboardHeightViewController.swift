/*
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * Created by Shawn Schantz on 2024-10-25.
 * 
 * View controller for adjusting the height of the keyboard
 */

import Foundation
import UIKit
import os.log

class KeyboardHeightViewController: UIViewController {
  var label: UILabel = UILabel()
  var keyboardImage: UIImageView = UIImageView()
  var contentView: UIView = UIView()
  
  public init() {
    super.init(nibName: nil, bundle: nil)
    _ = view
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    let deviceOrientation = UIDevice.current.orientation
    let portrait = deviceOrientation.isPortrait
 
    let keyboardScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: portrait)
    let scalemessage = "KeyboardHeightViewController viewDidLoad, isPortrait: \(portrait) keyboardScale: \(String(describing: keyboardScale))"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, scalemessage)

    let kbHeight = keyboardScale?.keyboardHeight ?? 216 // default for ancient devices

    let message = "   viewDidLoad,  boundsRect: \(self.view.bounds) keyboardHeight from scale: \(kbHeight)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)

    //title = NSLocalizedString("menu-settings-spacebar-title", bundle: engineBundle, comment: "")
    title = "Adjust Keyboard Height"
    navigationItem.setHidesBackButton(false, animated: true)
    navigationItem.leftBarButtonItem?.isEnabled = true

    navigationController?.toolbar?.barTintColor = Colors.statusToolbar

    self.configureContentView()
    self.configureLabel(isPortrait: portrait)
    self.configureKeyboardImage(keyboardHeight: kbHeight)
  }
  
  func configureContentView() {
    //contentView.backgroundColor = .green
    contentView.backgroundColor = .white
    view.addSubview(contentView)

    contentView.translatesAutoresizingMaskIntoConstraints = false
    NSLayoutConstraint.activate([
      contentView.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor),
      contentView.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor),
      contentView.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor),
      contentView.bottomAnchor.constraint(equalTo: view.safeAreaLayoutGuide.bottomAnchor)
    ])
  }
  
  func configureLabel(isPortrait: Bool) {
    label.backgroundColor=UIColor.white
    label.textAlignment = NSTextAlignment.left
    let portraitText = "portrait"
    let landscapeText = "landscape"
    let otherOrientationText = (isPortrait) ? landscapeText : portraitText
    label.text = "Instructions: \ndrag the top edge of the keyboard to adjust height.\nRotate device to set for \(otherOrientationText)."
    
    label.numberOfLines = 0
    contentView.addSubview(label)

    label.translatesAutoresizingMaskIntoConstraints = false
    NSLayoutConstraint.activate([
      label.topAnchor.constraint(equalTo: contentView.topAnchor, constant:50.0),
      label.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant:25.0),
      label.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant:-25.0),
      label.heightAnchor.constraint(greaterThanOrEqualToConstant: 50.0)
    ])
  }

  func configureKeyboardImage(keyboardHeight: CGFloat) {
    keyboardImage.contentMode = UIView.ContentMode.scaleToFill
    keyboardImage.backgroundColor=UIColor.cyan
    let kbImage = UIImage(named:"portrait-osk")
    keyboardImage.image = kbImage
    contentView.addSubview(keyboardImage)

    let kbImageMessage = "   viewDidLoad,  kbImage: \(String(describing: kbImage))"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, kbImageMessage)

    keyboardImage.translatesAutoresizingMaskIntoConstraints = false
    NSLayoutConstraint.activate([
      keyboardImage.topAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -keyboardHeight),
      keyboardImage.leftAnchor.constraint(equalTo: contentView.leftAnchor),
      keyboardImage.rightAnchor.constraint(equalTo: contentView.rightAnchor),
      keyboardImage.heightAnchor.constraint(equalToConstant: keyboardHeight)
    ])
  }

  override func viewDidAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    let kbImageMessage = "viewDidAppear,  kbImage: \(String(describing: self.keyboardImage.image)) kbFrame: \(self.keyboardImage.frame)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, kbImageMessage)

  }

  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}

