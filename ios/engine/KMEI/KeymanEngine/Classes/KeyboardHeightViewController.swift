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
    let isPortrait = deviceOrientation.isPortrait
 
    let keyboardScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: isPortrait)
    let scalemessage = "KeyboardHeightViewController viewDidLoad, isPortrait: \(isPortrait) keyboardScale: \(String(describing: keyboardScale))"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, scalemessage)

    let kbHeight = keyboardScale?.keyboardHeight ?? 216 // default for ancient devices

    let message = "   viewDidLoad,  boundsRect: \(self.view.bounds) keyboardHeight from scale: \(kbHeight)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)

    //title = NSLocalizedString("menu-settings-spacebar-title", bundle: engineBundle, comment: "")
    title = "Adjust Keyboard Height"
    navigationItem.setHidesBackButton(false, animated: true)
    navigationItem.leftBarButtonItem?.isEnabled = true

    navigationController?.toolbar?.barTintColor = Colors.statusToolbar

    contentView.frame = self.view.bounds
    contentView.translatesAutoresizingMaskIntoConstraints = false
    contentView.backgroundColor = .green
    //contentView.alpha = 0.5
    view.addSubview(contentView)

    let margins = view.layoutMarginsGuide
    NSLayoutConstraint.activate([
      contentView.leadingAnchor.constraint(equalTo: margins.leadingAnchor),
      contentView.trailingAnchor.constraint(equalTo: margins.trailingAnchor)
    ])

    let safeAreaGuide = view.safeAreaLayoutGuide
    NSLayoutConstraint.activate([
      contentView.topAnchor.constraint(equalToSystemSpacingBelow: safeAreaGuide.topAnchor, multiplier: 1.0),
      safeAreaGuide.bottomAnchor.constraint(equalToSystemSpacingBelow: contentView.bottomAnchor, multiplier: 1.0)
     ])

    label.frame = CGRectMake(10, 50, contentView.bounds.width-20, 100)
    label.backgroundColor=UIColor.white
    label.textAlignment = NSTextAlignment.left
    label.text = "Drag the top edge of the keyboard to adjust its height./nSeparate values are saved for portrait and landscape keyboard height."
    label.numberOfLines = 0
    //label.isHidden=true
    contentView.addSubview(label)
    
    // calculate frame for keyboard image
    let keyboardFrame = CGRectMake(0, contentView.frame.height-kbHeight, contentView.frame.width, kbHeight)
    keyboardImage.frame = keyboardFrame
    keyboardImage.contentMode = UIView.ContentMode.scaleToFill
    keyboardImage.backgroundColor=UIColor.cyan
    
    let kbImage = UIImage(named:"portrait-osk")
    let kbImageMessage = "   viewDidLoad,  keyboardFrame: \(keyboardFrame) kbImage: \(kbImage)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, kbImageMessage)

    keyboardImage.image = kbImage
    contentView.addSubview(keyboardImage)
  }
  
  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}

