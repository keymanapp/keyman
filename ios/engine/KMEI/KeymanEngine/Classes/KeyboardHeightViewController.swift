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

    let contentView = UIView(frame: self.view.bounds)
    contentView.backgroundColor = UIColor.lightGray
    contentView.alpha = 0.5
    view.addSubview(contentView)
    
    label.frame = CGRectMake(10, 50, contentView.bounds.width-20, 100)
    label.backgroundColor=UIColor.white
    label.textAlignment = NSTextAlignment.center
    label.text = "Drag the top edge of the keyboard to adjust its height./nSeparate values are saved for portrait and landscape keyboard height."
    label.numberOfLines = 0
    //label.isHidden=true
    contentView.addSubview(label)
    
    // calculate frame for keyboard image
    let keyboardFrame = CGRectMake(0, self.view.frame.height-kbHeight, self.view.frame.width, kbHeight)
    let kbFrameMessage = "   viewDidLoad,  keyboardFrame: \(keyboardFrame)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, kbFrameMessage)
    keyboardImage.frame = keyboardFrame
    keyboardImage.contentMode = UIView.ContentMode.scaleToFill
    keyboardImage.backgroundColor=UIColor.white
    keyboardImage.image = UIImage(named:"portrait-osk")
    contentView.addSubview(keyboardImage)
  }
  
  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}

