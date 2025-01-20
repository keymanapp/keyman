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
  let contentView: UIView = UIView()
  let defaultButton = UIButton(type: .roundedRect)
  let label: UILabel = UILabel()
  let keyboardImage: UIImageView = UIImageView()
  let keyboardResizer: UIImageView = UIImageView()
  var keyboardConstraintsArray: [NSLayoutConstraint] = []
  var resizerConstraintsArray: [NSLayoutConstraint] = []
  var isPortrait = true
  var keyboardHeight = 0.0
  var defaultPortraitHeight = 0.0
  var defaultLandscapeHeight = 0.0
  var minKeyboardHeight = 10.0
  var maxKeyboardHeight = 10.0

  public init() {
    super.init(nibName: nil, bundle: nil)
    _ = view
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    os_log("KeyboardHeightViewController viewDidLoad", log:KeymanEngineLogger.ui, type: .info)

    self.determineOrientation()
    self.determineDefaultKeyboardHeights()
    self.applyKeyboardHeight()
    
    title = NSLocalizedString("adjust-keyboard-height-title", bundle: engineBundle, comment: "")
    navigationItem.setHidesBackButton(false, animated: true)
    navigationItem.leftBarButtonItem?.isEnabled = true

    navigationController?.toolbar?.barTintColor = UIColor.orange

    self.configureContentView()
    self.configureDefaultHeightButton()
    self.configureLabel()
    self.configureKeyboardImage()
    self.updateKeyboardImage()
    self.configureKeyboardConstraints()
    self.configureKeyboardResizer()
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
 
    self.calculateKeyboardHeightLimits()
  }
  
  private func determineOrientation() {
    self.isPortrait = UIScreen.main.bounds.height > UIScreen.main.bounds.width
    let message = "determineOrientation, isPortrait: \(self.isPortrait)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, message)
  }
  
  private func determineDefaultKeyboardHeights() {
    // if no KeyboardScaleMap found for device, then default to Defaults.defaultUnknownKeyboardHeight
    let portraitKeyboardScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: true)
    self.defaultPortraitHeight = Double(portraitKeyboardScale?.keyboardHeight ?? Defaults.unknownDeviceKeyboardHeight)

    let landscapeKeyboardScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: false)
    self.defaultLandscapeHeight = Double(landscapeKeyboardScale?.keyboardHeight ?? Defaults.unknownDeviceKeyboardHeight)
  }
  
  /**
   * Read the previously set keyboard heights from UserDefaults.
   * Running the first time, these values would be defaults set in KeymanWebviewController.initKeyboardSize()
   * These values should be set prior to now; if not, log an error and use  default values
   */
  private func applyKeyboardHeight() {
    if (self.isPortrait) {
      if (Storage.active.userDefaults.object(forKey: Key.portraitKeyboardHeight) != nil) {
        self.keyboardHeight = Storage.active.userDefaults.portraitKeyboardHeight
        let message = "applyKeyboardHeight, from UserDefaults loaded portrait value \(self.keyboardHeight)"
        os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, message)
      } else {
        self.keyboardHeight = self.defaultPortraitHeight
        let message = "applyKeyboardHeight, portraitHeight not found in UserDefaults, using default value \(self.keyboardHeight)"
        os_log("%{public}s", log:KeymanEngineLogger.ui, type: .error, message)
      }
    } else {
      if (Storage.active.userDefaults.object(forKey: Key.portraitKeyboardHeight) != nil) {
        self.keyboardHeight = Storage.active.userDefaults.landscapeKeyboardHeight
        let message = "applyKeyboardHeight, from UserDefaults loaded landscape value \(self.keyboardHeight)"
        os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, message)
      } else {
        self.keyboardHeight = self.defaultLandscapeHeight
        let message = "applyKeyboardHeight, landscapeHeight not found in UserDefaults, using default value \(self.keyboardHeight)"
        os_log("%{public}s", log:KeymanEngineLogger.ui, type: .error, message)
      }
    }
  }

  
  /**
   * Set some reasonable limits on how small or large a keyboard can be set to during resizing.
   */
  private func calculateKeyboardHeightLimits() {
    // minimum height
    self.minKeyboardHeight = 100.0
    
    // set maxKeyboardHeight to 100 pts smaller than the contentView height
    self.maxKeyboardHeight = contentView.frame.height - 100.0
    
    let message = "minKeyboardHeight: \(minKeyboardHeight) maxKeyboardHeight: \(maxKeyboardHeight)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, message)
  }
  
  /**
   * Given the vertical attemptedTranslation in points due to the user dragging the keyboard resizer view,
   * return the amount that the user is allowed to grow or shrink the keyboard without exceeding its size limits.
   */
  private func applyKeyboardSizeLimits(attemptedTranslation: Double) -> Double {
    guard attemptedTranslation != 0 else {
      return 0
    }

    // Are we trying to reduce or enlarge the keyboard?
    // An increase in keyboardResizer .y translation moves it down and reduces the keyboard height.
    let reducingKeyboardHeight = attemptedTranslation > 0
    
    // by default, unless we exceed keyboard height limitations, we will approve all of the translation
    var approvedTranslation = attemptedTranslation

    // Calculate new proposed height by subtracting the translation.
    let proposedKeyboardHeight = self.keyboardHeight - attemptedTranslation

    // calculations vary depending on reducing or enlarging the keyboard
    if (reducingKeyboardHeight) {
      // if we are reducing too much, then calculate how much translation is allowed
      if (proposedKeyboardHeight < self.minKeyboardHeight) {
        // if we are already at the minimum size, return 0
        if(self.keyboardHeight <= self.minKeyboardHeight) {
          approvedTranslation = 0;
        } else {
          // how much space to translate the resizer downward, a positive number
          let availableToReduce = self.keyboardHeight - self.minKeyboardHeight
          approvedTranslation = availableToReduce
        }
      }
    } else { // enlarging the keyboard height
      if (proposedKeyboardHeight > self.maxKeyboardHeight) {
        // if we are already at the maximum size, return 0
        if(self.keyboardHeight >= self.maxKeyboardHeight) {
          approvedTranslation = 0;
        } else {
          // how much space to translate the resizer upward, a negative number
          let availableToEnlarge = self.keyboardHeight - self.maxKeyboardHeight
          approvedTranslation = availableToEnlarge
        }
      }
    }
    
    // round to nearest whole number
    approvedTranslation = approvedTranslation.rounded(.toNearestOrAwayFromZero)
    let messageBegan = "approvedTranslation: \(approvedTranslation)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, messageBegan)
    return approvedTranslation
  }
  
  /**
   * Called when the keyboardResizer view is being moved to adjust the keyboard height.
   */
  @objc func handleDrag(_ dragRecognizer: UIPanGestureRecognizer) {
    switch dragRecognizer.state {
    case .began:
      os_log("keyboardResizer handleDrag began", log:KeymanEngineLogger.ui, type: .info)

    case .changed:
      self.provideResizeFeedback(drag: dragRecognizer)

    case .ended:
      // reset transforms before actual keyboard height change
      self.keyboardResizer.transform = .identity
      self.keyboardImage.transform = .identity
      
      let verticalTranslation = dragRecognizer.translation(in: self.contentView).y
      let approvedTranslation = self.applyKeyboardSizeLimits(attemptedTranslation: verticalTranslation)
      if (approvedTranslation != 0) {
        let newKeyboardHeight = keyboardHeight - approvedTranslation;
        self.writeNewKeyboardHeight(newHeight: newKeyboardHeight)
      } else {
        os_log("handleDrag .ended with no resizing", log:KeymanEngineLogger.ui, type: .info)
      }
    default: break
    }
  }
  
  /**
   * Stretch the keyboard image and move the keyboardResizier object as it is being resized.
   */
  private func provideResizeFeedback(drag: UIPanGestureRecognizer) {
    // how much did we drag vertically
    let verticalTranslation = drag.translation(in: self.contentView).y

    // enforce keyboard size limits on translation
    let approvedTranslation = self.applyKeyboardSizeLimits(attemptedTranslation: verticalTranslation)
    
    if (approvedTranslation != 0) {
      let keyboardTranslation = CGAffineTransform(translationX: 0, y: approvedTranslation)
      self.keyboardResizer.transform = keyboardTranslation
       
      let newKeyboardHeight = self.keyboardHeight - approvedTranslation;
      let yScaleFactor = newKeyboardHeight/self.keyboardHeight
      
      let keyboardScale = CGAffineTransform(scaleX: 1, y: yScaleFactor)
      
      // concatenate both the translate and the scale for the keyboard image
      let keyboardHeightTranslation = CGAffineTransform(translationX: 0, y: approvedTranslation/2)
      let keyboardImageTransform = keyboardScale.concatenating(keyboardHeightTranslation)
      self.keyboardImage.transform = keyboardImageTransform
    }
  }
  
  /**
   * Write the new height of the keyboard to to UserDefaults.
   */
  private func writeNewKeyboardHeight (newHeight: Double) {
    let message = "writeNewKeyboardHeight, newHeight :\(newHeight) isPortrait:  \(isPortrait)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)
    
    keyboardHeight = newHeight
    self.updateKeyboardConstraints()
    
    if (isPortrait) {
      Storage.active.userDefaults.portraitKeyboardHeight = newHeight
    } else {
      Storage.active.userDefaults.landscapeKeyboardHeight = newHeight
    }
  }
  
  private func configureKeyboardConstraints() {
    // create constraints for each orientation and apply when device is rotated
    self.keyboardConstraintsArray = [
      keyboardImage.leftAnchor.constraint(equalTo: contentView.leftAnchor),
      keyboardImage.rightAnchor.constraint(equalTo: contentView.rightAnchor),
      keyboardImage.topAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -keyboardHeight),
      keyboardImage.heightAnchor.constraint(equalToConstant: keyboardHeight)
    ]
    NSLayoutConstraint.activate(self.keyboardConstraintsArray)
  }

  /**
   * recalculate constraints for new portrait keyboard height
   */
  private func updateKeyboardConstraints() {
    if (!self.keyboardConstraintsArray.isEmpty) {
      NSLayoutConstraint.deactivate(self.keyboardConstraintsArray)
    }
    self.keyboardConstraintsArray = [
      keyboardImage.leftAnchor.constraint(equalTo: contentView.leftAnchor),
      keyboardImage.rightAnchor.constraint(equalTo: contentView.rightAnchor),
      keyboardImage.topAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -self.keyboardHeight),
      keyboardImage.heightAnchor.constraint(equalToConstant: self.keyboardHeight)
    ]
    NSLayoutConstraint.activate(self.keyboardConstraintsArray)
    
    self.updateKeyboardResizerConstraints()
  }
  
  private func configureContentView() {
    contentView.isUserInteractionEnabled = true
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
  
  func configureDefaultHeightButton() {
    defaultButton.titleLabel?.font = .systemFont(ofSize: 20, weight: .bold)
    let buttonTitle = NSLocalizedString("button-label-reset-default-keyboard-height", bundle: engineBundle, comment: "")
    defaultButton.setTitle(buttonTitle, for: .normal)
    defaultButton.sizeToFit()
    defaultButton.layer.cornerRadius = 8.0
    defaultButton.addTarget(self, action: #selector(self.restoreDefaultKeyboardHeight), for: .touchUpInside)
    contentView.addSubview(defaultButton)

    defaultButton.translatesAutoresizingMaskIntoConstraints = false
    NSLayoutConstraint.activate([
      defaultButton.topAnchor.constraint(equalTo: contentView.topAnchor, constant: 10.0),
      defaultButton.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: 10.0)
    ])
  }

  @objc private func restoreDefaultKeyboardHeight() {
    os_log("restored to default", log:KeymanEngineLogger.ui, type: .info)
    let defaultHeight = self.isPortrait ? self.defaultPortraitHeight : self.defaultLandscapeHeight
    
    UIView.animate(withDuration: 1.1, delay: 0.1, usingSpringWithDamping: 0.35, initialSpringVelocity: 4.5, options: .curveEaseOut, animations: {
        self.writeNewKeyboardHeight(newHeight: defaultHeight)
        self.view.layoutIfNeeded()
      }, completion: nil)
  }
  
  private func configureLabel() {
    label.backgroundColor = .white
    label.textColor = .black
    label.textAlignment = NSTextAlignment.left
    label.font = UIFont.systemFont(ofSize: 16.0)
    self.setLabelTextForOrientation()
    label.numberOfLines = 0
    contentView.addSubview(label)

    label.translatesAutoresizingMaskIntoConstraints = false
    NSLayoutConstraint.activate([
      label.topAnchor.constraint(equalTo: self.defaultButton.bottomAnchor, constant: 5.0),
      label.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: 10.0),
      label.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant:-25.0),
      label.heightAnchor.constraint(greaterThanOrEqualToConstant: 50.0)
    ])
  }

  private func setLabelTextForOrientation() {
    let dragText = NSLocalizedString("keyboard-drag-instructions", bundle: engineBundle, comment: "")
    var rotateText: String? = nil;
    
    if (self.isPortrait) {
      rotateText = NSLocalizedString("portrait-keyboard-rotate-instructions", bundle: engineBundle, comment: "")
    } else {
      rotateText = NSLocalizedString("landscape-keyboard-rotate-instructions", bundle: engineBundle, comment: "")
    }
    
    label.text = "\(dragText)\n\(rotateText!)"
  }

  private func configureKeyboardImage() {
    keyboardImage.isUserInteractionEnabled = true
    keyboardImage.contentMode = UIView.ContentMode.scaleToFill
    keyboardImage.backgroundColor=UIColor.cyan
    contentView.addSubview(keyboardImage)

    keyboardImage.translatesAutoresizingMaskIntoConstraints = false
  }

  private func configureKeyboardResizer() {
    var arrowSymbolImage: UIImage? = nil
    keyboardResizer.isUserInteractionEnabled = true
    
    // if using iOS earlier than 13.0, use bitmap image of arrow and non-dynamic gray
    if #available(iOSApplicationExtension 13.0, *) {
      arrowSymbolImage = UIImage(systemName: "arrow.up.and.down.square.fill")
      keyboardResizer.backgroundColor = UIColor.systemGray3 // adapts for light/dark mode
    } else {
      arrowSymbolImage = UIImage(named:"arrow.up.and.down", in:engineBundle, compatibleWith:nil)
      keyboardResizer.backgroundColor = UIColor.systemGray
    }
    keyboardResizer.image = arrowSymbolImage
    keyboardResizer.layer.cornerRadius = 5
    contentView.addSubview(keyboardResizer)
    keyboardResizer.translatesAutoresizingMaskIntoConstraints = false

    self.updateKeyboardResizerConstraints()

    let drag = UIPanGestureRecognizer(target: self, action: #selector(handleDrag))
    drag.maximumNumberOfTouches = 1
    drag.minimumNumberOfTouches = 1
    keyboardResizer.addGestureRecognizer(drag)
  }
  
  private func updateKeyboardResizerConstraints() {
    // first, clear existing constraints
    if (!resizerConstraintsArray.isEmpty) {
      NSLayoutConstraint.deactivate(resizerConstraintsArray)
    }

    // add new constraints using the current value of the keyboard height
    resizerConstraintsArray = [
      keyboardResizer.centerXAnchor.constraint(equalTo: contentView.centerXAnchor),
      keyboardResizer.topAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -keyboardHeight-30),
      keyboardResizer.heightAnchor.constraint(equalToConstant: 60),
      keyboardResizer.widthAnchor.constraint(equalToConstant: 60)
    ]
    NSLayoutConstraint.activate(resizerConstraintsArray)
  }

  private func updateKeyboardImage() {
    var kbImage: UIImage? = nil
    if (self.isPortrait) {
      kbImage = UIImage(named:"keyboard.compact", in:engineBundle, compatibleWith:nil)
    } else {
      kbImage = UIImage(named:"keyboard.regular", in:engineBundle, compatibleWith:nil)
    }
    keyboardImage.image = kbImage

    let kbImageMessage = "updateKeyboardImage,  kbImage: \(String(describing: kbImage))"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, kbImageMessage)
  }

  override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
    let rotateMessage = "viewWillTransition to size \(size) with current UIScreen.main.bounds.size size = \(UIScreen.main.bounds.size)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, rotateMessage)
    super.viewWillTransition(to: size, with: coordinator)

    /**
     determine which orientation the device had rotated to and adjust content as necessary
     */
    self.determineOrientation()
  
    coordinator.animate(alongsideTransition: { _ in
      self.applyKeyboardHeight()
      self.calculateKeyboardHeightLimits()
      self.setLabelTextForOrientation()
      self.updateKeyboardImage()
      self.updateKeyboardConstraints()
    }, completion: nil)
  }

  override func traitCollectionDidChange(_ previousTraitCollection: UITraitCollection?) {
       super.traitCollectionDidChange(previousTraitCollection)
       if (traitCollection.userInterfaceStyle == .dark) {
         os_log("keyboardHeight traitCollectionDidChange to .dark", log:KeymanEngineLogger.ui, type: .debug)
       } else {
         os_log("keyboardHeight traitCollectionDidChange to .light", log:KeymanEngineLogger.ui, type: .debug)
       }
   }

  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}

