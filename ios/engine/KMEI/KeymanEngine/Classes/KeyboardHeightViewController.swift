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

    self.determineOrientation(screenSize: UIScreen.main.bounds.size)
    self.loadDefaultKeyboardHeights()
    self.applyKeyboardHeight()
    
    //title = NSLocalizedString("menu-settings-spacebar-title", bundle: engineBundle, comment: "")
    title = "Adjust Height"
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
  
  private func determineOrientation(screenSize: CGSize) {
    self.isPortrait = UIScreen.main.bounds.height > UIScreen.main.bounds.width
    let message = "determineOrientation, isPortrait: \(self.isPortrait)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, message)
  }
  
  private func loadDefaultKeyboardHeights() {
    // if no KeyboardScaleMap found for device, then default to 216.0
    let portraitKeyboardScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: true)
    self.defaultPortraitHeight = Double(portraitKeyboardScale?.keyboardHeight ?? 216.0)

    let landscapeKeyboardScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: false)
    self.defaultLandscapeHeight = Double(landscapeKeyboardScale?.keyboardHeight ?? 216.0)
  }
  
  private func applyKeyboardHeight() {
    if (self.isPortrait) {
      if (Storage.active.userDefaults.object(forKey: Key.portraitKeyboardHeight) != nil) {
        self.keyboardHeight = Storage.active.userDefaults.portraitKeyboardHeight
        let message = "applyKeyboardHeight, from UserDefaults loaded portrait value \(self.keyboardHeight)"
        os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)
      } else {
        self.keyboardHeight = self.defaultPortraitHeight
        let message = "applyKeyboardHeight, portraitHeight not found in UserDefaults, using default value \(self.keyboardHeight)"
        os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)
      }
    } else {
      if (Storage.active.userDefaults.object(forKey: Key.portraitKeyboardHeight) != nil) {
        self.keyboardHeight = Storage.active.userDefaults.landscapeKeyboardHeight
        let message = "applyKeyboardHeight, from UserDefaults loaded landscape value \(self.keyboardHeight)"
        os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)
      } else {
        self.keyboardHeight = self.defaultLandscapeHeight
        let message = "applyKeyboardHeight, landscapeHeight not found in UserDefaults, using default value \(self.keyboardHeight)"
        os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)
      }
    }
  }

  private func calculateKeyboardHeightLimits() {
    // minimum height
    self.minKeyboardHeight = 100.0
    
    // set maxKeyboardHeight to 100 pts smaller than the contentView height
    self.maxKeyboardHeight = contentView.frame.height - 100.0
    
    let messageBegan = "minKeyboardHeight: \(minKeyboardHeight) maxKeyboardHeight: \(maxKeyboardHeight)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, messageBegan)
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
    
    let messageBegan = "approvedTranslation: \(approvedTranslation)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, messageBegan)
    return approvedTranslation
  }
  
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
        self.changeKeyboardHeight(newHeight: newKeyboardHeight)
      } else {
        os_log("handleDrag .ended with no resizing", log:KeymanEngineLogger.ui, type: .info)
      }
    default: break
    }
  }
  
  private func provideResizeFeedback(drag: UIPanGestureRecognizer) {
    // how much did we drag vertically
    let verticalTranslation = drag.translation(in: self.contentView).y

    let messageBegan = "verticalTranslation: \(verticalTranslation)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .debug, messageBegan)

    // enforce keyboard size limits on translation
    let approvedTranslation = self.applyKeyboardSizeLimits(attemptedTranslation: verticalTranslation)
    
    if (approvedTranslation == 0) {
      os_log("reached keyboard size limit", log:KeymanEngineLogger.ui, type: .debug)
    } else if (approvedTranslation != 0) {
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
  
  private func changeKeyboardHeight (newHeight: Double) {
    let messageOne = "changeKeyboardHeight, newHeight :\(newHeight) isPortrait:  \(isPortrait)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, messageOne)
    
    keyboardHeight = newHeight
    self.updateKeyboardConstraints()
    //self.view.setNeedsLayout()
    
    if (isPortrait) {
      Storage.active.userDefaults.portraitKeyboardHeight = newHeight
      let persistMessage = "changeKeyboardHeight, persist portraitKeyboardHeight :\(newHeight)"
      os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, persistMessage)
    } else {
      Storage.active.userDefaults.landscapeKeyboardHeight = newHeight
      let persistMessage = "changeKeyboardHeight, persist landscapeKeyboardHeight :\(newHeight)"
      os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, persistMessage)
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

  // recalculate constraints for new portrait keyboard height
  private func updateKeyboardConstraints() {
    if (!self.keyboardConstraintsArray.isEmpty) {
      NSLayoutConstraint.deactivate(self.keyboardConstraintsArray)
    }
    self.keyboardConstraintsArray = [
      keyboardImage.leftAnchor.constraint(equalTo: contentView.leftAnchor),
      keyboardImage.rightAnchor.constraint(equalTo: contentView.rightAnchor),
      keyboardImage.topAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -keyboardHeight),
      keyboardImage.heightAnchor.constraint(equalToConstant: keyboardHeight)
    ]
    NSLayoutConstraint.activate(self.keyboardConstraintsArray)
    
    self.updateKeyboardResizerConstraints()
  }
  
  func configureContentView() {
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

  @objc func restoreDefaultKeyboardHeight() {
    os_log("restored to default", log:KeymanEngineLogger.ui, type: .info)
    let defaultHeight = self.isPortrait ? self.defaultPortraitHeight : self.defaultLandscapeHeight
    
    UIView.animate(withDuration: 1.1, delay: 0.1, usingSpringWithDamping: 0.35, initialSpringVelocity: 4.5, options: .curveEaseOut, animations: {
        self.changeKeyboardHeight(newHeight: defaultHeight)
        self.view.layoutIfNeeded()
      }, completion: nil)
  }
  
  func configureLabel() {
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

  func setLabelTextForOrientation() {
    let dragText = NSLocalizedString("keyboard-drag-instructions", bundle: engineBundle, comment: "")
    var rotateText: String? = nil;
    
    if (self.isPortrait) {
      rotateText = NSLocalizedString("portrait-keyboard-rotate-instructions", bundle: engineBundle, comment: "")
    } else {
      rotateText = NSLocalizedString("landscape-keyboard-rotate-instructions", bundle: engineBundle, comment: "")
    }
    
    label.text = "\(dragText)\n\(rotateText!)."
  }

  func configureKeyboardImage() {
    keyboardImage.isUserInteractionEnabled = true
    keyboardImage.contentMode = UIView.ContentMode.scaleToFill
    keyboardImage.backgroundColor=UIColor.cyan
    contentView.addSubview(keyboardImage)

    keyboardImage.translatesAutoresizingMaskIntoConstraints = false
  }

  func configureKeyboardResizer() {
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
  
  func updateKeyboardResizerConstraints() {
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

  func updateKeyboardImage() {
    var kbImage: UIImage? = nil
    if (self.isPortrait) {
      kbImage = UIImage(named:"keyboard.compact", in:engineBundle, compatibleWith:nil)
    } else {
      kbImage = UIImage(named:"keyboard.regular", in:engineBundle, compatibleWith:nil)
    }
    keyboardImage.image = kbImage

    let kbImageMessage = "updateKeyboardImage,  kbImage: \(String(describing: kbImage))"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, kbImageMessage)
  }

  override func viewWillTransition(to size: CGSize, with coordinator: UIViewControllerTransitionCoordinator) {
    super.viewWillTransition(to: size, with: coordinator)
    let rotateMessage = "viewWillTransition called"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, rotateMessage)

    /**
     using the specified size, determine which orientation the device is moving to and adjust content as necessary
     */
    self.determineOrientation(screenSize: size)
  
    coordinator.animate(alongsideTransition: { _ in
    //UIView.animate(withDuration: 1, delay: 0, options: .layoutSubviews, animations: {
      self.applyKeyboardHeight()
      self.calculateKeyboardHeightLimits()
      self.setLabelTextForOrientation()
      self.updateKeyboardImage()
      self.updateKeyboardConstraints()
      //self.view.setNeedsLayout()
    }, completion: nil)

//    coordinator.animate(alongsideTransition: nil) { _ in
//      self.applyKeyboardHeight()
//      self.calculateKeyboardHeightLimits()
//      self.setLabelTextForOrientation()
//      self.updateKeyboardImage()
//      self.updateKeyboardConstraints()
//    }
//    self.view.setNeedsLayout()
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

