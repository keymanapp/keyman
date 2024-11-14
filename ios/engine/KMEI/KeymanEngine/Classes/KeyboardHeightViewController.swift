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
    self.applyKeyboardHeight()
    self.calculateKeyboardHeightLimits()
    
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
 
  }
  
  private func determineOrientation(screenSize: CGSize) {
    self.isPortrait = UIScreen.main.bounds.height > UIScreen.main.bounds.width
    let message = "determineOrientation, isPortrait: \(self.isPortrait)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)
  }
  
  private func applyKeyboardHeight() {
    let portraitKeyboardScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: true)
    let landscapeKeyboardScale = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: false)

    /*
    self.portraitKeyboardHeight = KeyboardScaleMap.getDeviceDefaultKeyboardScale(forPortrait: isPortrait)?.keyboardHeight ?? 216 // default for ancient devices
     */

    self.defaultPortraitHeight = Double(portraitKeyboardScale!.keyboardHeight)
    self.defaultLandscapeHeight = Double(landscapeKeyboardScale!.keyboardHeight)

    // TODO: first attempt read from UserDefaults
    if (self.isPortrait) {
      self.keyboardHeight = self.defaultPortraitHeight
    } else {
      self.keyboardHeight = self.defaultLandscapeHeight
    }
  }

  private func calculateKeyboardHeightLimits() {
    var defaultHeight = 0.0
    
    if (self.isPortrait) {
      defaultHeight = self.defaultPortraitHeight
    } else {
      defaultHeight = self.defaultLandscapeHeight
    }
    
    self.minKeyboardHeight = defaultHeight/1.85
    self.maxKeyboardHeight = defaultHeight*1.9
    let messageBegan = "minKeyboardHeight: \(minKeyboardHeight) maxKeyboardHeight: \(maxKeyboardHeight)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, messageBegan)
  }
  
  /**
   * Given a vertical translation in points based on the user dragging the keyboard resizer view,
   * return the amount that the user is allowed to grow or shrink the keyboard.
   * If the attemptedTranslation amount does cause the keyboard to exceed its size limits,
   * then the same value will be returned.
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
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, messageBegan)
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
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, messageBegan)

    // enforce keyboard size limits on translation
    let approvedTranslation = self.applyKeyboardSizeLimits(attemptedTranslation: verticalTranslation)
    
    if (approvedTranslation == 0) {
      os_log("reached keyboard size limit", log:KeymanEngineLogger.ui, type: .info)
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
    
    // TODO: persist
    let messageTwo = "changeKeyboardHeight, persist newHeight :\(newHeight) isPortrait:  \(self.isPortrait)"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, messageTwo)
    if (isPortrait) {
    } else {
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
    defaultButton.titleLabel?.font = UIFont.boldSystemFont(ofSize: 16)
    defaultButton.setTitle("Reset to Default Keyboard Height", for: .normal)
    defaultButton.sizeToFit()
    defaultButton.layer.cornerRadius = 8.0
    //defaultButton.backgroundColor=UIColor.systemBlue
    //defaultButton.titleLabel?.textColor = UIColor.white
    defaultButton.addTarget(self, action: #selector(self.restoreDefaultKeyboardHeight), for: .touchUpInside)
    //defaultButton.sizeToFit()
    contentView.addSubview(defaultButton)

    defaultButton.translatesAutoresizingMaskIntoConstraints = false
    NSLayoutConstraint.activate([
      defaultButton.topAnchor.constraint(equalTo: contentView.topAnchor, constant: 10.0),
      defaultButton.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: 10.0)
    ])
  }

  @objc func restoreDefaultKeyboardHeight() {
    os_log("restored to default", log:KeymanEngineLogger.ui, type: .info)
    let animationOptions: UIView.AnimationOptions = [.curveEaseInOut]
    if (self.isPortrait) {
      UIView.animate(withDuration: 0.7, delay: 0.0, options:animationOptions, animations: {
        self.changeKeyboardHeight(newHeight: self.defaultPortraitHeight)
        self.view.layoutIfNeeded()
      }, completion: nil)
    } else {
      UIView.animate(withDuration: 0.7, delay: 0.0, options:animationOptions, animations: {
        self.changeKeyboardHeight(newHeight: self.defaultLandscapeHeight)
        self.view.layoutIfNeeded()
      }, completion: nil)
    }
  }
  
  func configureLabel() {
    label.backgroundColor=UIColor.white
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
    let portraitText = "portrait"
    let landscapeText = "landscape"
    let otherOrientationText = (self.isPortrait) ? landscapeText : portraitText
    label.text = "Drag arrow control to adjust keyboard height.\nRotate device to adjust for \(otherOrientationText)."
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
    //let arrowSymbolImage = UIImage(systemName: "arrow.up.and.down.square.fill")
    //let arrowConfiguration = UIImage.SymbolConfiguration(scale: .large)
    if #available(iOSApplicationExtension 13.0, *) {
      arrowSymbolImage = UIImage(systemName: "arrow.up.and.down.square.fill")
    } else {
      arrowSymbolImage = UIImage(named: "arrow.up.and.down")
    }
    keyboardResizer.image = arrowSymbolImage
    keyboardResizer.backgroundColor = UIColor.systemGray
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

    // add new restraints using the current value of the keyboard height
    resizerConstraintsArray = [
      keyboardResizer.centerXAnchor.constraint(equalTo: contentView.centerXAnchor),
      keyboardResizer.topAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -keyboardHeight-22),
      keyboardResizer.heightAnchor.constraint(equalToConstant: 44),
      keyboardResizer.widthAnchor.constraint(equalToConstant: 44)
    ]
    NSLayoutConstraint.activate(resizerConstraintsArray)
  }

  func updateKeyboardImage() {
    var kbImage: UIImage? = nil
    if (self.isPortrait) {
      kbImage = UIImage(named:"portrait-osk")
    } else {
      kbImage = UIImage(named:"landscape-osk")
    }
    keyboardImage.image = kbImage

    let kbImageMessage = "   viewDidLoad,  kbImage: \(String(describing: kbImage))"
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
    
    coordinator.animate(alongsideTransition: nil) { _ in
      self.applyKeyboardHeight()
      self.calculateKeyboardHeightLimits()
      self.setLabelTextForOrientation()
      self.updateKeyboardImage()
      self.updateKeyboardConstraints()
    }
    self.view.setNeedsLayout()
  }

  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
}

