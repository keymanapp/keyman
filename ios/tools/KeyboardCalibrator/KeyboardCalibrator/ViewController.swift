//
//  ViewController.swift
//  DefaultKeyboardHost
//
//  Created by Joshua Horton on 1/10/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import UIKit

class OrientationSpec {
  var predictRect: CGRect? = nil
  var plainRect: CGRect? = nil
  var dummyRect: CGRect? = nil

  var insetBottom: CGFloat? = nil

  public init() {
  }

  public var sufficient: Bool {
    get {
      return predictRect != nil && plainRect != nil && insetBottom != nil
    }
  }

  public var complete: Bool {
    get {
      return sufficient && dummyRect != nil
    }
  }

  public var bannerHeight: CGFloat {
    get {
      if let predictHeight = predictRect?.height, let plainHeight = plainRect?.height {
        return predictHeight - plainHeight
      } else {
        return CGFloat.nan
      }
    }
  }

  public var keyboardHeight: CGFloat {
    get {
      if !sufficient {
        return CGFloat.nan
      } else {
        return plainRect!.height - insetBottom! - inputServicesHeight
      }
    }
  }

  public var inputServicesHeight: CGFloat {
    if let plainRect = plainRect {
      if let dummyRect = dummyRect {
        return dummyRect.height - plainRect.height
      } else {
        return 0
      }
    } else {
      return CGFloat.nan
    }
  }
}

// Corresponds to indices within `operationalControl`
enum CaptureOperation: Int {
  case predict = 0
  case plain = 1
  case calibrate = 2
  case check = 3
}

class ViewController: UIViewController {

  @IBOutlet weak var bannerLabel: UILabel!
  @IBOutlet weak var keyboardLabel: UILabel!
  @IBOutlet weak var insetLabel: UILabel!
  @IBOutlet weak var systemLabel: UILabel!
  @IBOutlet weak var captureStatsView: UIScrollView!
  @IBOutlet weak var uikitPointsLabel: UILabel!
  
  @IBOutlet weak var captureInput: UITextField!
  @IBOutlet weak var operationControl: UISegmentedControl!
  @IBOutlet weak var captureButton: UIButton!

  var capture: OrientationSpec! = OrientationSpec()
  var captureMode: CaptureOperation = .predict
  var kbdFrame: NSValue? = nil

  override func viewDidLoad() {
    super.viewDidLoad()

    NotificationCenter.default.addObserver(
      self,
      selector: #selector(keyboardWillShow),
      name: UIResponder.keyboardWillShowNotification,
      object: nil
    )

    captureStatsView.isHidden = true
    let screen = UIScreen.main.bounds
    uikitPointsLabel.text = "(\(screen.width), \(screen.height))"
  }

  @objc func keyboardWillShow(_ notification: Notification) {
    if let keyboardFrame: NSValue = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? NSValue {

        // We can only retrieve this during the notification, so we save it to a property
        // for later capture.
        kbdFrame = keyboardFrame
    }
  }

  @objc func clearKeyboard() {
    self.view.endEditing(true)
  }

  @IBAction func operationChanged(_ sender: Any) {
    // Operation mode has been altered; update accordingly
    captureMode = CaptureOperation(rawValue: operationControl.selectedSegmentIndex)!

    switch captureMode {
      case .predict:
        captureInput.autocorrectionType = .yes
        captureInput.inputView = nil
        captureButton.setTitle("Capture", for: .normal)
      case .plain:
        captureInput.autocorrectionType = .no
        captureInput.inputView = nil
        captureButton.setTitle("Capture", for: .normal)
      case .calibrate:
        captureInput.autocorrectionType = .no
        captureInput.inputView = nil
        captureButton.setTitle("Calibrate", for: .normal)
      case .check:
        captureInput.autocorrectionType = .no

        // Compute proper height
        captureInput.inputView = DummyInputViewController(height: capture.keyboardHeight, inset: capture.inputServicesHeight).inputView

        captureButton.setTitle("Clear keyboard", for: .normal)
    }

    captureInput.reloadInputViews()

    //clearKeyboard()
  }

  @IBAction func performCapture() {
    let rect = kbdFrame?.cgRectValue
    capture.insetBottom = self.view.safeAreaInsets.bottom

    // Determine correct capture mode
    switch captureMode {
      case .predict:
        capture.predictRect = rect
      case .plain:
        capture.plainRect = rect
      case .calibrate:
        capture.dummyRect = rect
      case .check:
        break
    }

    // Update according to state of overall capture
    if capture.sufficient {
      operationControl.setEnabled(true, forSegmentAt: CaptureOperation.calibrate.rawValue)

      // TODO: Update this thing
      onCaptureSufficient()
    }

    if capture.complete {
      operationControl.setEnabled(true, forSegmentAt: CaptureOperation.check.rawValue)
    }

    // Perform automatic operation-mode change where appropriate.
    switch captureMode {
      case .predict:
        operationControl.selectedSegmentIndex = CaptureOperation.plain.rawValue
        operationChanged(self) // is NOT automatically called otherwise!
      case .plain:
        operationControl.selectedSegmentIndex = CaptureOperation.calibrate.rawValue
        operationChanged(self) // is NOT automatically called otherwise!
      case .calibrate:
        operationControl.selectedSegmentIndex = CaptureOperation.check.rawValue
        operationChanged(self)
        break
      case .check:
        // Process is complete, so there's nothing automatic to do here.
        break
    }

    clearKeyboard()
  }

  func onCaptureSufficient() {
    let insetHeight = capture.insetBottom!
    var totalInset = insetHeight

    // Set the appropriate label's value!
    bannerLabel.text = "Banner: \(capture.bannerHeight)"
    insetLabel.text = "Inset: \(insetHeight)"

    if let _ = capture.dummyRect {
      systemLabel.text = "System inset: \(capture.inputServicesHeight)"
      totalInset += capture.inputServicesHeight
      systemLabel.isHidden = false
    } else {
      systemLabel.isHidden = true
    }

    // let keyboardHeight = capture.plainRect!.height - totalInset
    keyboardLabel.text = "Keyboard: \(capture.keyboardHeight)"

    DummyInputViewController.keyboardHeightDefault = capture.keyboardHeight

    captureStatsView.isHidden = false
  }

  @IBAction func clearCapture() {
    capture = OrientationSpec()

    captureStatsView.isHidden = true

    // Auto-reset to the default option
    operationControl.selectedSegmentIndex = CaptureOperation.predict.rawValue
    operationControl.setEnabled(false, forSegmentAt: CaptureOperation.calibrate.rawValue)
    operationControl.setEnabled(false, forSegmentAt: CaptureOperation.check.rawValue)
    operationChanged(self)
  }
}

