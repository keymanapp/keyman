//
//  MainViewController.swift
//  KeymanEngineDemo
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit
import QuartzCore

class MainViewController: UIViewController, UIAlertViewDelegate, TextViewDelegate, TextFieldDelegate {
  var textView1: TextView!
  var textView2: TextField!
  var textView3: UITextView!
  
  private var keyboardPickerDismissedObserver: NotificationObserver?
  
  deinit {
    NotificationCenter.default.removeObserver(self)
  }
  
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
    title = "Keyman Demo"
    
    NotificationCenter.default.addObserver(self, selector: #selector(self.resizeView),
                                           name: .UIKeyboardWillShow, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.resizeView),
                                           name: .UIKeyboardWillHide, object: nil)
    keyboardPickerDismissedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardPickerDismissed,
      observer: self,
      function: MainViewController.keyboardPickerDismissed)
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  // MARK: - View lifecycle
  override func viewDidLoad() {
    super.viewDidLoad()
    
    view.backgroundColor = UIColor.gray
    navigationItem.rightBarButtonItem = KeyboardPickerBarButtonItem(presentingVC: self)
    
    let contentView = self.contentView()
    contentView.frame = view.bounds
    view.addSubview(contentView)
  }
  
  // TODO prevent autorotation to portraitUpsideDown
  
  // MARK: - Helper Methods
  func contentView() -> UIScrollView {
    let contentWidth: CGFloat = 320.0 // arbitrary non-zero width for now (it will get auto-resized)
    let margin: CGFloat = 10.0
    let contentView = UIScrollView(frame: CGRect(x: 0.0, y: 0.0, width: contentWidth, height: 1.0))
    
    // Labels
    let keymanLabel = UILabel()
    keymanLabel.text = "Keyman Text Views/Fields:"
    keymanLabel.numberOfLines = 0
    keymanLabel.lineBreakMode = .byWordWrapping
    keymanLabel.frame = CGRect(x: margin, y: margin, width: contentWidth - margin * 2.0, height: 40.0)
    keymanLabel.autoresizingMask = .flexibleWidth
    keymanLabel.backgroundColor = UIColor.clear
    keymanLabel.textColor = UIColor.white
    contentView.addSubview(keymanLabel)
    
    let normalLabel = UILabel()
    normalLabel.text = "Normal Text View:"
    normalLabel.numberOfLines = 0
    normalLabel.lineBreakMode = .byWordWrapping
    normalLabel.frame = CGRect(x: margin, y: margin, width: contentWidth - margin * 2.0, height: 40.0)
    normalLabel.autoresizingMask = .flexibleWidth
    normalLabel.backgroundColor = UIColor.clear
    normalLabel.textColor = UIColor.white
    contentView.addSubview(normalLabel)
    
    // Text Views/Fields
    textView1 = TextView()
    textView2 = TextField()
    textView3 = UITextView()
    setTextViewStyle(textView1)
    setTextFieldStyle(textView2)
    setTextViewStyle(textView3)
    
    textView1.setKeymanDelegate(self)
    textView2.setKeymanDelegate(self)
    textView2.placeholder = "text field"
    textView2.borderStyle = .roundedRect
    textView2.clearButtonMode = .whileEditing
    
    textView1.frame = CGRect(x: margin, y: keymanLabel.frame.maxY + 5.0,
                             width: contentView.frame.width - margin * 2.0, height: 80.0)
    textView2.frame = CGRect(x: margin, y: textView1.frame.maxY + margin,
                             width: contentView.frame.width - margin * 2.0, height: 30.0)
    normalLabel.frame = CGRect(x: margin, y: textView2.frame.maxY + margin,
                               width: contentWidth - margin * 2.0, height: 40.0)
    textView3.frame = CGRect(x: margin, y: normalLabel.frame.maxY + margin,
                             width: contentWidth - margin * 2.0, height: 40.0)
    
    textView1.viewController = self
    textView2.viewController = self
    contentView.addSubview(textView1)
    contentView.addSubview(textView2)
    contentView.addSubview(textView3)
    
    // Button - keyboard picker
    let kbButton = KeyboardPickerButton(presentingVC: self)
    kbButton.center = CGPoint(x: contentWidth / 2.0, y: textView3.frame.maxY + 30.0)
    kbButton.frame = kbButton.frame.integral
    kbButton.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin]
    contentView.addSubview(kbButton)
    
    // Button - close keyboard
    let dismissButton = UIButton(type: .roundedRect)
    dismissButton.setTitle("Close Keyboard", for: .normal)
    dismissButton.addTarget(self, action: #selector(self.dismissKeyboard), for: .touchUpInside)
    dismissButton.sizeToFit()
    dismissButton.center = CGPoint(x: contentWidth / 2.0, y: kbButton.frame.maxY + 30.0)
    dismissButton.frame = dismissButton.frame.integral
    dismissButton.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin]
    contentView.addSubview(dismissButton)
    
    // Button - download custom keyboard
    let downloadButton = UIButton(type: .roundedRect)
    downloadButton.setTitle("Download custom keyboard", for: .normal)
    downloadButton.sizeToFit()
    downloadButton.center = CGPoint(x: contentWidth / 2.0, y: dismissButton.frame.maxY + 30.0)
    downloadButton.frame = downloadButton.frame.integral
    downloadButton.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin]
    contentView.addSubview(downloadButton)
    
    // Content view setup
    contentView.frame = CGRect(x: 0.0, y: 0.0, width: contentWidth, height: downloadButton.frame.maxY + margin)
    contentView.contentSize = contentView.frame.size
    contentView.backgroundColor = UIColor.gray
    contentView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
    return contentView
  }
  
  @objc func dismissKeyboard() {
    textView1?.dismissKeyboard()
    textView2?.dismissKeyboard()
    textView3?.resignFirstResponder()
  }
  
  func setTextViewStyle(_ textView: UITextView) {
    textView.font = UIFont.systemFont(ofSize: 20.0)
    textView.autoresizingMask = .flexibleWidth
    textView.layer.cornerRadius = 6.0
  }
  
  func setTextFieldStyle(_ textView: UITextField) {
    textView.font = UIFont.systemFont(ofSize: 20.0)
    textView.autoresizingMask = .flexibleWidth
    textView.layer.cornerRadius = 6.0
  }
  
  @objc func resizeView(forKeyboardNotification notification: Notification) {
    let endFrame = notification.userInfo?[UIKeyboardFrameEndUserInfoKey] as? NSValue
    let duration = notification.userInfo?[UIKeyboardAnimationDurationUserInfoKey] as? NSNumber
    let curve = notification.userInfo?[UIKeyboardAnimationCurveUserInfoKey] as? NSNumber
    
    let statusBarHeight: CGFloat = 20.0
    let landscapeNavBarHeight: CGFloat = 33.0
    
    if let endFrame = endFrame, let duration = duration, duration != 0 {
      let keyboardFrame = endFrame.cgRectValue
      let orientation = UIApplication.shared.statusBarOrientation
      let oldFrame = view.frame
      var newFrame = oldFrame
      switch orientation {
      case .portrait:
        newFrame = CGRect(x: oldFrame.minX, y: oldFrame.minY,
                          width: oldFrame.width, height: keyboardFrame.minY - view.superview!.frame.minY)
      case .landscapeLeft:
        newFrame = CGRect(x: 0.0, y: 0.0, width: keyboardFrame.height,
                          height: keyboardFrame.minX - landscapeNavBarHeight - statusBarHeight)
      case .landscapeRight:
        let windowWidth = UIApplication.shared.keyWindow?.frame.width ?? 0
        newFrame = CGRect(x: 0.0, y: 0.0, width: keyboardFrame.height,
                          height: windowWidth - landscapeNavBarHeight - statusBarHeight - keyboardFrame.maxX)
      case .portraitUpsideDown:
        break
      default:
        break
      }
      UIView.animate(withDuration: TimeInterval(truncating: duration), delay: 0.0,
                     options: UIViewAnimationOptions(rawValue: UIViewAnimationOptions.RawValue(truncating: curve!)),
                     animations: {() -> Void in
        self.view.frame = newFrame
      }, completion: nil)
    }
  }
  
  // MARK: - Responding to Keyman notifications
  
  private func keyboardPickerDismissed() {
    textView1.becomeFirstResponder()
  }
  
  func showActivityIndicator() {
    if parent?.view.viewWithTag(-1) != nil {
      return
    }
    let activityView = UIActivityIndicatorView(activityIndicatorStyle: .whiteLarge)
    let containerView = UIView(frame: activityView.bounds.insetBy(dx: -10.0, dy: -10.0))
    containerView.backgroundColor = UIColor(red: 0.0, green: 1.0, blue: 0.0, alpha: 0.75)
    containerView.layer.cornerRadius = 6.0
    containerView.center = view.center
    containerView.tag = -1
    containerView.autoresizingMask =
    [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin, .flexibleBottomMargin]
    activityView.center = CGPoint(x: containerView.bounds.size.width * 0.5, y: containerView.bounds.size.height * 0.5)
    activityView.startAnimating()
    containerView.addSubview(activityView)
    parent?.view.addSubview(containerView)
  }
  
  @objc func dismissActivityIndicator() {
    parent?.view.viewWithTag(-1)?.removeFromSuperview()
  }
  
  @objc func showAlert(_ message: String) {
    let engineBundle = Bundle(for: Manager.self)
    let alertController = UIAlertController(title: NSLocalizedString("alert-download-error-title", bundle: engineBundle, comment: ""),
                                            message: message,
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: NSLocalizedString("command-ok", bundle: engineBundle, comment: ""),
                                            style: UIAlertActionStyle.default,
                                            handler: nil))
    self.present(alertController, animated: true, completion: nil)
  }
}
