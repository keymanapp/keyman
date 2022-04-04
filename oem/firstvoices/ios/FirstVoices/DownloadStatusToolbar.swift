/*
* DownloadStatusToolbar.swift
* FirstVoices app
*
* License: MIT
*
* Copyright © 2022 FirstVoices.
*
* Created by Shawn Schantz on 2022-03-25.
*
* Toolbar class to show activity indicator during downloads.
*
*/

import UIKit

class DownloadStatusToolbar: UIToolbar {
  private var activityIndicator: UIActivityIndicatorView?

  private var _navigationController: UINavigationController?

  override init(frame: CGRect) {
    super.init(frame: frame)
  }

  // Only here because it's required by Swift.
  required init?(coder: NSCoder) {
    super.init(coder: coder)
  }

  public var navigationController: UINavigationController? {
    get {
      return _navigationController
    }
    
    set {
      _navigationController = newValue
    }
  }

  /**
   * Creates the spinner to show that activity is in progress.
   */
  private func setupActivityIndicator() -> UIActivityIndicatorView {
    let indicatorView = UIActivityIndicatorView(style: .gray)
    indicatorView.center = CGPoint(x: frame.width - indicatorView.frame.width,
                                   y: frame.height * 0.5)
    indicatorView.autoresizingMask = [.flexibleLeftMargin, .flexibleTopMargin, .flexibleBottomMargin]
    indicatorView.startAnimating()
    
    return indicatorView
  }

  private func clearSubviews() {
    activityIndicator?.removeFromSuperview()
  }

  /*
  public func startAnimating() {
    self.activityIndicator = self.setupActivityIndicator()
  }
  */
  
  public func hideStatus() {
    if !(navigationController?.isToolbarHidden ?? true) {
      navigationController!.setToolbarHidden(true, animated: true)
    }
  }
  
  public func displayStatus(_ withIndicator: Bool, duration: TimeInterval? = nil) {
    clearSubviews()
    
    if withIndicator {
      activityIndicator = setupActivityIndicator()
      addSubview(activityIndicator!)
    }
    
    // Automatically display if we're hidden and have access to our owning UINavigationController.
    if navigationController?.isToolbarHidden ?? false {
      navigationController!.setToolbarHidden(false, animated: true)
    }
    
    if let timeout = duration {
      hideAfterDelay(timeout: timeout)
    }
  }

  private func hideAfterDelay(timeout: TimeInterval) {
    Timer.scheduledTimer(timeInterval: timeout, target: self, selector: #selector(self.delayedHide),
                         userInfo: nil, repeats: false)
  }

  @objc private func delayedHide() {
    // If the navigation controller exists and the toolbar is not hidden.  The ! + true literal together
    // give false for the condition when the navigation controller does not exist.
    if !(navigationController?.isToolbarHidden ?? true) {
      navigationController!.setToolbarHidden(true, animated: true)
    }
  }
}
