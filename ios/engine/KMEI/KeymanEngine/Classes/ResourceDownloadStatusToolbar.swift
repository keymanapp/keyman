//
//  ResourceDownloadStatusToolbar.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 8/14/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import UIKit

public class ResourceDownloadStatusToolbar: UIToolbar {
  private var statusLabel: UILabel?
  private var activityIndicator: UIActivityIndicatorView?
  private var actionButton: UIButton?
  
  private var _navigationController: UINavigationController?
  
  override init(frame: CGRect) {
    super.init(frame: frame)
    
    setup()
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
  
  private func setup() {
    barTintColor = Colors.statusToolbar
  }
  
  /**
   * Creates a simple text label for indicating current status without allowing interactivity.
   */
  private func setupStatusLabel() -> UILabel {
    let labelFrame = CGRect(origin: frame.origin,
                            size: CGSize(width: frame.width * 0.95,
                                         height: frame.height * 0.7))
    let label = UILabel(frame: labelFrame)
    label.backgroundColor = UIColor.clear
    label.textColor = UIColor.white
    label.textAlignment = .center
    label.center = CGPoint(x: frame.width * 0.5, y: frame.height * 0.5)
    label.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                              .flexibleBottomMargin, .flexibleWidth, .flexibleHeight]
    
    return label
  }
  
  /**
   * Creates the classic spinning-wheel 'activity in progress' indicator.
   */
  private func setupActivityIndicator() -> UIActivityIndicatorView {
    let indicatorView = UIActivityIndicatorView(style: .gray)
    indicatorView.center = CGPoint(x: frame.width - indicatorView.frame.width,
                                   y: frame.height * 0.5)
    indicatorView.autoresizingMask = [.flexibleLeftMargin, .flexibleTopMargin, .flexibleBottomMargin]
    indicatorView.startAnimating()
    
    return indicatorView
  }
  
  private func setupActionButton(_ text: String, for handler: Any?, onClick: Selector) -> UIButton {
    let button = UIButton(type: .roundedRect)
    button.addTarget(handler, action: onClick, for: .touchUpInside)
    
    button.frame = CGRect(x: frame.origin.x, y: frame.origin.y,
                          width: frame.width * 0.95, height: frame.height * 0.7)
    button.center = CGPoint(x: frame.width / 2, y: frame.height / 2)
    button.tintColor = Colors.statusResourceUpdateButton
    button.setTitleColor(UIColor.white, for: .normal)
    button.setTitle(text, for: .normal)
    button.autoresizingMask =  [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                                .flexibleBottomMargin, .flexibleWidth, .flexibleHeight]
    
    return button
  }
  
  private func clearSubviews() {
    statusLabel?.removeFromSuperview()
    activityIndicator?.removeFromSuperview()
    actionButton?.removeFromSuperview()
  }
  
  public func displayStatus(_ text: String, withIndicator: Bool, duration: TimeInterval? = nil) {
    clearSubviews()
    
    statusLabel = setupStatusLabel()
    statusLabel!.text = text
    addSubview(statusLabel!)
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
  
  public func displayButton(_ text: String, with target: Any?, callback: Selector) {
    clearSubviews()
    
    // This forces iOS to create any intervening default UIToolbar subviews that may exist by default.
    // Anything added afterward will be placed on top of those, enabling interactivity.
    layoutIfNeeded()
    
    actionButton = setupActionButton(text, for: target, onClick: callback)
    addSubview(actionButton!)
    bringSubviewToFront(actionButton!)
    
    // Automatically display if we're hidden and have access to our owning UINavigationController.
    if navigationController?.isToolbarHidden ?? false {
      navigationController!.setToolbarHidden(false, animated: true)
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
