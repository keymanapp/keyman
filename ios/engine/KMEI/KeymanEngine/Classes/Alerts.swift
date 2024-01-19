//
//  Alerts.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 1/27/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit
import Reachability
import os.log

open class Alerts {
  public typealias AcceptanceHandler = ((UIAlertAction)) -> Void

  public static func constructActivitySpinner() -> UIActivityIndicatorView {
    let activitySpinner: UIActivityIndicatorView
    activitySpinner = UIActivityIndicatorView(style: .whiteLarge)
    activitySpinner.hidesWhenStopped = true
    activitySpinner.translatesAutoresizingMaskIntoConstraints = false
    activitySpinner.backgroundColor = Colors.spinnerBackground
    activitySpinner.layer.cornerRadius = 6.0
    activitySpinner.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                                     .flexibleBottomMargin]
    return activitySpinner
  }

  public static func showErrorAlert(in vc: UIViewController, title: String, msg: String, handler: @escaping AcceptanceHandler) {
    let alertController = UIAlertController(title: title,
                                            message: msg,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: NSLocalizedString("command-ok", bundle: engineBundle, comment: ""),
                                            style: UIAlertAction.Style.default,
                                            handler: handler))

    vc.present(alertController, animated: true, completion: nil)
  }

  public static func showConnectionErrorAlert(in vc: UIViewController, handler: @escaping AcceptanceHandler) {
    showErrorAlert(in: vc,
                   title: NSLocalizedString("alert-no-connection-title", bundle: engineBundle, comment: ""),
                   msg: NSLocalizedString("alert-no-connection-detail", bundle: engineBundle, comment: ""),
                   handler: handler)
  }

  public static func showDownloadErrorAlert(in vc: UIViewController, handler: @escaping AcceptanceHandler) {
    var networkReachable: Reachability?
    do {
      try networkReachable = Reachability(hostname: KeymanHosts.KEYMAN_COM.host!)
    } catch {
      os_log("Reachability could not start", log:KeymanEngineLogger.ui, type: .debug)
   }

    if networkReachable?.connection == Reachability.Connection.unavailable || networkReachable == nil {
      showConnectionErrorAlert(in: vc, handler: handler)
    } else {
      // Show a different alert!
      showErrorAlert(in: vc,
                     title: NSLocalizedString("alert-download-error-title", bundle: engineBundle, comment: ""),
                     msg: NSLocalizedString("alert-download-error-detail", bundle: engineBundle, comment: ""),
                     handler: handler)
    }
  }

  public static func popToNavigationRootHandler(for vc: UIViewController) -> AcceptanceHandler {
    return { alertAction in
      vc.navigationController?.popToRootViewController(animated: true)
    }
  }
}
