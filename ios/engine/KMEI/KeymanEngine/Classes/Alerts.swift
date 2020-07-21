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

open class Alerts {
  public typealias AcceptanceHandler = ((UIAlertAction)) -> Void

  public static func showErrorAlert(in vc: UIViewController, title: String, msg: String, handler: @escaping AcceptanceHandler) {
    let alertController = UIAlertController(title: title,
                                            message: msg,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertAction.Style.default,
                                            handler: handler))

    vc.present(alertController, animated: true, completion: nil)
  }

  public static func showConnectionErrorAlert(in vc: UIViewController, handler: @escaping AcceptanceHandler) {
    showErrorAlert(in: vc, title: "Connection Error", msg: "Could not reach Keyman server.  Please try again later.", handler: handler)
  }

  public static func showDownloadErrorAlert(in vc: UIViewController, handler: @escaping AcceptanceHandler) {
    var networkReachable: Reachability?
    do {
      try networkReachable = Reachability(hostname: KeymanHosts.KEYMAN_COM.host!)
    } catch {
      log.debug("reachability could not start")
    }

    if networkReachable?.connection == Reachability.Connection.unavailable || networkReachable == nil {
      showConnectionErrorAlert(in: vc, handler: handler)
    } else {
      // Show a different alert!
      showErrorAlert(in: vc, title: "Download error", msg: "Error occurred during download or installation.", handler: handler)
    }
  }

  public static func popToNavigationRootHandler(for vc: UIViewController) -> AcceptanceHandler {
    return { alertAction in
      vc.navigationController?.popToRootViewController(animated: true)
    }
  }
}
