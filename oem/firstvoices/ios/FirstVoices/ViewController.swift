//
//  ViewController.swift
//  FirstVoices app
//
//  License: MIT
//
//  Copyright © 2019 FirstVoices.
//
//  Created by Serkan Kurt on 17/11/2015.
//  Converted and rewritten by Marc Durdin on 15/05/2019.
//

import UIKit
import KeymanEngine

class ViewController: UIViewController, UIWebViewDelegate {

  @IBOutlet var webView: UIWebView!
  @IBOutlet var statusBarBackground: UIView!

  override func viewDidLoad() {
    super.viewDidLoad()

    //
    // Show Instructions page
    //

    self.webView!.delegate = self
    let filePath: String? = Bundle.main.path(forResource: FVConstants.instructionsName,
                                             ofType: FVConstants.instructionsType,
                                             inDirectory: FVConstants.instructionsPath)
    self.webView!.loadRequest(URLRequest.init(url: URL.init(fileURLWithPath: filePath!)))
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    navigationController?.isNavigationBarHidden = true
    self.setStatusBarBackgroundWithOrientation(UIApplication.shared.statusBarOrientation)
  }

  override func didReceiveMemoryWarning() {
    super.didReceiveMemoryWarning()
    // Dispose of any resources that can be recreated.
  }

  func willRotate(_ to: UIInterfaceOrientation, duration: TimeInterval) {
    self.setStatusBarBackgroundWithOrientation(to)
  }

  func setStatusBarBackgroundWithOrientation(_ orientation: UIInterfaceOrientation) {
    if orientation.isPortrait {
      self.statusBarBackground?.isHidden = false
    } else {
      self.statusBarBackground?.isHidden = (UIDevice.current.userInterfaceIdiom == UIUserInterfaceIdiom.phone)
    }
  }

  func webView(_ webView: UIWebView, didFailLoadWithError error: Error) {
    //NSLog([NSString stringWithFormat:@"Error : %@", error]);
  }

  func webView(_ webView: UIWebView, shouldStartLoadWith request: URLRequest, navigationType: UIWebView.NavigationType) -> Bool {
    let fragment: String? = request.url!.fragment
    if fragment?.range(of: "showKeyboards") != nil {
      self.performSegue(withIdentifier: "keyboardList", sender: nil)
 //     self.performSegue(withIdentifier: "Keyboards", sender: nil)
      return false
    }
    if navigationType == .linkClicked {
      UIApplication.shared.openURL(request.url!)
      return false
    }
    return true
  }

}

