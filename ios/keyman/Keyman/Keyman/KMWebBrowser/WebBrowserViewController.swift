//
//  WebBrowserViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation
import UIKit

class WebBrowserViewController: UIViewController, UIWebViewDelegate, UIAlertViewDelegate {
  @IBOutlet var webView: UIWebView!
  @IBOutlet var navBar: UINavigationBar!
  @IBOutlet var toolBar: UIToolbar!
  @IBOutlet var navBarTopConstraint: NSLayoutConstraint!
  var addressField: UITextField!
  var rightView: UIView!
  var refreshButton: UIButton!
  var stopButton: UIButton!
  @IBOutlet var backButton: UIBarButtonItem!
  @IBOutlet var forwardButton: UIBarButtonItem!
  @IBOutlet var bookmarksButton: UIBarButtonItem!
  @IBOutlet var globeButton: UIBarButtonItem!
  @IBOutlet var closeButton: UIBarButtonItem!

  var fontFamily: String = UIFont.systemFont(ofSize: UIFont.systemFontSize).fontName
  var newFontFamily: String = ""

  let webBrowserLastURLKey = "KMWebBrowserLastURL"

  override func viewDidLoad() {
    super.viewDidLoad()

    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardChanged), name: NSNotification.Name.keymanKeyboardChanged, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardPickerDismissed), name: NSNotification.Name.keymanKeyboardPickerDismissed, object: nil)
    
    webView.delegate = self
    webView.scalesPageToFit = true

    // Setup NavigationBar
    let screenRect = UIScreen.main.bounds
    let orientation = UIApplication.shared.statusBarOrientation
    if UIDevice.current.userInterfaceIdiom == .phone {
      let size = CGFloat.maximum(screenRect.size.height, screenRect.size.width)
      if size > 568.0 {
        // Navbar for iPhone 6 & 6 Plus
        let bgImg: UIImage?
        if UIInterfaceOrientationIsPortrait(orientation) {
          bgImg = UIImage(named: "kmwb-navbar-Portrait.png")?.resizableImage(withCapInsets: UIEdgeInsetsMake(0, 0, 0, 0), resizingMode: .stretch)
        }
        else {
          bgImg = UIImage(named: "kmwb-navbar-Landscape-568h.png")?.resizableImage(withCapInsets: UIEdgeInsetsMake(0, 0, 0, 0), resizingMode: .stretch)
        }
        navBar.setBackgroundImage(bgImg, for: UIBarMetrics.default)
      }
      else if size == 568 {
        // Navbar for iPhone and iPod Touch with 4" Display
        navBar.setBackgroundImage(UIImage(named: "kmwb-navbar-Portrait.png"), for: UIBarMetrics.default)
        navBar.setBackgroundImage(UIImage(named: "kmwb-navbar-Landscape-568h.png"), for: UIBarMetrics.compact)
      }
      else if size < 568.0 {
        // Navbar for iPhone and iPod Touch with 3.5" Display
        navBar.setBackgroundImage(UIImage(named: "kmwb-navbar-Portrait.png"), for: UIBarMetrics.default)
        navBar.setBackgroundImage(UIImage(named: "kmwb-navbar-Landscape.png"), for: UIBarMetrics.compact)
      }
    }
    else {
      // Navbar for iPad
      if UIInterfaceOrientationIsPortrait(orientation) {
        navBar.setBackgroundImage(UIImage(named: "kmwb-navbar-Portrait.png"), for: UIBarMetrics.default)
      }
      else {
        navBar.setBackgroundImage(UIImage(named: "kmwb-navbar-Landscape.png"), for: UIBarMetrics.default)
      }
    }

    // Setup address field
    let addressFrame = CGRect(x: 10, y: 4.0, width: navBar.frame.size.width - 20, height: 28)
    addressField = UITextField(frame: addressFrame)
    addressField.clearButtonMode = .whileEditing
    addressField.rightViewMode = .unlessEditing
    addressField.autoresizingMask = .flexibleWidth
    addressField.borderStyle = .roundedRect
    addressField.autocapitalizationType = .none
    addressField.autocorrectionType = .no
    addressField.font = UIFont.systemFont(ofSize: 17)
    addressField.placeholder = "http://"
    addressField.text = ""
    addressField.keyboardType = UIKeyboardType.URL
    addressField.addTarget(self, action: #selector(self.loadAddress), for: .editingDidEndOnExit)
    navBar.addSubview(addressField)

    rightView = UIView(frame: CGRect(x: 0, y: 0, width: 40, height: 28))
    addressField.rightView = rightView

    let refreshSize = CGSize(width: 18, height: 22)
    let refreshIcon = UIImage(named: "UIButtonBarRefresh.png")?.resize(to: refreshSize).withRenderingMode(.alwaysOriginal)
    refreshButton = UIButton(type: UIButtonType.system)
    refreshButton.setImage(refreshIcon, for: .normal)
    refreshButton.frame = rightView.frame
    refreshButton.addTarget(self, action: #selector(self.refresh), for: .touchUpInside)
    refreshButton.isHidden = true
    rightView.addSubview(refreshButton)

    let stopSize = CGSize(width: 17, height: 17)
    let stopIcon = UIImage(named: "UIButtonBarStop.png")?.resize(to: stopSize).withRenderingMode(.alwaysOriginal)
    stopButton = UIButton(type: UIButtonType.system)
    stopButton.setImage(stopIcon, for: .normal)
    stopButton.frame = rightView.frame
    stopButton.addTarget(self, action: #selector(self.stop), for: .touchUpInside)
    stopButton.isHidden = true
    rightView.addSubview(stopButton)

    updateButtons()

    let userData = UserDefaults.standard
    let lastUrlStr = userData.object(forKey: webBrowserLastURLKey) as? String ?? "https://www.google.com/"
    if let url = URL(string: lastUrlStr) {
      let request = URLRequest(url: url)
      webView.loadRequest(request)
    }
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    navBarTopConstraint.constant = AppDelegate.statusBarHeight()
  }

  deinit {
    NotificationCenter.default.removeObserver(self)
  }

  override func willRotate(to toInterfaceOrientation: UIInterfaceOrientation, duration: TimeInterval) {
    let orientation: UIInterfaceOrientation = toInterfaceOrientation
    if UIDevice.current.userInterfaceIdiom == .phone {
      let screenRect: CGRect = UIScreen.main.bounds
      let size: CGFloat = screenRect.size.height > screenRect.size.width ? screenRect.size.height : screenRect.size.width
      if size > 568.0 {
        // Navbar for iPhone 6 & 6 Plus
        var bgImg: UIImage? = nil
        if UIInterfaceOrientationIsPortrait(orientation) {
          bgImg = UIImage(named: "kmwb-navbar-Portrait.png")?.resizableImage(withCapInsets: UIEdgeInsetsMake(0, 0, 0, 0), resizingMode: .stretch)
        }
        else {
          bgImg = UIImage(named: "kmwb-navbar-Landscape-568h.png")?.resizableImage(withCapInsets: UIEdgeInsetsMake(0, 0, 0, 0), resizingMode: .stretch)
        }
        navigationController?.navigationBar.setBackgroundImage(bgImg, for: UIBarMetrics.default)
      }
    }
    else {
      if UIInterfaceOrientationIsPortrait(orientation) {
        navigationController?.navigationBar.setBackgroundImage(UIImage(named: "kmwb-navbar-Portrait.png"), for: UIBarMetrics.default)
      }
      else {
        navigationController?.navigationBar.setBackgroundImage(UIImage(named: "kmwb-navbar-Landscape.png"), for: UIBarMetrics.default)
      }
    }
  }

  override func didRotate(from fromInterfaceOrientation: UIInterfaceOrientation) {
    navBarTopConstraint.constant = AppDelegate.statusBarHeight()
  }

  func loadAddress(_ sender: Any, event: UIEvent) {
    guard let urlString = addressField?.text, var url = URL(string: urlString) else {
      NSLog("Attempting to load invalid URL: %@", addressField?.text ?? "nil")
      return
    }
    if url.scheme == nil {
      url = URL(string: "http://\(urlString)") ?? url
    }
    let request = URLRequest(url: url)
    webView.loadRequest(request)
  }

  func updateAddress(_ request: URLRequest) {
    let url: URL? = request.mainDocumentURL
    addressField.text = url?.absoluteString
    let userData = UserDefaults.standard
    userData.set(url?.absoluteString, forKey: "KMWebBrowserLastURL")
    userData.synchronize()
  }

  func refresh(_ sender: Any) {
    webView.reload()
  }

  func stop(_ sender: Any?) {
    webView.stopLoading()
  }

  @IBAction func back(_ sender: Any) {
    webView.goBack()
  }

  @IBAction func forward(_ sender: Any) {
    webView.goForward()
  }

  @IBAction func bookmarks(_ sender: Any) {
    let bookmarksVC = BookmarksViewController()
    bookmarksVC.webBrowser = self
    present(bookmarksVC, animated: true, completion: nil)
  }

  @IBAction func globe(_ sender: Any) {
    KMManager.sharedInstance().showKeyboardPicker(in: self, shouldAddKeyboard: false)
  }

  func close(_ sender: Any?) {
    dismiss(animated: true, completion: nil)
  }

  // MARK: - UIWebViewDelegate methods
  func webView(_ webView: UIWebView, shouldStartLoadWith request: URLRequest, navigationType: UIWebViewNavigationType) -> Bool {
    updateAddress(request)
    return true
  }

  func webViewDidStartLoad(_ webView: UIWebView) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = true
    updateButtons()
  }

  func webViewDidFinishLoad(_ webView: UIWebView) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = false
    appendCSSFontFamily()
    updateButtons()
  }

  func webView(_ webView: UIWebView, didFailLoadWithError error: Error) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = false
    updateButtons()
    let alert = UIAlertView(title: "Cannot Open Page",
                            message: error.localizedDescription,
                            delegate: self,
                            cancelButtonTitle: "OK",
                            otherButtonTitles: "")
    alert.show()
  }

  func updateButtons() {
    refreshButton.isHidden = webView.isLoading
    stopButton.isHidden = !webView.isLoading
    backButton.isEnabled = webView.canGoBack
    forwardButton.isEnabled = webView.canGoForward
  }

  func appendCSSFontFamily() {
    let jsStr: String = "var style = document.createElement('style');" +
      "style.type = 'text/css';" +
      "style.innerHTML = '*{font-family:\"\(fontFamily)\" !important;}';" +
    "document.getElementsByTagName('head')[0].appendChild(style);"
    webView?.stringByEvaluatingJavaScript(from: jsStr)
  }

  func keyboardChanged(_ notification: Notification) {
    let kbInfo = notification.userInfo?[kKeymanKeyboardInfoKey] as? [AnyHashable: Any] ?? [AnyHashable: Any]()
    if let kbID = kbInfo[kKeymanKeyboardIdKey] as? String, let langID = kbInfo[kKeymanLanguageIdKey] as? String {
      newFontFamily = KMManager.sharedInstance().fontNameForKeyboard(withID: kbID, languageID: langID)
    }
    else {
      newFontFamily = UIFont.systemFont(ofSize: UIFont.systemFontSize).fontName
    }
  }

  func keyboardPickerDismissed(_ notification: Notification) {
    if !(newFontFamily == fontFamily) {
      fontFamily = newFontFamily
      webView?.reload()
    }
  }
}

extension UIImage {
  func resize(to newSize: CGSize) -> UIImage {
    let horizontalRatio = newSize.width / size.width
    let verticalRatio = newSize.height / size.height

    let ratio = max(horizontalRatio, verticalRatio)
    let newSize = CGSize(width: size.width * ratio, height: size.height * ratio)
    UIGraphicsBeginImageContextWithOptions(newSize, true, 0)
    draw(in: CGRect(origin: CGPoint(x: 0, y: 0), size: newSize))
    let newImage = UIGraphicsGetImageFromCurrentImageContext()
    UIGraphicsEndImageContext()
    return newImage!
  }
}
