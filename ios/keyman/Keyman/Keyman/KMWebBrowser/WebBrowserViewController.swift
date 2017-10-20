//
//  WebBrowserViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

class WebBrowserViewController: UIViewController, UIWebViewDelegate, UIAlertViewDelegate {
  @IBOutlet var webView: UIWebView!
  @IBOutlet var navBar: UINavigationBar!
  @IBOutlet var toolBar: UIToolbar!
  @IBOutlet var navBarTopConstraint: NSLayoutConstraint!

  private var addressField: UITextField!
  private var rightView: UIView!
  private var refreshButton: UIButton!
  private var stopButton: UIButton!

  @IBOutlet var backButton: UIBarButtonItem!
  @IBOutlet var forwardButton: UIBarButtonItem!
  @IBOutlet var bookmarksButton: UIBarButtonItem!
  @IBOutlet var globeButton: UIBarButtonItem!
  @IBOutlet var closeButton: UIBarButtonItem!

  var fontFamily = UIFont.systemFont(ofSize: UIFont.systemFontSize).fontName
  private var newFontFamily = ""

  private let webBrowserLastURLKey = "KMWebBrowserLastURL"

  override func viewDidLoad() {
    super.viewDidLoad()

    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardChanged),
        name: NSNotification.Name.keymanKeyboardChanged, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardPickerDismissed),
        name: NSNotification.Name.keymanKeyboardPickerDismissed, object: nil)

    webView.delegate = self
    webView.scalesPageToFit = true

    // Setup NavigationBar
    let screenRect = UIScreen.main.bounds
    let orientation = UIApplication.shared.statusBarOrientation
    if UIDevice.current.userInterfaceIdiom == .phone {
      let size = CGFloat.maximum(screenRect.size.height, screenRect.size.width)
      if size > 568.0 {
        // Navbar for iPhone 6 & 6 Plus
        let image: UIImage
        if UIInterfaceOrientationIsPortrait(orientation) {
          image = #imageLiteral(resourceName: "kmwb-navbar-Portrait.png")
        } else {
          image = #imageLiteral(resourceName: "kmwb-navbar-Landscape-568h.png")
        }
        let bgImg = image.resizableImage(
          withCapInsets: UIEdgeInsets(top: 0, left: 0, bottom: 0, right: 0), resizingMode: .stretch)
        navBar.setBackgroundImage(bgImg, for: UIBarMetrics.default)
      } else if size == 568 {
        // Navbar for iPhone and iPod Touch with 4" Display
        navBar.setBackgroundImage(#imageLiteral(resourceName: "kmwb-navbar-Portrait.png"),
                                  for: UIBarMetrics.default)
        navBar.setBackgroundImage(#imageLiteral(resourceName: "kmwb-navbar-Landscape-568h.png"),
                                  for: UIBarMetrics.compact)
      } else if size < 568.0 {
        // Navbar for iPhone and iPod Touch with 3.5" Display
        navBar.setBackgroundImage(#imageLiteral(resourceName: "kmwb-navbar-Portrait.png"),
                                  for: UIBarMetrics.default)
        navBar.setBackgroundImage(#imageLiteral(resourceName: "kmwb-navbar-Landscape.png"),
                                  for: UIBarMetrics.compact)
      }
    } else {
      // Navbar for iPad
      if UIInterfaceOrientationIsPortrait(orientation) {
        navBar.setBackgroundImage(#imageLiteral(resourceName: "kmwb-navbar-Portrait.png"), for: UIBarMetrics.default)
      } else {
        navBar.setBackgroundImage(#imageLiteral(resourceName: "kmwb-navbar-Landscape.png"), for: UIBarMetrics.default)
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
    let refreshIcon = #imageLiteral(resourceName: "UIButtonBarRefresh.png").resize(to: refreshSize)
      .withRenderingMode(.alwaysOriginal)
    refreshButton = UIButton(type: UIButtonType.system)
    refreshButton.setImage(refreshIcon, for: .normal)
    refreshButton.frame = rightView.frame
    refreshButton.addTarget(self, action: #selector(self.refresh), for: .touchUpInside)
    refreshButton.isHidden = true
    rightView.addSubview(refreshButton)

    let stopSize = CGSize(width: 17, height: 17)
    let stopIcon = #imageLiteral(resourceName: "UIButtonBarStop.png").resize(to: stopSize)
      .withRenderingMode(.alwaysOriginal)
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
      let screenRect = UIScreen.main.bounds
      let size = CGFloat.maximum(screenRect.size.width, screenRect.size.height)
      if size > 568.0 {
        // TODO: Refactor since duplicated in viewDidLoad()
        // Navbar for iPhone 6 & 6 Plus
        let image: UIImage
        if UIInterfaceOrientationIsPortrait(orientation) {
          image = #imageLiteral(resourceName: "kmwb-navbar-Portrait.png")
        } else {
          image = #imageLiteral(resourceName: "kmwb-navbar-Landscape-568h.png")
        }
        let bgImg = image.resizableImage(
          withCapInsets: UIEdgeInsets(top: 0, left: 0, bottom: 0, right: 0), resizingMode: .stretch)
        navigationController?.navigationBar.setBackgroundImage(bgImg, for: UIBarMetrics.default)
      }
    } else {
      let image: UIImage
      if UIInterfaceOrientationIsPortrait(orientation) {
        image = #imageLiteral(resourceName: "kmwb-navbar-Portrait.png")
      } else {
        image = #imageLiteral(resourceName: "kmwb-navbar-Landscape.png")
      }
      navigationController?.navigationBar.setBackgroundImage(image, for: UIBarMetrics.default)
    }
  }

  override func didRotate(from fromInterfaceOrientation: UIInterfaceOrientation) {
    navBarTopConstraint.constant = AppDelegate.statusBarHeight()
  }

  @objc func loadAddress(_ sender: Any, event: UIEvent) {
    guard let urlString = addressField?.text, var url = URL(string: urlString) else {
      #if DEBUG
        NSLog("Attempting to load invalid URL: %@", addressField?.text ?? "nil")
      #endif
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

  @objc func refresh(_ sender: Any) {
    webView.reload()
  }

  @objc func stop(_ sender: Any?) {
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
    Manager.shared.showKeyboardPicker(in: self, shouldAddKeyboard: false)
  }

  @objc func close(_ sender: Any?) {
    dismiss(animated: true, completion: nil)
  }

  func webView(_ webView: UIWebView,
               shouldStartLoadWith request: URLRequest,
               navigationType: UIWebViewNavigationType) -> Bool {
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

  private func updateButtons() {
    refreshButton.isHidden = webView.isLoading
    stopButton.isHidden = !webView.isLoading
    backButton.isEnabled = webView.canGoBack
    forwardButton.isEnabled = webView.canGoForward
  }

  private func appendCSSFontFamily() {
    let jsStr: String = "var style = document.createElement('style');" +
      "style.type = 'text/css';" +
      "style.innerHTML = '*{font-family:\"\(fontFamily)\" !important;}';" +
    "document.getElementsByTagName('head')[0].appendChild(style);"
    webView?.stringByEvaluatingJavaScript(from: jsStr)
  }

  @objc func keyboardChanged(_ notification: Notification) {
    let kbInfo = notification.userInfo?[kKeymanKeyboardInfoKey] as? [AnyHashable: Any] ?? [AnyHashable: Any]()
    if let kbID = kbInfo[kKeymanKeyboardIdKey] as? String,
       let langID = kbInfo[kKeymanLanguageIdKey] as? String,
       let fontName = Manager.shared.fontNameForKeyboard(withID: kbID, languageID: langID) {
      newFontFamily = fontName
    } else {
      newFontFamily = UIFont.systemFont(ofSize: UIFont.systemFontSize).fontName
    }
  }

  @objc func keyboardPickerDismissed(_ notification: Notification) {
    if newFontFamily != fontFamily {
      fontFamily = newFontFamily
      webView?.reload()
    }
  }
}
