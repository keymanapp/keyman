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

  var navbarBackground: KMNavigationBarBackgroundView!
  var fontFamily = UIFont.systemFont(ofSize: UIFont.systemFontSize).fontName
  private var newFontFamily = ""

  private let webBrowserLastURLKey = "KMWebBrowserLastURL"

  private var keyboardChangedObserver: NotificationObserver?
  private var keyboardPickerDismissedObserver: NotificationObserver?

  convenience init() {
    self.init(nibName: "WebBrowserViewController", bundle: nil)
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    keyboardChangedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardChanged,
      observer: self,
      function: WebBrowserViewController.keyboardChanged)
    keyboardPickerDismissedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardPickerDismissed,
      observer: self,
      function: WebBrowserViewController.keyboardPickerDismissed)

    webView.delegate = self
    webView.scalesPageToFit = true

    if #available(iOS 13.0, *) {
      // Dark mode settings must be applied through this new property,
      // its class, and others like it.
      navBar.standardAppearance.configureWithOpaqueBackground()
    } else {
      // Fallback on earlier versions
    }

    // Setup NavigationBar
    navbarBackground = KMNavigationBarBackgroundView()
    navbarBackground.hideLogo()
    navbarBackground.addToNavbar(navBar)
    navbarBackground.setOrientation(UIApplication.shared.statusBarOrientation)

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
    refreshButton = UIButton(type: UIButton.ButtonType.system)
    refreshButton.setImage(refreshIcon, for: .normal)
    refreshButton.frame = rightView.frame
    refreshButton.addTarget(self, action: #selector(self.refresh), for: .touchUpInside)
    refreshButton.isHidden = true
    rightView.addSubview(refreshButton)

    let stopSize = CGSize(width: 17, height: 17)
    let stopIcon = #imageLiteral(resourceName: "UIButtonBarStop.png").resize(to: stopSize)
      .withRenderingMode(.alwaysOriginal)
    stopButton = UIButton(type: UIButton.ButtonType.system)
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

    if #available(iOS 13.0, *) {
      // 13.0 auto-adjusts the top to avoid the status bar.
    } else {
      navBarTopConstraint.constant = AppDelegate.statusBarHeight()
    }
  }

  override func willRotate(to toInterfaceOrientation: UIInterfaceOrientation, duration: TimeInterval) {
    if let navbarBackground = navbarBackground {
      navbarBackground.setOrientation(toInterfaceOrientation)
    }
  }

  override func didRotate(from fromInterfaceOrientation: UIInterfaceOrientation) {
    navBarTopConstraint.constant = AppDelegate.statusBarHeight()
  }

  @objc func loadAddress(_ sender: Any, event: UIEvent) {
    if let urlString = addressField?.text {
      loadUrlString(urlString)
    }
  }

  func loadUrlString(_ urlString: String, allowSearchRedirect: Bool  = true) {
    guard var url = URL(string: urlString) else {
      if allowSearchRedirect {
        loadSearchString(urlString)
      }
      return
    }
    if url.scheme == nil {
      url = URL(string: "http://\(urlString)") ?? url
    }
    let request = URLRequest(url: url)
    webView.loadRequest(request)
  }

  func loadSearchString(_ searchString: String) {
    if let query = searchString.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed) {
      loadUrlString("google.com/search?q=\(query)", allowSearchRedirect: false)
    }
  }

  func updateAddress(_ request: URLRequest) {
    let url: URL? = request.mainDocumentURL
    addressField.text = url?.absoluteString
    let userData = UserDefaults.standard
    userData.set(url?.absoluteString, forKey: webBrowserLastURLKey)
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
               navigationType: UIWebView.NavigationType) -> Bool {
    if request.url?.lastPathComponent.hasSuffix(".kmp") ?? false {
      // The user is trying to download a .kmp, but the standard
      // UIWebView can't handle it properly.
      //
      // Instead, we can just forward that URL back into Safari.
      UIApplication.shared.openURL(request.url!)

      return false
    }
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

    var signalError: Bool = true

    // An error will likely result if the user attempts to download a KMP,
    // despite the fact that we tell it not to attempt a load.
    let nsError = error as NSError
    if let url = nsError.userInfo["NSErrorFailingURLKey"] as? NSURL {
      signalError = !(url.path?.hasSuffix(".kmp") ?? false)
    }

    if signalError {
      let alertController = UIAlertController(title: "Cannot Open Page",
                                              message: error.localizedDescription,
                                              preferredStyle: UIAlertController.Style.alert)
      alertController.addAction(UIAlertAction(title: "OK",
                                              style: UIAlertAction.Style.default,
                                              handler: nil))
      self.present(alertController, animated: true, completion: nil)
    }
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

  private func keyboardChanged(_ kb: InstallableKeyboard) {
    if let fontName = Manager.shared.fontNameForKeyboard(withFullID: kb.fullID) {
      newFontFamily = fontName
    } else {
      newFontFamily = UIFont.systemFont(ofSize: UIFont.systemFontSize).fontName
    }
  }

  private func keyboardPickerDismissed() {
    if newFontFamily != fontFamily {
      fontFamily = newFontFamily
      webView?.reload()
    }
  }
}
