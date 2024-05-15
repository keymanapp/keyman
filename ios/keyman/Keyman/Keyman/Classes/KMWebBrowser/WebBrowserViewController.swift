//
//  WebBrowserViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import WebKit
import Sentry

class WebBrowserViewController: UIViewController, WKNavigationDelegate, UIAlertViewDelegate {
  @IBOutlet var webView: WKWebView!
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
  var fontKeyboard: InstallableKeyboard?
  private var newFontKeyboard: InstallableKeyboard?
  
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
    
    webView.navigationDelegate = self
    
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
      webView.load(request)
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
    webView.load(request)
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
  
  func webView(_ webView: WKWebView,
               decidePolicyFor navigationAction: WKNavigationAction,
               decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
    let request = navigationAction.request
    if request.url?.lastPathComponent.hasSuffix(".kmp") ?? false {
      // Can't have the browser auto-download with no way to select a different page.
      // Can't just ignore it, either, as the .kmp may result from a redirect from
      // the previous URL.  (Like if using the keyman.com keyboard search!)
      let userData = UserDefaults.standard
      userData.set(nil as String?, forKey: webBrowserLastURLKey)
      userData.synchronize()
      
      // The user is trying to download a .kmp, but the standard
      // WKWebView can't handle it properly.
      
      ResourceDownloadManager.shared.downloadRawKMP(from: request.url!) { file, error in
        // do something!
        if let error = error {
          let alertTitle = NSLocalizedString("alert-error-title", bundle: Bundle(for: Manager.self), comment: "")
          let alert = ResourceFileManager.shared.buildSimpleAlert(title: alertTitle,
                                                                  message: error.localizedDescription)
          self.present(alert, animated: true, completion: nil)
          return
        }
        
        // Re-use the standard 'open random file' code as when launching the
        // app from a file.  This will also auto-dismiss the browser, returning
        // to the app's main screen.
        if let file = file {
          let appDelegate = UIApplication.shared.delegate as! AppDelegate
          _ = appDelegate.application(UIApplication.shared, open: file)
        }
      }
      
      decisionHandler(WKNavigationActionPolicy.cancel)
      return
    }
    updateAddress(request)
    decisionHandler(WKNavigationActionPolicy.allow)
    return
  }
  
  func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = true
    updateButtons()
  }
  
  func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
    UIApplication.shared.isNetworkActivityIndicatorVisible = false
    appendCSSFontFamily()
    updateButtons()
  }
  
  func webView(_ webView: WKWebView, didFailNavigation error: Error) {
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
      let alertController = UIAlertController(title: NSLocalizedString("error-opening-page", comment: ""),
                                              message: error.localizedDescription,
                                              preferredStyle: UIAlertController.Style.alert)
      alertController.addAction(UIAlertAction(title: NSLocalizedString("command-ok",
                                                                       bundle: Bundle(for: Manager.self),
                                                                       comment: ""),
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
    guard let fontKeyboard = fontKeyboard else {
      return
    }
    
    let styleFontFamily = "KeymanEmbeddedBrowserFont"
    
    guard let fontFaceStyle = buildFontSheet(keyboard: fontKeyboard, styleName: styleFontFamily) else {
      return
    }
    
    let jsStr: String = """
    var style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = `
    * {
      font-family:\"\(styleFontFamily)\" !important;
    }
    
    \(fontFaceStyle)
    `;
    
    document.getElementsByTagName('head')[0].appendChild(style);
    """
    
    webView.evaluateJavaScript(jsStr)
    
  }
  
  private func buildFontSheet(keyboard: InstallableKeyboard, styleName: String) -> String? {
    guard let fontKeyboard = fontKeyboard,
          let fontURL = Manager.shared.fontPathForKeyboard(withFullID: fontKeyboard.fullID) else {
      return nil
    }
    
    guard let fontData = NSData(contentsOf: fontURL) else {
      return nil
    }
    
    var dataType: String = ""
    var fontFormat: String = ""
    switch fontURL.pathExtension {
    case "ttf":
      dataType = "font/truetype"
      fontFormat = "truetype"
    case "otf":
      dataType = "font/opentype"
      fontFormat = "opentype"
      // The following two entries are here for completeness.  At present, we do not
      // actually distribute these in .kmp packages; we only include one font within them,
      // and these two are web-oriented, not main-OS oriented.
    case "woff":
      dataType = "font/woff"
      fontFormat = "woff"
    case "woff2":
      dataType = "font/woff2"
      fontFormat = "woff2"
    default:
      return nil
    }
    
    let styleString = """
    @font-face {
      font-family: "\( styleName )";
      src: url(data:\(dataType);charset=utf-8;base64,\(fontData.base64EncodedString())); format('\(fontFormat)')
    }
    """
    
    return styleString
  }
  
  private func keyboardChanged(_ kb: InstallableKeyboard) {
    if kb.font != nil {
      newFontKeyboard = kb
    } else {
      newFontKeyboard = nil
    }
  }
  
  private func keyboardPickerDismissed() {
    if newFontKeyboard?.font?.family != fontKeyboard?.font?.family {
      fontKeyboard = newFontKeyboard
      webView?.reload()
    }
  }
}
