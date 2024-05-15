//
//  MainViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit
import QuartzCore
import os

// Internal strings
private let baseUri = "https://r.keymanweb.com/20/fonts/get_mobileconfig.php?id="
private let profileKey = "profile"
private let checkedProfilesKey = "CheckedProfiles"

// text size constants
private let defaultIPhoneTextSize: CGFloat = 16.0
private let defaultIPadTextSize: CGFloat = 32.0
private let minTextSize: CGFloat = 9.0
private let maxTextSize: CGFloat = 72.0

// External strings
let userTextKey = "UserText"
let userTextSizeKey = "UserTextSize"
let dontShowGetStartedKey = "DontShowGetStarted" // older preference setting name, use shouldShowGetStartedKey
let shouldShowGetStartedKey = "ShouldShowGetStarted"
let shouldReportErrorsKey = "ShouldReportErrors"
let launchedFromUrlNotification = NSNotification.Name("LaunchedFromUrlNotification")
let urlKey = "url"

class MainViewController: UIViewController, TextViewDelegate, UIActionSheetDelegate {
  private let getStartedViewTag = 7183
  private let activityIndicatorViewTag = 6573
  private let dropDownListTag = 686876
  
  var textView: TextView!
  var textSize: CGFloat = 0.0
  var navbarBackground: KMNavigationBarBackgroundView!
  
  private var getStartedVC: GetStartedViewController!
  private var infoView: InfoViewController!
  private var popover: UIViewController?
  private var textSizeController: UISlider!
  private var dropdownItems: [UIBarButtonItem]!
  
  private var profilesToInstall: [String] = []
  private var checkedProfiles: [String] = []
  private var profileName: String?
  private var keyboardToDownload: InstallableKeyboard?
  private var customKeyboardToDownload: URL?
  private var wasKeyboardVisible: Bool = false
  
  private var screenWidth: CGFloat = 0.0
  private var screenHeight: CGFloat = 0.0
  private var portLeftMargin: CGFloat = 0.0
  private var portRightMargin: CGFloat = 0.0
  private var lscpeLeftMargin: CGFloat = 0.0
  private var lscpeRightMargin: CGFloat = 0.0
  private var didKeyboardLoad = false
  
  private var keyboardLoadedObserver: NotificationObserver?
  private var languagesUpdatedObserver: NotificationObserver?
  private var languagesDownloadFailedObserver: NotificationObserver?
  private var keyboardPickerDismissedObserver: NotificationObserver?
  private var keyboardChangedObserver: NotificationObserver?
  private var keyboardRemovedObserver: NotificationObserver?
  
  var appDelegate: AppDelegate! {
    return UIApplication.shared.delegate as? AppDelegate
  }
  
  var overlayWindow: UIWindow! {
    return appDelegate?.overlayWindow
  }
  
  convenience init() {
    if UIDevice.current.userInterfaceIdiom == .phone {
      self.init(nibName: "MainViewController_iPhone", bundle: nil)
    } else {
      self.init(nibName: "MainViewController_iPad", bundle: nil)
    }
  }
  
  override init(nibName nibNameOrNil: String?, bundle nibBundleOrNil: Bundle?) {
    super.init(nibName: nibNameOrNil, bundle: nibBundleOrNil)
    
    // Setup Notifications
    NotificationCenter.default.addObserver(self, selector: #selector(self.onKeyboardWillShow),
                                           name: UIResponder.keyboardWillShowNotification, object: nil)
    // `keyboardDidShow` apparently matches an internal Apple API and will fail an app submission.
    // So, cheap workaround:  just prefix the thing.
    NotificationCenter.default.addObserver(self, selector: #selector(self.onKeyboardDidShow),
                                           name: UIResponder.keyboardDidShowNotification, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.onKeyboardWillHide),
                                           name: UIResponder.keyboardWillHideNotification, object: nil)
    keyboardLoadedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardLoaded,
      observer: self,
      function: MainViewController.keyboardLoaded)
    keyboardChangedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardChanged,
      observer: self,
      function: MainViewController.keyboardChanged)
    keyboardPickerDismissedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardPickerDismissed,
      observer: self,
      function: MainViewController.keyboardPickerDismissed)
    keyboardRemovedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardRemoved,
      observer: self,
      function: MainViewController.keyboardRemoved)
    
    // Unfortunately, it's the main app with the file definitions.
    // We have to gerry-rig this so that the framework-based SettingsViewController
    // can launch the app-based DocumentViewController.
    Manager.shared.fileBrowserLauncher = { navVC in
      let vc = PackageBrowserViewController(documentTypes: ["com.keyman.kmp"],
                                            in: .import,
                                            navVC: navVC)
      
      // Present the "install from file" browser within the specified navigation view controller.
      // (Allows displaying from within the Settings menu)
      navVC.present(vc, animated: true)
    }
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  private func constructAndAddHostedViews() {
    self.constructAndAddMainTextView()
    self.constructAndAddInfoView()
    self.constructAndAddTextSizeController()
    resizeViews(withKeyboardVisible: textView.isFirstResponder)
  }
  
  private func constructAndAddMainTextView() {
    // Setup Keyman TextView
    textView = TextView(frame: view.frame)
    textView.translatesAutoresizingMaskIntoConstraints = false
    textView.setKeymanDelegate(self)
    textView.viewController = self
    textView.backgroundColor = UIColor(named: "InputBackground")!
    textView.isScrollEnabled = true
    textView.isUserInteractionEnabled = true
    view?.addSubview(textView!)
    
    textView.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor).isActive = true
    textView.leftAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leftAnchor).isActive = true
    textView.rightAnchor.constraint(equalTo: view.safeAreaLayoutGuide.rightAnchor).isActive = true
    textView.bottomAnchor.constraint(equalTo: view.safeAreaLayoutGuide.bottomAnchor).isActive = true
  }
  
  private func initializeTextSettings() {
    self.initializeUserText()
    self.initializeTextSize()
  }
  
  private func initializeUserText() {
    let userData = AppDelegate.activeUserDefaults()
    let userText = userData.object(forKey: userTextKey) as? String
    
    if let text = userText, !text.isEmpty {
      self.textView.text = text
    }
  }
  
  private func initializeTextSize() {
    let userData = AppDelegate.activeUserDefaults()
    let userTextSize = userData.object(forKey: userTextSizeKey) as? String
    
    if let sizeString = userTextSize, let size = Float(sizeString) {
      self.textSize = CGFloat(size)
    } else {
      // no textSize saved in UserDefaults, so use default size
      self.textSize = self.calculateDefaultTextSize()
    }
    
    // default to system font so that we have a non-nil font that allows us to set size
    self.textView.font = UIFont.systemFont(ofSize: textSize)
    
    // set slider position
    self.textSizeController.value = (Float(textSize - minTextSize))
  }
  
  public func saveTextSettings() {
    let userData = AppDelegate.activeUserDefaults()
    userData.set(self.textView?.text, forKey: userTextKey)
    userData.set(self.textSize.description, forKey: userTextSizeKey)
    userData.synchronize()
    os_log("saving text size: %{public}s", log: KeymanLogger.settings, type: .debug, textSize.description)
  }
  
  private func calculateDefaultTextSize() -> CGFloat {
    let defaultSize = (UIDevice.current.userInterfaceIdiom == .phone) ?
    defaultIPhoneTextSize : defaultIPadTextSize
    return defaultSize
  }
  
  private func constructAndAddInfoView() {
    // Setup Info View
    infoView = InfoViewController {
      self.infoButtonClick(nil)
    }
    
    infoView.view?.translatesAutoresizingMaskIntoConstraints = false
    infoView.view?.isHidden = true
    infoView.view?.backgroundColor = UIColor(named: "InputBackground")!
    view?.addSubview(infoView!.view)
    
    // Don't forget to add the child view's controller, too!
    // Its safe area layout guide won't work correctly without this!
    self.addChild(infoView)
    
    infoView.view?.topAnchor.constraint   (equalTo: view.safeAreaLayoutGuide.topAnchor).isActive = true
    infoView.view?.leftAnchor.constraint  (equalTo: view.leftAnchor  ).isActive = true
    infoView.view?.rightAnchor.constraint (equalTo: view.rightAnchor ).isActive = true
    infoView.view?.bottomAnchor.constraint(equalTo: view.bottomAnchor).isActive = true
  }
  
  private func constructAndAddTextSizeController() {
    // Setup Text Size Controller
    textSizeController = UISlider()
    textSizeController.frame = CGRect.zero
    textSizeController.minimumValue = 0.0
    textSizeController.maximumValue = Float(maxTextSize - minTextSize)
    textSizeController.value = Float(textSize - minTextSize)
    let textSizeUp = #imageLiteral(resourceName: "textsize_up.png").resize(to: CGSize(width: 20, height: 20))
    textSizeController.maximumValueImage = textSizeUp
    let textSizeDown = #imageLiteral(resourceName: "textsize_up.png").resize(to: CGSize(width: 15, height: 15))
    textSizeController.minimumValueImage = textSizeDown
    textSizeController.addTarget(self, action: #selector(self.sliderValueChanged), for: .valueChanged)
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()
    
    extendedLayoutIncludesOpaqueBars = true
    
    let systemFonts = Set<String>(UIFont.familyNames)
    
    // Setup Keyman Manager & fetch keyboards list
    Manager.shared.canRemoveDefaultKeyboard = true
    
    // Pre-load for use in update checks.
    if !ResourceDownloadManager.shared.updateCacheIsCurrent {
      // TODO:  Actually use the query's results once available.
      //        That said, this is as far as older versions used the query here.
      ResourceDownloadManager.shared.queryKeysForUpdatablePackages { _, _ in }
    }
    
    view?.backgroundColor = UIColor(named: "InputBackground")!
    
    // Check for configuration profiles/fonts to install
    FontManager.shared.registerCustomFonts()
    let kmFonts = FontManager.shared.fonts
    var profilesByFontName: [String: String] = [:]
    for (url, font) in kmFonts where url.lastPathComponent != Resources.oskFontFilename {
      let profile = url.deletingPathExtension().appendingPathExtension("mobileconfig").lastPathComponent
      profilesByFontName[font.name] = profile
    }
    
    let customFonts = UIFont.familyNames.filter { !systemFonts.contains($0) && !($0 == "KeymanwebOsk") }
    
    let userData = AppDelegate.activeUserDefaults()
    checkedProfiles = userData.object(forKey: checkedProfilesKey) as? [String] ?? [String]()
    
    profilesToInstall = [String]()
    for name in customFonts {
      if let profile = profilesByFontName[UIFont.fontNames(forFamilyName: name)[0]] {
        profilesToInstall.append(profile)
      }
    }
    
    // Setup NavigationBar
    if let navbar = navigationController?.navigationBar {
      if #available(iOS 13.0, *) {
        // Dark mode settings must be applied through this new property,
        // its class, and others like it.
        navbar.standardAppearance.configureWithOpaqueBackground()
      } else {
        // Fallback on earlier versions
      }
      navbarBackground = KMNavigationBarBackgroundView()
      navbarBackground.addToNavbar(navbar)
      navbarBackground.setOrientation(UIApplication.shared.statusBarOrientation)
    }
    
    self.constructAndAddHostedViews()
    
    setNavBarButtons()
    self.initializeTextSettings()
  }
  
  override func viewWillDisappear(_ animated: Bool) {
    super.viewWillDisappear(animated)
    wasKeyboardVisible = textView.isFirstResponder
  }
  
  private func setNavBarButtons() {
    let orientation: UIInterfaceOrientation = UIApplication.shared.statusBarOrientation
    let fixedSpace = UIBarButtonItem(barButtonSystemItem: .fixedSpace, target: nil, action: nil)
    
    let imageScaleF: CGFloat = 0.9
    let moreButton = createNavBarButton(with: UIImage(named: "IconMore")!,
                                        highlightedImage: UIImage(named: "IconMoreSelected")!,
                                        imageScale: imageScaleF,
                                        action: #selector(self.showDropDownMenu),
                                        orientation: orientation)
    moreButton.title = NSLocalizedString("menu-more", comment: "")
    
    let infoButton = createNavBarButton(with: UIImage(named: "IconInfo")!,
                                        highlightedImage: UIImage(named: "IconInfoSelected")!,
                                        imageScale: imageScaleF,
                                        action: #selector(self.infoButtonClick),
                                        orientation: orientation)
    infoButton.title = NSLocalizedString("menu-help", comment: "")
    
    let getStartedButton = createNavBarButton(with: UIImage(named: "IconNotepad")!,
                                              highlightedImage: UIImage(named: "IconNotepadSelected")!,
                                              imageScale: imageScaleF,
                                              action: #selector(self.showGetStartedView),
                                              orientation: orientation)
    getStartedButton.title = NSLocalizedString("menu-get-started", comment: "")
    
    let trashButton = createNavBarButton(with: UIImage(named: "IconTrash")!,
                                         highlightedImage: UIImage(named: "IconTrashSelected")!,
                                         imageScale: imageScaleF,
                                         action: #selector(self.trashButtonClick),
                                         orientation: orientation)
    trashButton.title = NSLocalizedString("menu-clear-text", comment: "")
    
    let textSizeButton = createNavBarButton(with: UIImage(named: "IconTextSize")!,
                                            highlightedImage: UIImage(named: "IconTextSizeSelected")!,
                                            imageScale: imageScaleF,
                                            action: #selector(self.textSizeButtonClick),
                                            orientation: orientation)
    textSizeButton.title = NSLocalizedString("menu-text-size", comment: "")
    
    let browserButton = createNavBarButton(with: UIImage(named: "IconBrowser")!,
                                           highlightedImage: UIImage(named: "IconBrowserSelected")!,
                                           imageScale: 1.0,
                                           action: #selector(self.showKMWebBrowserView),
                                           orientation: orientation)
    browserButton.title = NSLocalizedString("menu-show-browser", comment: "")
    
    let actionButton = createNavBarButton(with: UIImage(named: "IconShare")!,
                                          highlightedImage: UIImage(named: "IconShareSelected")!,
                                          imageScale: imageScaleF,
                                          action: #selector(self.actionButtonClick),
                                          orientation: orientation)
    actionButton.title = NSLocalizedString("menu-share", comment: "")
    
    let settingsButton = createNavBarButton(with: UIImage(named: "IconMore")!,
                                            highlightedImage: UIImage(named: "IconMoreSelected")!,
                                            imageScale: imageScaleF,
                                            action: #selector(self.settingsButtonClick),
                                            orientation: orientation)
    settingsButton.title = NSLocalizedString("menu-settings", comment: "")
    
    dropdownItems = [textSizeButton, trashButton, infoButton, getStartedButton, settingsButton]
    
    var scaleF: CGFloat = (UIScreen.main.scale == 2.0) ? 2.0 : 1.5
    scaleF *= (UIDevice.current.userInterfaceIdiom == .phone) ? 1.0 : 2.0
    fixedSpace.width = 10 * scaleF
    if UIDevice.current.userInterfaceIdiom == .phone {
      navigationItem.rightBarButtonItems = [moreButton, fixedSpace, browserButton, fixedSpace, actionButton]
    } else {
      navigationItem.rightBarButtonItems = [settingsButton, fixedSpace, infoButton, fixedSpace, getStartedButton,
                                            fixedSpace, trashButton, fixedSpace, textSizeButton, fixedSpace, browserButton, fixedSpace, actionButton]
    }
  }
  
  private func createNavBarButton(with image: UIImage,
                                  highlightedImage imageHighlighted: UIImage,
                                  imageScale scaleF: CGFloat,
                                  action selector: Selector,
                                  orientation: UIInterfaceOrientation) -> UIBarButtonItem {
    let size = CGSize(width: image.size.width * scaleF, height: image.size.height * scaleF)
    
    let vAdjPort: CGFloat = UIScreen.main.scale == 2.0 ? -3.6 : -2.6
    let vAdjLscpe: CGFloat = -1.6
    
    let vAdj = orientation.isPortrait ? vAdjPort : vAdjLscpe
    let navBarHeight = self.navBarHeight()
    let y = (navBarHeight - size.height) / 2.0 + vAdj
    
    let customButton = UIButton(type: .custom)
    customButton.setImage(image, for: .normal)
    customButton.setImage(imageHighlighted, for: .highlighted)
    customButton.frame = CGRect(x: 0.0, y: y, width: size.width, height: size.height)
    customButton.addTarget(self, action: selector, for: .touchUpInside)
    
    let containerView = UIView(frame: CGRect(x: 0.0, y: 0.0, width: size.width, height: navBarHeight))
    containerView.backgroundColor = UIColor.clear
    containerView.addSubview(customButton)
    
    return UIBarButtonItem(customView: containerView)
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    
    if wasKeyboardVisible {
      textView.becomeFirstResponder()
    }
    
    if !didKeyboardLoad {
      showActivityIndicator()
    } else if shouldShowGetStarted {
      perform(#selector(self.showGetStartedView), with: nil, afterDelay: 1.0)
    }
  }
  
  deinit {
    NotificationCenter.default.removeObserver(self)
  }
  
  override func willRotate(to toInterfaceOrientation: UIInterfaceOrientation, duration: TimeInterval) {
    let orientation: UIInterfaceOrientation = toInterfaceOrientation
    for view in overlayWindow.subviews {
      UIView.animate(withDuration: duration, delay: 0.0, options: ([]), animations: {() -> Void in
        view.transform = self.transform(for: orientation)
      }, completion: nil)
    }
    
    if let navbarBackground = navbarBackground {
      navbarBackground.setOrientation(toInterfaceOrientation)
    }
    
    popover?.dismiss(animated: false)
    _ = dismissDropDownMenu()
  }
  
  override func didRotate(from fromInterfaceOrientation: UIInterfaceOrientation) {
    if infoView?.view?.isHidden ?? true {
      setNavBarButtons()
    }
    resizeViews(withKeyboardVisible: textView.isFirstResponder)
  }
  
  private func resizeViews(withKeyboardVisible keyboardVisible: Bool) {
    guard let window = UIApplication.shared.windows.first else {
      return
    }
    
    // As the keyboard isn't naturally part of the safe area, we must
    // adjust the safe area's size when it appears.
    let kbHeight: CGFloat = keyboardVisible ? textView.inputView?.frame.height ?? 0 : 0
    
    let bottomPadding = window.safeAreaInsets.bottom
    
    var kbdSafeArea = UIEdgeInsets()
    if keyboardVisible {
      // The keyboard also contains the safe area when visible, and we only want
      // to add the _additional_ height.
      kbdSafeArea.bottom += kbHeight - bottomPadding
    }
    self.additionalSafeAreaInsets = kbdSafeArea
  }
  
  private func navBarWidth() -> CGFloat {
    return navigationController!.navigationBar.frame.width
  }
  
  private func navBarHeight() -> CGFloat {
    return navigationController!.navigationBar.frame.height
  }
  
  @objc func onKeyboardWillShow(_ notification: Notification) {
    _ = dismissDropDownMenu()
    resizeViews(withKeyboardVisible: true)
  }
  
  @objc func onKeyboardDidShow(_ notification: Notification) {
    // Workaround to display overlay window above keyboard
    let windows = UIApplication.shared.windows
    let lastWindow = windows.last
    overlayWindow.windowLevel = lastWindow!.windowLevel + 1
  }
  
  @objc func onKeyboardWillHide(_ notification: Notification) {
    resizeViews(withKeyboardVisible: false)
  }
  
  // MARK: - Keyman Notifications
  private func keyboardLoaded() {
    didKeyboardLoad = true
    dismissActivityIndicator()
    
    // Defer display of the keyboard until the current "message" on the main dispatch
    // queue is complete.  There are still ongoing calculations - including within the
    // keyboard's WebView itself!  (This function's call is actually triggered _by_ it!)
    DispatchQueue.main.async {
      self.textView.becomeFirstResponder()
    }
    if shouldShowGetStarted {
      perform(#selector(self.showGetStartedView), with: nil, afterDelay: 1.0)
    }
  }
  
  private func keyboardChanged(_ kb: InstallableKeyboard) {
    checkProfile(forFullID: kb.fullID, doListCheck: true)
  }
  
  private func keyboardPickerDismissed() {
    textView.becomeFirstResponder()
    if UIDevice.current.userInterfaceIdiom == .pad && shouldShowGetStarted {
      perform(#selector(self.showGetStartedView), with: nil, afterDelay: 1.0)
    }
  }
  
  private func keyboardRemoved(_ keyboard: InstallableKeyboard) {
    guard let font = keyboard.font,
          let profile = profileName(withFont: font) else {
      return
    }
    profilesToInstall = profilesToInstall.filter { $0 != profile }
    profilesToInstall.append(profile)
    checkedProfiles = checkedProfiles.filter { $0 != profile }
    let userData = AppDelegate.activeUserDefaults()
    userData.set(checkedProfiles, forKey: checkedProfilesKey)
    userData.synchronize()
  }
  
  // MARK: - View Actions
  
  @objc func infoButtonClick(_ sender: Any?) {
    UIView.setAnimationDelegate(self)
    let animSelector = NSSelectorFromString("animationDidStop:finished:context:")
    UIView.setAnimationDidStop(animSelector)
    UIView.beginAnimations(nil, context: nil)
    UIView.setAnimationDuration(0.75)
    UIView.setAnimationTransition((!textView.isHidden ? .flipFromRight : .flipFromLeft), for: view, cache: true)
    
    if !textView.isHidden {
      textView.isHidden = true
      infoView.view.isHidden = false
      infoView.reloadKeymanHelp()
    } else {
      textView.isHidden = false
      infoView.view.isHidden = true
    }
    
    UIView.commitAnimations()
    
    if !textView.isHidden {
      navigationItem.titleView = nil
      navigationItem.rightBarButtonItem = nil
      setNavBarButtons()
      if shouldShowGetStarted {
        perform(#selector(self.showGetStartedView), with: nil, afterDelay: 0.75)
      } else if wasKeyboardVisible {
        perform(#selector(self.displayKeyboard), with: nil, afterDelay: 0.75)
      }
    } else {
      _ = dismissDropDownMenu()
      popover?.dismiss(animated: false)
      wasKeyboardVisible = textView.isFirstResponder
      textView.dismissKeyboard()
      
      navigationItem.rightBarButtonItems = nil
      let vAdjPort: CGFloat = UIScreen.main.scale == 2.0 ? -3.6 : -2.6
      let vAdjLscpe: CGFloat = -1.6
      
      let doneString = NSLocalizedString("command-done", bundle: Bundle(for: Manager.self), comment: "")
      let infoDoneButton = UIBarButtonItem(title: doneString, style: .plain, target: self,
                                           action: #selector(self.infoButtonClick))
      infoDoneButton.setBackgroundVerticalPositionAdjustment(vAdjPort, for: UIBarMetrics.default)
      infoDoneButton.setBackgroundVerticalPositionAdjustment(vAdjLscpe, for: UIBarMetrics.compact)
      navigationItem.rightBarButtonItem = infoDoneButton
      
      let versionLabel = UILabel()
      let infoDict = Bundle.main.infoDictionary
      let version = infoDict?["KeymanVersionWithTag"] as? String ?? ""
      versionLabel.text = String.init(format: NSLocalizedString("version-label", comment: ""), version)
      versionLabel.font = UIFont.systemFont(ofSize: 9.0)
      versionLabel.textAlignment = .right
      versionLabel.sizeToFit()
      let frame: CGRect = versionLabel.frame
      versionLabel.frame = frame.insetBy(dx: -8, dy: 0)
      versionLabel.backgroundColor = UIColor.clear
      navigationItem.titleView = versionLabel
    }
  }
  
  @objc func settingsButtonClick(_ sender: Any) {
    _ = dismissDropDownMenu()
    popover?.dismiss(animated: false)
    
    Manager.shared.showKeymanEngineSettings(inVC: self)
  }
  
  func showInstalledLanguages() {
    Manager.shared.hideKeyboard()
    
    // Allows us to set a custom UIToolbar for download/update status displays.
    let nc = UINavigationController(navigationBarClass: nil, toolbarClass: ResourceDownloadStatusToolbar.self)
    
    // Grab our newly-generated toolbar instance and inform it of its parent NavigationController.
    // This will help streamline use of the 'toolbar' as a status/update bar.
    let toolbar = nc.toolbar as? ResourceDownloadStatusToolbar
    toolbar?.navigationController = nc
    
    // As it's the first added view controller, settingsVC will function as root automatically.
    let settingsVC = SettingsViewController()
    nc.pushViewController(settingsVC, animated: false)
    nc.modalTransitionStyle = .coverVertical
    nc.modalPresentationStyle = .pageSheet
    
    let installedLanguagesVC = InstalledLanguagesViewController()
    nc.pushViewController(installedLanguagesVC, animated: true)
    
    self.present(nc, animated: true) {
      // Requires a host NavigationViewController, hence calling it afterward.
      installedLanguagesVC.launchKeyboardSearch()
    }
  }
  
  @objc func actionButtonClick(_ sender: Any) {
    _ = dismissDropDownMenu()
    popover?.dismiss(animated: false)
    
    let lrm = "\u{200e}"
    let rlm = "\u{200f}"
    var text: String! = textView.text
    if text.hasPrefix(lrm) || text.hasPrefix(rlm) {
      text = String(text[text.index(after: text.startIndex)...])
    }
    
    let activityProvider = ActivityItemProvider(text: text, font: textView.font)
    
    let printText = UISimpleTextPrintFormatter(text: textView.text)
    printText.font = textView.font
    
    let activityVC = UIActivityViewController(activityItems: [activityProvider, printText], applicationActivities: nil)
    activityVC.excludedActivityTypes = [UIActivity.ActivityType.assignToContact,
                                        UIActivity.ActivityType.postToWeibo,
                                        UIActivity.ActivityType.saveToCameraRoll]
    activityVC.completionWithItemsHandler = {(_ activityType: UIActivity.ActivityType?, _ completed: Bool,
                                              _ returnedItems: [Any]?, _ activityError: Error?) -> Void in
      
      // If a share was completed -OR- if the user backed out of the share selection menu.
      if completed || activityType == nil {
        self.textView.isUserInteractionEnabled = true
      } // else (if the user started to share but backed out to the menu) do nothing.
    }
    
    textView.isUserInteractionEnabled = false
    if UIDevice.current.userInterfaceIdiom == .phone {
      present(activityVC, animated: true, completion: nil)
    } else {
      activityVC.modalPresentationStyle = UIModalPresentationStyle.popover
      activityVC.popoverPresentationController?.barButtonItem = navigationItem.rightBarButtonItems?[12]
      self.present(activityVC, animated: true)
      
      popover = activityVC
    }
  }
  
  @objc func textSizeButtonClick(_ sender: Any) {
    _ = dismissDropDownMenu()
    popover?.dismiss(animated: false)
    
    let textSizeVC = UIViewController()
    textSizeVC.modalPresentationStyle = .overCurrentContext
    textSizeVC.view.backgroundColor = UIColor.clear
    
    let mframe: CGRect = textSizeVC.view.frame
    let w: CGFloat = 304
    let h: CGFloat = 142
    let sframe = CGRect(x: mframe.size.width / 2.0 - w / 2.0, y: mframe.size.height, width: w, height: h)
    let eframe = CGRect(x: sframe.origin.x, y: sframe.origin.y - h - 8, width: w, height: h)
    let containerView = UIView(frame: sframe)
    containerView.autoresizingMask = [.flexibleTopMargin, .flexibleLeftMargin, .flexibleRightMargin]
    containerView.backgroundColor = Colors.systemBackground
    containerView.tag = 1
    containerView.layer.cornerRadius = 4.0
    
    let doneButton = UIButton(type: .roundedRect)
    doneButton.addTarget(self, action: #selector(self.dismissTextSizeVC), for: .touchUpInside)
    
    let doneString = NSLocalizedString("command-done", bundle: Bundle(for: Manager.self), comment: "")
    doneButton.setTitle(doneString, for: .normal)
    doneButton.titleLabel?.font = doneButton.titleLabel?.font?.withSize(21.0)
    doneButton.setTitleColor(UIColor(red: 0.0, green: 0.5, blue: 1.0, alpha: 1.0), for: .normal)
    doneButton.layer.cornerRadius = 4.0
    let bframe = CGRect(x: 0, y: h - 42, width: sframe.size.width, height: 42)
    doneButton.frame = bframe
    containerView.addSubview(doneButton)
    
    let lframe = CGRect(x: 0, y: bframe.origin.y - 1, width: sframe.size.width, height: 1)
    let borderLine = UIView(frame: lframe)
    if #available(iOS 13.0, *) {
      borderLine.backgroundColor = UIColor.separator
    } else {
      let c: CGFloat = 230.0 / 255.0
      borderLine.backgroundColor = UIColor(red: c, green: c, blue: c, alpha: 1.0)
    }
    containerView.addSubview(borderLine)
    
    let tframe = CGRect(x: 0, y: 0, width: bframe.size.width, height: 40)
    let textSizeTitle = UILabel(frame: tframe)
    textSizeTitle.tag = 2
    textSizeTitle.backgroundColor = UIColor.clear
    textSizeTitle.text = String.init(format: NSLocalizedString("text-size-label", comment: ""), Int(textSize))
    textSizeTitle.textAlignment = .center
    if #available(iOS 13.0, *) {
      textSizeTitle.textColor = UIColor.secondaryLabel
    } else {
      textSizeTitle.textColor = UIColor.gray
    }
    textSizeTitle.font = textSizeTitle.font.withSize(14.0)
    containerView.addSubview(textSizeTitle)
    
    let sliderFrame = CGRect(x: 12, y: 45, width: bframe.size.width - 24, height: 50)
    textSizeController.frame = sliderFrame
    containerView.addSubview(textSizeController)
    
    textSizeVC.view.addSubview(containerView)
    if UIDevice.current.userInterfaceIdiom == .phone {
      wasKeyboardVisible = textView.isFirstResponder
      textView.dismissKeyboard()
      present(textSizeVC, animated: false, completion: {() -> Void in
        UIView.animate(withDuration: 0.25, delay: 0.0, options: .curveEaseIn, animations: {() -> Void in
          containerView.frame = eframe
          textSizeVC.view.backgroundColor = UIColor(red: 0.0, green: 0.0, blue: 0.0, alpha: 0.4)
        }, completion: nil)
      })
    } else {
      let vc = UIViewController()
      vc.modalPresentationStyle = .popover
      vc.popoverPresentationController?.barButtonItem = navigationItem.rightBarButtonItems?[8]
      vc.view = containerView
      vc.preferredContentSize = containerView.frame.size
      present(vc, animated: true, completion: nil)
    }
  }
  
  @objc func dismissTextSizeVC(_ sender: Any) {
    let presentedVC = presentedViewController
    if UIDevice.current.userInterfaceIdiom == .phone {
      let containerView: UIView! = presentedVC?.view?.viewWithTag(1)
      let sframe = containerView.frame
      let eframe = CGRect(x: sframe.origin.x, y: sframe.origin.y + sframe.height + 8,
                          width: sframe.width, height: sframe.height)
      UIView.animate(withDuration: 0.25, delay: 0.0, options: .curveEaseIn, animations: {() -> Void in
        containerView.frame = eframe
        presentedVC?.view?.backgroundColor = UIColor.clear
      }, completion: {(_ finished: Bool) -> Void in
        presentedVC?.dismiss(animated: false, completion: {() -> Void in
          if self.wasKeyboardVisible {
            self.textView.becomeFirstResponder()
          }
        })
      })
    } else {
      presentedVC?.dismiss(animated: true, completion: nil)
    }
  }
  
  @objc func trashButtonClick(_ sender: Any) {
    _ = dismissDropDownMenu()
    let alert = UIAlertController(title: nil, message: nil, preferredStyle: .actionSheet)
    let clear = UIAlertAction(title: NSLocalizedString("menu-clear-text", comment: ""),
                              style: .destructive,
                              handler: {(_ action: UIAlertAction) -> Void in
      self.textView.text = ""
    })
    let cancel = UIAlertAction(title: NSLocalizedString("menu-cancel", comment: ""),
                               style: .default, handler: nil)
    alert.addAction(clear)
    alert.addAction(cancel)
    alert.popoverPresentationController?.barButtonItem = navigationItem.rightBarButtonItems?[6]
    present(alert, animated: true, completion: nil)
  }
  
  func showActivityIndicator() {
    if !overlayWindow.isHidden && overlayWindow.viewWithTag(activityIndicatorViewTag) != nil {
      return
    }
    
    dismissGetStartedView(nil)
    overlayWindow.isHidden = false
    let activityView = UIActivityIndicatorView(style: .whiteLarge)
    let containerView = UIView(frame: activityView.bounds.insetBy(dx: -10.0, dy: -10.0))
    containerView.backgroundColor = Colors.spinnerBackground
    containerView.layer.cornerRadius = 6.0
    containerView.center = overlayWindow.center
    containerView.tag = activityIndicatorViewTag
    containerView.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                                      .flexibleBottomMargin]
    activityView.center = CGPoint(x: containerView.bounds.size.width * 0.5, y: containerView.bounds.size.height * 0.5)
    activityView.startAnimating()
    containerView.addSubview(activityView)
    overlayWindow.addSubview(containerView)
  }
  
  @objc func dismissActivityIndicator() {
    overlayWindow.viewWithTag(activityIndicatorViewTag)?.removeFromSuperview()
    overlayWindow.isHidden = true
  }
  
  @objc func sliderValueChanged(_ sender: UISlider) {
    textSize = CGFloat(sender.value + Float(minTextSize)).rounded()
    textView.font = textView.font?.withSize(textSize)
    let textSizeTitle = presentedViewController?.view.viewWithTag(2) as? UILabel
    textSizeTitle?.text = String.init(format: NSLocalizedString("text-size-label", comment: ""), Int(textSize))
  }
  private func rectForBarButtonItem(_ barButtonItem: UIBarButtonItem) -> CGRect {
    let view =  barButtonItem.value(forKey: "view") as? UIView
    return view?.frame ?? CGRect.zero
  }
  
  private func resetTextViewCursor() {
    textView.selectedRange = NSRange(location: 0, length: 0)
    
    // TODO: Figure out what this does
    if let size = textView.font?.pointSize {
      textView.font = textView.font?.withSize(size + 5)
      textView.font = textView.font?.withSize(size - 5)
    }
  }
  
  private func params(of query: String) -> [String: String] {
    let components = query.components(separatedBy: "&")
    return components.reduce([String: String]()) { prevParams, s in
      var params = prevParams
      let components = s.components(separatedBy: "=")
      if components.count == 2 {
        params[components[0]] = components[1]
      }
      return params
    }
  }
  
  private func profileName(withFullID fullID: FullKeyboardID) -> String? {
    guard let keyboard = AppDelegate.activeUserDefaults().userKeyboard(withFullID: fullID),
          let font = keyboard.font else {
      return nil
    }
    
    return profileName(withFont: font)
  }
  
  private func profileName(withFont font: Font) -> String? {
    return font.source.first { $0.lowercased().hasSuffix(FileExtensions.configurationProfile) }
  }
  
  private func checkProfile(forFullID fullID: FullKeyboardID, doListCheck: Bool) {
    if fullID == Defaults.keyboard.fullID {
      return
    }
    if profileName != nil {
      return  // already installing a profile
    }
    
    guard let profile = profileName(withFullID: fullID) else {
      return
    }
    
    var doInstall: Bool = false
    if doListCheck {
      for value in profilesToInstall where !checkedProfiles.contains(value) && profile == value {
        doInstall = true
        break
      }
    } else {
      doInstall = true
    }
    
    if doInstall {
      profileName = profile
      let keyboard = AppDelegate.activeUserDefaults().userKeyboard(withFullID: fullID)!
      let languageName = keyboard.languageName
      let title = String.init(format: NSLocalizedString("language-for-font", comment: ""), languageName)
      let msg = String.init(
        format: NSLocalizedString(
          "font-install-description",
          comment: "Long-form used when installing a font in order to display a language properly."),
        languageName)
      confirmInstall(withTitle: title, message: msg,
                     cancelButtonHandler: handleUserDecisionAboutInstallingProfile,
                     installButtonHandler: handleUserDecisionAboutInstallingProfile)
    } else {
      profileName = nil
    }
  }
  
  // showKeyboard matches an internal Apple API, so it's not available for use as a selector.
  @objc func displayKeyboard() {
    textView.becomeFirstResponder()
  }
  
  private func confirmInstall(withTitle title: String, message msg: String,
                              cancelButtonHandler cbHandler: ((UIAlertAction) -> Swift.Void)? = nil,
                              installButtonHandler installHandler: ((UIAlertAction) -> Swift.Void)?) {
    dismissGetStartedView(nil)
    
    let alertController = UIAlertController(title: title, message: msg,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: NSLocalizedString("menu-cancel", comment: ""),
                                            style: UIAlertAction.Style.cancel,
                                            handler: cbHandler))
    alertController.addAction(UIAlertAction(title: NSLocalizedString("confirm-install", comment: ""),
                                            style: UIAlertAction.Style.default,
                                            handler: installHandler))
    
    self.present(alertController, animated: true, completion: nil)
  }
  
  private func handleUserDecisionAboutInstallingProfile(withAction action: UIAlertAction) {
    if let profileName = profileName {
      checkedProfiles.append(profileName)
      let userData = AppDelegate.activeUserDefaults()
      userData.set(checkedProfiles, forKey: checkedProfilesKey)
      userData.synchronize()
      
      if action.style == .default {
        UIApplication.shared.open(URL(string: "\(baseUri)\(profileName)")!)
      }
      self.profileName = nil
    }
  }
  
  private func showGetStartedIfNeeded(withAction action: UIAlertAction) {
    if shouldShowGetStarted {
      showGetStartedView(nil)
    }
  }
  
  @objc func showDropDownMenu(_ sender: Any) {
    if dismissDropDownMenu() {
      return
    }
    
    wasKeyboardVisible = textView.isFirstResponder
    textView.dismissKeyboard()
    
    let menuRect: CGRect = self.calculateDropDownMenuRect()
    let dropDownList = DropDownListView(listItems: dropdownItems, itemSize: menuRect.size, position: menuRect.origin)
    dropDownList.tag = dropDownListTag
    
    navigationController?.view?.addSubview(dropDownList)
  }
  
  /**
   * Calculates coordinates of Keyman app dropdown menu
   * takes into account safearea so that menu does display over iPhone notch when
   * rotated to landscape (issue #8168)
   */
  private func calculateDropDownMenuRect() -> CGRect {
    let rightInset: CGFloat? = self.view.window?.safeAreaInsets.right
    let width: CGFloat = 160
    let height: CGFloat = 44
    let x: CGFloat = navBarWidth() - width - (rightInset ?? 0)
    let y: CGFloat = AppDelegate.statusBarHeight() + navBarHeight()
    let menuRect = CGRect(x: x, y: y, width: width, height: height)
    return menuRect
  }
  
  private func dismissDropDownMenu() -> Bool {
    let view = navigationController?.view?.viewWithTag(dropDownListTag)
    if view != nil && wasKeyboardVisible {
      textView.becomeFirstResponder()
    }
    view?.removeFromSuperview()
    return view != nil
  }
  
  @objc func showGetStartedView(_ sender: Any?) {
    if !overlayWindow.isHidden && overlayWindow.viewWithTag(getStartedViewTag) != nil {
      return
    }
    
    popover?.dismiss(animated: false)
    _ = dismissDropDownMenu()
    overlayWindow.isHidden = false
    getStartedVC = GetStartedViewController()
    getStartedVC.mainViewController = self
    let containerView: UIView! = getStartedVC.view
    let navBar = containerView?.viewWithTag(786586) as? UINavigationBar
    let doneButton = navBar?.topItem?.rightBarButtonItem
    doneButton?.target = self
    doneButton?.action = #selector(self.dismissGetStartedView)
    
    Manager.shared.hideKeyboard()
    
    containerView.frame = containerView.frame.insetBy(dx: 15, dy: 140)
    containerView.backgroundColor = UIColor.white
    containerView.layer.cornerRadius = 6.0
    containerView.center = overlayWindow.center
    containerView.tag = getStartedViewTag
    containerView.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                                      .flexibleBottomMargin]
    let orientation = UIApplication.shared.statusBarOrientation
    containerView.transform = transform(for: orientation)
    overlayWindow.addSubview(containerView)
  }
  
  @objc func dismissGetStartedView(_ sender: Any?) {
    overlayWindow.viewWithTag(getStartedViewTag)?.removeFromSuperview()
    overlayWindow.isHidden = true
    
    Manager.shared.showKeyboard()
  }
  
  private var shouldShowGetStarted: Bool {
    // Do not display "Get started" when MainView is not visible
    if navigationController?.visibleViewController != self {
      return false
    }
    
    let userData = AppDelegate.activeUserDefaults()
    if userData.bool(forKey: dontShowGetStartedKey) {
      return false
    }
    
    // Needs a nil check to ensure Get Started displays for the initial install - userData.bool defaults to false.
    if !userData.bool(forKey: shouldShowGetStartedKey) && userData.object(forKey: shouldShowGetStartedKey) != nil {
      return false
    }
    
    if !AppDelegate.isKeymanEnabledSystemWide() {
      return true
    }
    
    guard let userKbs = userData.userKeyboards else {
      return true
    }
    if userKbs.isEmpty {
      return true
    }
    if userKbs.count >= 2 {
      return false
    }
    
    let firstKB = userKbs[0]
    return firstKB.id == Defaults.keyboard.id && firstKB.languageID == Defaults.keyboard.languageID
  }
  
  @objc func showKMWebBrowserView(_ sender: Any) {
    popover?.dismiss(animated: false)
    _ = dismissDropDownMenu()
    let webBrowserVC = WebBrowserViewController()
    if Manager.shared.currentKeyboard?.font != nil {
      webBrowserVC.fontKeyboard = Manager.shared.currentKeyboard
    }
    present(webBrowserVC, animated: true, completion: nil)
  }
  
  private func transform(for orientation: UIInterfaceOrientation) -> CGAffineTransform {
    return CGAffineTransform.identity
  }
}
