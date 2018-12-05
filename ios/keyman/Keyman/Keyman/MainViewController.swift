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

// Internal strings
private let baseUri = "https://r.keymanweb.com/20/fonts/get_mobileconfig.php?id="
private let profileKey = "profile"
private let checkedProfilesKey = "CheckedProfiles"

// External strings
let userTextKey = "UserText"
let userTextSizeKey = "UserTextSize"
let dontShowGetStartedKey = "DontShowGetStarted"
let launchedFromUrlNotification = NSNotification.Name("LaunchedFromUrlNotification")
let urlKey = "url"

class MainViewController: UIViewController, TextViewDelegate, UIActionSheetDelegate {
  private let minTextSize: CGFloat = 9.0
  private let maxTextSize: CGFloat = 72.0
  private let getStartedViewTag = 7183
  private let activityIndicatorViewTag = 6573
  private let dropDownListTag = 686876

  var textView: TextView!
  var textSize: CGFloat = 0.0
  var navbarBackground: KMNavigationBarBackgroundView!

  private var getStartedVC: GetStartedViewController!
  private var infoView: InfoViewController!
  private var popover: UIPopoverController?
  private var textSizeController: UISlider!
  private var dropdownItems: [UIBarButtonItem]!

  private var profilesToInstall: [String] = []
  private var checkedProfiles: [String] = []
  private var profileName: String?
  private var launchUrl: URL?
  private var keyboardToDownload: InstallableKeyboard?
  private var customKeyboardToDownload: URL?
  private var wasKeyboardVisible: Bool = false

  private var screenWidth: CGFloat = 0.0
  private var screenHeight: CGFloat = 0.0
  private var portLeftMargin: CGFloat = 0.0
  private var portRightMargin: CGFloat = 0.0
  private var lscpeLeftMargin: CGFloat = 0.0
  private var lscpeRightMargin: CGFloat = 0.0
  private var didDownload = false
  private var didKeyboardLoad = false

  private var keyboardLoadedObserver: NotificationObserver?
  private var languagesUpdatedObserver: NotificationObserver?
  private var languagesDownloadFailedObserver: NotificationObserver?
  private var keyboardPickerDismissedObserver: NotificationObserver?
  private var keyboardChangedObserver: NotificationObserver?
  private var keyboardDownloadStartedObserver: NotificationObserver?
  private var keyboardDownloadCompletedObserver: NotificationObserver?
  private var keyboardDownloadFailedObserver: NotificationObserver?
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
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillShow),
        name: NSNotification.Name.UIKeyboardWillShow, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardDidShow),
        name: NSNotification.Name.UIKeyboardWillShow, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardWillHide),
        name: NSNotification.Name.UIKeyboardWillHide, object: nil)
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
    keyboardDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadStarted,
      observer: self,
      function: MainViewController.keyboardDownloadStarted)
    keyboardDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadCompleted,
      observer: self,
      function: MainViewController.keyboardDownloadCompleted)
    keyboardDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadFailed,
      observer: self,
      function: MainViewController.keyboardDownloadFailed)
    keyboardRemovedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardRemoved,
      observer: self,
      function: MainViewController.keyboardRemoved)

  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    extendedLayoutIncludesOpaqueBars = true
    automaticallyAdjustsScrollViewInsets = false

    let systemFonts = Set<String>(UIFont.familyNames)

    // Setup Keyman Manager & fetch keyboards list
    Manager.shared.canRemoveDefaultKeyboard = true
    Manager.shared.apiKeyboardRepository.fetch()

    let bgColor = UIColor(red: 1.0, green: 1.0, blue: 207.0 / 255.0, alpha: 1.0)
    view?.backgroundColor = bgColor

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
      navbarBackground = KMNavigationBarBackgroundView()
      navbarBackground.addToNavbar(navbar)
      navbarBackground.setOrientation(UIApplication.shared.statusBarOrientation)
    }

    // Setup Keyman TextView
    textSize = 16.0
    textView = TextView(frame: view.frame)
    textView.setKeymanDelegate(self)
    textView.viewController = self
    textView.backgroundColor = bgColor
    textView.isScrollEnabled = true
    textView.isUserInteractionEnabled = true
    textView.font = textView.font?.withSize(textSize)
    view?.addSubview(textView!)

    // Setup Info View
    infoView = InfoViewController()
    if UIDevice.current.userInterfaceIdiom != .phone {
      textSize *= 2
      textView.font = textView?.font?.withSize(textSize)
    }

    infoView.view?.isHidden = true
    infoView.view?.backgroundColor = bgColor
    view?.addSubview(infoView!.view)

    resizeViews(withKeyboardVisible: textView.isFirstResponder)

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

    setNavBarButtons()
    loadSavedUserText()
  }

  override func viewWillDisappear(_ animated: Bool) {
    super.viewWillDisappear(animated)
    wasKeyboardVisible = textView.isFirstResponder
  }

  private func setNavBarButtons() {
    let orientation: UIInterfaceOrientation = UIApplication.shared.statusBarOrientation
    let fixedSpace = UIBarButtonItem(barButtonSystemItem: .fixedSpace, target: nil, action: nil)

    let imageScaleF: CGFloat = 0.9
    let moreButton = createNavBarButton(with: #imageLiteral(resourceName: "more.png"),
                                        highlightedImage: #imageLiteral(resourceName: "more-selected.png"),
                                        imageScale: imageScaleF,
                                        action: #selector(self.showDropDownMenu),
                                        orientation: orientation)
    moreButton.title = "More"

    let infoButton = createNavBarButton(with: #imageLiteral(resourceName: "724-info.png"),
                                        highlightedImage: #imageLiteral(resourceName: "724-info-selected.png"),
                                        imageScale: imageScaleF,
                                        action: #selector(self.infoButtonClick),
                                        orientation: orientation)
    infoButton.title = "Info"

    let getStartedButton = createNavBarButton(with: #imageLiteral(resourceName: "887-notepad.png"),
                                              highlightedImage: #imageLiteral(resourceName: "887-notepad-selected.png"),
                                              imageScale: imageScaleF,
                                              action: #selector(self.showGetStartedView),
                                              orientation: orientation)
    getStartedButton.title = "Get Started"

    let trashButton = createNavBarButton(with: #imageLiteral(resourceName: "711-trash.png"),
                                         highlightedImage: #imageLiteral(resourceName: "711-trash-selected.png"),
                                         imageScale: imageScaleF,
                                         action: #selector(self.trashButtonClick),
                                         orientation: orientation)
    trashButton.title = "Clear Text"

    let textSizeButton = createNavBarButton(with: #imageLiteral(resourceName: "textsize.png"),
                                            highlightedImage: #imageLiteral(resourceName: "textsize_selected.png"),
                                            imageScale: imageScaleF,
                                            action: #selector(self.textSizeButtonClick),
                                            orientation: orientation)
    textSizeButton.title = "Text Size"

    let browserButton = createNavBarButton(with: #imageLiteral(resourceName: "786-browser.png"),
                                           highlightedImage: #imageLiteral(resourceName: "786-browser-selected.png"),
                                           imageScale: 1.0,
                                           action: #selector(self.showKMWebBrowserView),
                                           orientation: orientation)
    browserButton.title = "Browser"

    let actionButton = createNavBarButton(with: #imageLiteral(resourceName: "702-share.png"),
                                          highlightedImage: #imageLiteral(resourceName: "702-share-selected.png"),
                                          imageScale: imageScaleF,
                                          action: #selector(self.actionButtonClick),
                                          orientation: orientation)
    actionButton.title = "Share"

    dropdownItems = [textSizeButton, trashButton, getStartedButton, infoButton]

    var scaleF: CGFloat = (UIScreen.main.scale == 2.0) ? 2.0 : 1.5
    scaleF *= (UIDevice.current.userInterfaceIdiom == .phone) ? 1.0 : 2.0
    fixedSpace.width = 10 * scaleF
    if UIDevice.current.userInterfaceIdiom == .phone {
      navigationItem.rightBarButtonItems = [moreButton, fixedSpace, browserButton, fixedSpace, actionButton]
    } else {
      navigationItem.rightBarButtonItems = [infoButton, fixedSpace, getStartedButton, fixedSpace, trashButton,
          fixedSpace, textSizeButton, fixedSpace, browserButton, fixedSpace, actionButton]
    }
  }

  private func createNavBarButton(with image: UIImage,
                                  highlightedImage imageHighlighted: UIImage,
                                  imageScale scaleF: CGFloat,
                                  action selector: Selector,
                                  orientation: UIInterfaceOrientation) -> UIBarButtonItem {
    let icon = image.resize(to: CGSize(width: image.size.width * scaleF, height: image.size.height * scaleF))
        .withRenderingMode(.alwaysOriginal)
    let iconHighlighted = imageHighlighted.resize(to:
        CGSize(width: imageHighlighted.size.width * scaleF, height: imageHighlighted.size.height * scaleF))
        .withRenderingMode(.alwaysOriginal)

    let vAdjPort: CGFloat = UIScreen.main.scale == 2.0 ? -3.6 : -2.6
    let vAdjLscpe: CGFloat = -1.6

    let vAdj = UIInterfaceOrientationIsPortrait(orientation) ? vAdjPort : vAdjLscpe
    let navBarHeight = self.navBarHeight()
    let y = (navBarHeight - icon.size.height) / 2.0 + vAdj

    let customButton = UIButton(type: .custom)
    customButton.setImage(icon, for: .normal)
    customButton.setImage(iconHighlighted, for: .highlighted)
    customButton.frame = CGRect(x: 0.0, y: y, width: icon.size.width, height: icon.size.height)
    customButton.addTarget(self, action: selector, for: .touchUpInside)

    let containerView = UIView(frame: CGRect(x: 0.0, y: 0.0, width: icon.size.width, height: navBarHeight))
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
    // Resize textView and infoView
    let mainScreen: CGRect = UIScreen.main.bounds
    let margin: CGFloat = 2.0
    let barHeights: CGFloat = AppDelegate.statusBarHeight() + navBarHeight()
    let kbHeight: CGFloat = keyboardVisible ? textView.inputView?.frame.height ?? 0 : 0
    let width: CGFloat = mainScreen.size.width
    let height: CGFloat = mainScreen.size.height - barHeights - kbHeight

    textView.frame = CGRect(x: margin, y: barHeights + margin,
                             width: width - 2 * margin, height: height - 2 * margin)
    infoView?.view?.frame = CGRect(x: 0, y: barHeights, width: width, height: height + kbHeight)
  }

  private func navBarWidth() -> CGFloat {
    return navigationController!.navigationBar.frame.width
  }

  private func navBarHeight() -> CGFloat {
    return navigationController!.navigationBar.frame.height
  }

  @objc func keyboardWillShow(_ notification: Notification) {
    _ = dismissDropDownMenu()
    resizeViews(withKeyboardVisible: true)
  }

  @objc func keyboardDidShow(_ notification: Notification) {
    // Workaround to display overlay window above keyboard
    if #available(iOS 9.0, *) {
      let windows = UIApplication.shared.windows
      let lastWindow = windows.last
      overlayWindow.windowLevel = lastWindow!.windowLevel + 1
    }
  }

  @objc func keyboardWillHide(_ notification: Notification) {
    resizeViews(withKeyboardVisible: false)
  }

  // MARK: - Keyman Notifications
  private func keyboardLoaded() {
    didKeyboardLoad = true
    dismissActivityIndicator()
    textView.becomeFirstResponder()
    if let launchUrl = launchUrl {
      performAction(from: launchUrl)
    } else {
      if shouldShowGetStarted {
        perform(#selector(self.showGetStartedView), with: nil, afterDelay: 1.0)
      }
    }
  }

  private func keyboardChanged(_ kb: InstallableKeyboard) {
    var listCheck: Bool = true
    if didDownload {
      listCheck = false
      didDownload = false
    }

    checkProfile(forFullID: kb.fullID, doListCheck: listCheck)
  }

  private func keyboardDownloadStarted() {
    if launchUrl != nil {
      showActivityIndicator()
    }
  }

  private func keyboardDownloadCompleted(_ keyboards: [InstallableKeyboard]) {
    didDownload = true
    if launchUrl == nil {
      return
    }

    let userData = AppDelegate.activeUserDefaults()
    let userKeyboards = userData.userKeyboards
    if userKeyboards == nil || userKeyboards!.isEmpty {
      Manager.shared.addKeyboard(Defaults.keyboard)
    }

    perform(#selector(self.dismissActivityIndicator), with: nil, afterDelay: 1.0)

    for keyboard in keyboards {
      Manager.shared.addKeyboard(keyboard)
      _ = Manager.shared.setKeyboard(keyboard)
    }

    launchUrl = nil
  }

  private func keyboardDownloadFailed(_ notification: KeyboardDownloadFailedNotification) {
    if launchUrl != nil {
      perform(#selector(self.dismissActivityIndicator), with: nil, afterDelay: 1.0)
      let error = notification.error
      appDelegate.showSimpleAlert(title: "Keyboard Download Error", message: error.localizedDescription)
      launchUrl = nil
    }
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
      if wasKeyboardVisible {
        perform(#selector(self.showKeyboard), with: nil, afterDelay: 0.75)
      }
      if shouldShowGetStarted {
        perform(#selector(self.showGetStartedView), with: nil, afterDelay: 0.75)
      }
    } else {
      _ = dismissDropDownMenu()
      popover?.dismiss(animated: false)
      wasKeyboardVisible = textView.isFirstResponder
      textView.dismissKeyboard()

      navigationItem.rightBarButtonItems = nil
      let vAdjPort: CGFloat = UIScreen.main.scale == 2.0 ? -3.6 : -2.6
      let vAdjLscpe: CGFloat = -1.6

      let infoDoneButton = UIBarButtonItem(title: "Done", style: .plain, target: self,
                                           action: #selector(self.infoButtonClick))
      infoDoneButton.setBackgroundVerticalPositionAdjustment(vAdjPort, for: UIBarMetrics.default)
      infoDoneButton.setBackgroundVerticalPositionAdjustment(vAdjLscpe, for: UIBarMetrics.compact)
      navigationItem.rightBarButtonItem = infoDoneButton

      let versionLabel = UILabel()
      let version = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? ""
      let build = Bundle.main.infoDictionary?[kCFBundleVersionKey as String] as? String ?? ""
      versionLabel.text = "Version: \(version) (build \(build))"
      versionLabel.font = UIFont.systemFont(ofSize: 9.0)
      versionLabel.textAlignment = .right
      versionLabel.sizeToFit()
      let frame: CGRect = versionLabel.frame
      versionLabel.frame = frame.insetBy(dx: -8, dy: 0)
      versionLabel.backgroundColor = UIColor.clear
      navigationItem.titleView = versionLabel
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
    activityVC.excludedActivityTypes = [UIActivityType.assignToContact, UIActivityType.postToWeibo,
                                        UIActivityType.saveToCameraRoll]
    activityVC.completionWithItemsHandler = {(_ activityType: UIActivityType?, _ completed: Bool,
      _ returnedItems: [Any]?, _ activityError: Error?) -> Void in
      self.textView.isUserInteractionEnabled = true
    }

    textView.isUserInteractionEnabled = false
    if UIDevice.current.userInterfaceIdiom == .phone {
      present(activityVC, animated: true, completion: nil)
    } else {
      popover = UIPopoverController(contentViewController: activityVC)
      popover!.present(from: (navigationItem.rightBarButtonItems?[8])!, permittedArrowDirections: .any,
                      animated: true)
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
    containerView.tag = 1
    containerView.backgroundColor = UIColor.white
    containerView.layer.cornerRadius = 4.0

    let doneButton = UIButton(type: .roundedRect)
    doneButton.addTarget(self, action: #selector(self.dismissTextSizeVC), for: .touchUpInside)

    doneButton.backgroundColor = UIColor.white
    doneButton.setTitle("Done", for: .normal)
    doneButton.titleLabel?.font = doneButton.titleLabel?.font?.withSize(21.0)
    doneButton.setTitleColor(UIColor(red: 0.0, green: 0.5, blue: 1.0, alpha: 1.0), for: .normal)
    doneButton.layer.cornerRadius = 4.0
    let bframe = CGRect(x: 0, y: h - 42, width: sframe.size.width, height: 42)
    doneButton.frame = bframe
    containerView.addSubview(doneButton)

    let lframe = CGRect(x: 0, y: bframe.origin.y - 1, width: sframe.size.width, height: 1)
    let borderLine = UIView(frame: lframe)
    let c: CGFloat = 230.0 / 255.0
    borderLine.backgroundColor = UIColor(red: c, green: c, blue: c, alpha: 1.0)
    containerView.addSubview(borderLine)

    let tframe = CGRect(x: 0, y: 0, width: bframe.size.width, height: 40)
    let textSizeTitle = UILabel(frame: tframe)
    textSizeTitle.tag = 2
    textSizeTitle.backgroundColor = UIColor.clear
    textSizeTitle.text = "Text size: \(Int(textSize))"
    textSizeTitle.textAlignment = .center
    textSizeTitle.textColor = UIColor.gray
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
      vc.popoverPresentationController?.barButtonItem = navigationItem.rightBarButtonItems?[6]
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
    let clear = UIAlertAction(title: "Clear Text", style: .destructive, handler: {(_ action: UIAlertAction) -> Void in
      self.textView.text = ""
    })
    let cancel = UIAlertAction(title: "Cancel", style: .default, handler: nil)
    alert.addAction(clear)
    alert.addAction(cancel)
    alert.popoverPresentationController?.barButtonItem = navigationItem.rightBarButtonItems?[4]
    present(alert, animated: true, completion: nil)
  }

  func showActivityIndicator() {
    if !overlayWindow.isHidden && overlayWindow.viewWithTag(activityIndicatorViewTag) != nil {
      return
    }

    dismissGetStartedView(nil)
    overlayWindow.isHidden = false
    let activityView = UIActivityIndicatorView(activityIndicatorStyle: .whiteLarge)
    let containerView = UIView(frame: activityView.bounds.insetBy(dx: -10.0, dy: -10.0))
    containerView.backgroundColor = UIColor(white: 0.5, alpha: 0.8)
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
    textSizeTitle?.text = "Text size: \(Int(textSize))"
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

  private func loadSavedUserText() {
    let userData = AppDelegate.activeUserDefaults()
    let userText = userData.object(forKey: userTextKey) as? String
    let userTextSize = userData.object(forKey: userTextSizeKey) as? String

    if let text = userText, !text.isEmpty {
      textView.text = text
    }

    if let sizeString = userTextSize, let size = Float(sizeString) {
      textSize = CGFloat(size)
      textView.font = textView.font?.withSize(textSize)
      textSizeController.value = (Float(textSize - minTextSize))
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

  private func performAction(from url: URL) {
    guard let query = url.query else {
      launchUrl = nil
      return
    }

    if url.lastPathComponent != "open" {
      return
    }

    let params = self.params(of: query)
    if let urlString = params["url"] {
      // Download and set custom keyboard
      guard let url = URL(string: urlString) else {
        appDelegate.showSimpleAlert(title: "Custom Keyboard",
                                    message: "The keyboard could not be installed: Invalid Url")
        launchUrl = nil
        return
      }

      Manager.shared.dismissKeyboardPicker(self)
      if !infoView.view.isHidden {
        perform(#selector(self.infoButtonClick), with: nil)
      }

      customKeyboardToDownload = url
      let title = "Custom Keyboard: \(url.lastPathComponent)"
      confirmInstall(withTitle: title, message: "Would you like to install this keyboard?",
                cancelButtonHandler: showGetStartedIfNeeded,
                installButtonHandler: proceedWithCustomKeyboardDownload)
    } else if let kbID = params["keyboard"], let langID = params["language"] {
      // Query should include keyboard and language IDs to set the keyboard (first download if not available)
      guard let keyboard = Manager.shared.apiKeyboardRepository.installableKeyboard(withID: kbID,
                                                                                    languageID: langID) else {
        return
      }

      if Manager.shared.stateForKeyboard(withID: kbID) == .needsDownload {
        keyboardToDownload = keyboard
        confirmInstall(withTitle: "\(keyboard.languageName): \(keyboard.name)",
          message: "Would you like to install this keyboard?",
          installButtonHandler: proceedWithKeyboardDownload)
      } else {
        Manager.shared.addKeyboard(keyboard)
        _ = Manager.shared.setKeyboard(keyboard)
      }
    } else {
      launchUrl = nil
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
      let title = "\(languageName) Font"
      let msg = "Touch Install to make \(languageName) display correctly in all your apps"
      confirmInstall(withTitle: title, message: msg,
                cancelButtonHandler: handleUserDecisionAboutInstallingProfile,
                installButtonHandler: handleUserDecisionAboutInstallingProfile)
    } else {
      profileName = nil
    }
  }

  @objc func showKeyboard() {
    textView.becomeFirstResponder()
  }

  private func confirmInstall(withTitle title: String, message msg: String,
                              cancelButtonHandler cbHandler: ((UIAlertAction) -> Swift.Void)? = nil,
                              installButtonHandler installHandler: ((UIAlertAction) -> Swift.Void)?) {
    dismissGetStartedView(nil)

    let alertController = UIAlertController(title: title, message: msg,
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: "Cancel",
                                            style: UIAlertActionStyle.cancel,
                                            handler: cbHandler))
    alertController.addAction(UIAlertAction(title: "Install",
                                              style: UIAlertActionStyle.default,
                                              handler: installHandler))

    self.present(alertController, animated: true, completion: nil)
  }

  private func proceedWithKeyboardDownload(withAction action: UIAlertAction) {
    if let keyboard = keyboardToDownload {
      Manager.shared.downloadKeyboard(withID: keyboard.id, languageID: keyboard.languageID, isUpdate: false)
    }
  }

  private func handleUserDecisionAboutInstallingProfile(withAction action: UIAlertAction) {
    if let profileName = profileName {
      checkedProfiles.append(profileName)
      let userData = AppDelegate.activeUserDefaults()
      userData.set(checkedProfiles, forKey: checkedProfilesKey)
      userData.synchronize()

      if action.style == .default {
        UIApplication.shared.openURL(URL(string: "\(baseUri)\(profileName)")!)
      }
      self.profileName = nil
    }
  }

  private func proceedWithCustomKeyboardDownload(withAction action: UIAlertAction) {
    if let url = customKeyboardToDownload {
      Manager.shared.downloadKeyboard(from: url)
    }
    showGetStartedIfNeeded(withAction: action)
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

    let w: CGFloat = 160
    let h: CGFloat = 44
    let x: CGFloat = navBarWidth() - w
    let y: CGFloat = AppDelegate.statusBarHeight() + navBarHeight()

    let dropDownList = DropDownListView(listItems: dropdownItems, itemSize: CGSize(width: w, height: h),
                                        position: CGPoint(x: x, y: y))
    dropDownList.tag = dropDownListTag

    navigationController?.view?.addSubview(dropDownList)
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
    if let fontFamily = textView.font?.fontName {
      webBrowserVC.fontFamily = fontFamily
    }
    present(webBrowserVC, animated: true, completion: nil)
  }

  private func transform(for orientation: UIInterfaceOrientation) -> CGAffineTransform {
    return CGAffineTransform.identity
  }
}
