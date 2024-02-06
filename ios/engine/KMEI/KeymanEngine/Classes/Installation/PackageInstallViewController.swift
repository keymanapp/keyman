//
//  PackageInstallViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 12/18/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import WebKit
import DeviceKit
import os.log

public class PackageInstallViewController<Resource: LanguageResource>: UIViewController, UITableViewDelegate, UITableViewDataSource, UITabBarControllerDelegate, UIAdaptivePresentationControllerDelegate {
  private enum NavigationMode: Int {  // b/c hashable
    // left nav
    case cancel
    case back

    // right nav
    case none
    case next
    case install
  }

  private enum CellStyle {
    case none
    case preinstalled
    case install
  }

  public typealias CompletionHandler = ([Resource.FullID]?) -> Void

  // Needed to support iOS 9 + 10.
  @IBOutlet weak var webViewContainer: UIView!

  // Common fields between both layout formats.
  @IBOutlet weak var lblVersion: UILabel!
  @IBOutlet weak var lblCopyright: UILabel!
  @IBOutlet weak var languageTable: UITableView!

  // iPad layout only - May be altered programatically.
  @IBOutlet weak var ipadTagWidthConstraint: NSLayoutConstraint?

  // iPhone layout only
  // Do _NOT_ use `weak` here - things break otherwise.
  @IBOutlet var iphoneTabViewController: UITabBarController!

  let package: Resource.Package
  var packagePageController: PackageWebViewController?
  let pickingCompletionHandler: CompletionHandler
  let uiCompletionHandler: (() -> Void)
  let defaultLanguageCode: String
  let associators: [LanguagePickAssociator]
  let languages: [Language]
  let preinstalledLanguageCodes: Set<String>

  private var leftNavMode: NavigationMode = .cancel
  private var rightNavMode: NavigationMode = .none
  private var navMapping: [NavigationMode : UIBarButtonItem] = [:]

  private var dismissalBlock: (() -> Void)? = nil
  private weak var welcomeView: UIView?
  private var mayPick: Bool = true

  public init(for package: Resource.Package,
              defaultLanguageCode: String? = nil,
              languageAssociators: [LanguagePickAssociator] = [],
              pickingCompletionHandler: @escaping CompletionHandler,
              uiCompletionHandler: @escaping (() -> Void)) {
    self.package = package
    self.pickingCompletionHandler = pickingCompletionHandler
    self.uiCompletionHandler = uiCompletionHandler
    self.languages = package.languages

    self.associators = languageAssociators

    var xib: String
    if Device.current.isPad {
      xib = "PackageInstallView_iPad"
    } else {
      xib = "PackageInstallView_iPhone"
    }

    self.preinstalledLanguageCodes = PackageInstallViewController<Resource>.checkPreinstalledResources(package: package)
    self.defaultLanguageCode = PackageInstallViewController<Resource>.chooseDefaultSelectedLanguage(from: package, promptingCode: defaultLanguageCode, preinstalleds: self.preinstalledLanguageCodes)
    super.init(nibName: xib, bundle: Bundle.init(for: PackageInstallViewController.self))

    _ = view
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  // Must be static if we want the resulting value to be stored via `let` semantics during init.
  private static func checkPreinstalledResources(package: Resource.Package) -> Set<String> {
    var preinstalleds: Set<String> = Set()

    guard let typedPackage = package as? TypedKeymanPackage<Resource> else {
      os_log("Cannot check for previously-installed resources of unexpected type", log:KeymanEngineLogger.resources, type: .error)
      return preinstalleds
    }

    if let installedResources: [Resource] = Storage.active.userDefaults.userResources(ofType: Resource.self) {
      installedResources.forEach { resource in
        if resource.packageKey == package.key {
          // The resource is from this package.
          let langResources = typedPackage.installables(forLanguage: resource.languageID)
          if langResources.contains(where: { $0.typedFullID == resource.typedFullID }){
            preinstalleds.insert(resource.languageID)
          }
        }
      }
    }

    return preinstalleds
  }

  private static func chooseDefaultSelectedLanguage(from package: Resource.Package,
                                                    promptingCode defaultLanguageCode: String?,
                                                    preinstalleds: Set<String>) -> String {
    // If a default language code was specified, always pre-select it by default.
    // Even if the corresponding resource was already installed - the user may be 'manually updating' it.
    if package.languages.contains(where: { $0.id == defaultLanguageCode }) {
      return defaultLanguageCode!
    }

    // Otherwise... find the first not-already-installed language code.  For now, at least.
    let uninstalledSet = package.installableResourceSets.first { set in
      return set.contains(where: { !preinstalleds.contains($0.languageID) })
    }

    if uninstalledSet != nil {
      return uninstalledSet!.first{ !preinstalleds.contains($0.languageID) }!.languageID
    }

    // Failing that... just return the very first language and be done with it.
    return package.installableResourceSets[0][0].languageID
  }

  override public func viewDidLoad() {
    // The 'readme' version is guaranteed.
    // "Embeds" an instance of the more general PackageWebViewController.
    packagePageController = PackageWebViewController(for: package, page: .readme)!
    packagePageController!.willMove(toParent: self)
    let wkWebView = packagePageController!.view
    wkWebView!.frame = webViewContainer.frame
    wkWebView!.backgroundColor = .white
    wkWebView!.translatesAutoresizingMaskIntoConstraints = false
    webViewContainer.addSubview(wkWebView!)
    self.addChild(packagePageController!)
    packagePageController!.didMove(toParent: self)

    // Ensure the web view fills its available space.  Required b/c iOS 9 & 10 cannot load
    // these correctly from XIBs.
    wkWebView!.topAnchor.constraint(equalTo: webViewContainer.topAnchor).isActive = true
    wkWebView!.leadingAnchor.constraint(equalTo: webViewContainer.leadingAnchor).isActive = true
    wkWebView!.bottomAnchor.constraint(equalTo: webViewContainer.bottomAnchor).isActive = true
    wkWebView!.trailingAnchor.constraint(equalTo: webViewContainer.trailingAnchor).isActive = true

    loadNavigationItems()
    leftNavigationMode = .cancel
    rightNavigationMode = .install
    navigationItem.title = package.name

    // Initialize the package info labels and the language-picker table.
    let versionFormat = NSLocalizedString("installer-label-version", bundle: engineBundle, comment: "")
    lblVersion.text = String.localizedStringWithFormat(versionFormat, package.version.description)
    if let copyright = package.metadata.info?.copyright?.description {
      lblCopyright.text = copyright
    } else {
      lblCopyright.isHidden = true
    }

    languageTable.delegate = self
    languageTable.dataSource = self

    // Set the default selection.
    let defaultRow = languages.firstIndex(where: { $0.id == self.defaultLanguageCode })!
    let defaultIndexPath = IndexPath(row: defaultRow, section: 0)
    languageTable.selectRow(at: defaultIndexPath, animated: false, scrollPosition: .top)
    languageTable.cellForRow(at: defaultIndexPath)?.accessoryType = .checkmark

    associators.forEach {
      $0.pickerInitialized()
      $0.selectLanguages(Set([self.defaultLanguageCode]))
    }

    // iPhone-only layout setup
    if let tabVC = iphoneTabViewController {
      tabVC.delegate = self

      let tabView = tabVC.view!
      tabView.translatesAutoresizingMaskIntoConstraints = false
      self.view.addSubview(tabVC.view)

      tabView.topAnchor.constraint(equalTo: topLayoutGuide.bottomAnchor).isActive = true
      tabView.bottomAnchor.constraint(equalTo: bottomLayoutGuide.topAnchor).isActive = true
      tabView.leftAnchor.constraint(equalTo: view.leftAnchor).isActive = true
      tabView.rightAnchor.constraint(equalTo: view.rightAnchor).isActive = true

      tabVC.tabBar.items![0].title = NSLocalizedString("installer-label-package-info", bundle: engineBundle, comment: "")
      tabVC.tabBar.items![1].title = NSLocalizedString("installer-label-select-languages", bundle: engineBundle, comment: "")

      // Prevents the tab view from being obscured by the navigation bar.
      // It's weird, yeah.
      edgesForExtendedLayout = []

      rightNavigationMode = .next

      if #available(*, iOS 13.4) {
        // No icon issues here.
      } else {
        tabVC.tabBar.items![0].titlePositionAdjustment = UIOffset(horizontal: 0, vertical: -16)
        tabVC.tabBar.items![1].titlePositionAdjustment = UIOffset(horizontal: 0, vertical: -16)
      }
    }

    // If there's only one language in the package, hide the language picker.
    if languages.count <= 1 {
      if let sourceTagConstraint = ipadTagWidthConstraint {
        // Rebuild the width constraint, setting it to zero width.
        // The 'proper' iOS approach is to disable the old one and replace it with a new one.
        sourceTagConstraint.isActive = false
        let hideTagConstraint = NSLayoutConstraint(item: sourceTagConstraint.firstItem as Any,
                                                   attribute: sourceTagConstraint.firstAttribute,
                                                   relatedBy: sourceTagConstraint.relation,
                                                   toItem: sourceTagConstraint.secondItem,
                                                   attribute: sourceTagConstraint.secondAttribute,
                                                   multiplier: 0,
                                                   constant: 0)
        hideTagConstraint.isActive = true
      } else if let tabVC = iphoneTabViewController {
        // Hide the tab bar, making it appear as if only the "info view" portion exists.
        tabVC.tabBar.isHidden = true

        // Since we won't be showing the user a language list, allow them to install from the info view.
        rightNavigationMode = .install
      }
    }
  }

  private func loadNavigationItems() {
    navMapping[.cancel] = UIBarButtonItem(title: NSLocalizedString("command-cancel", bundle: engineBundle, comment: ""), style: .plain,
                                    target: self,
                                    action: #selector(cancelBtnHandler))

    navMapping[.back] = UIBarButtonItem(title: NSLocalizedString("command-back", bundle: engineBundle, comment: ""), style: .plain,
                                    target: self,
                                    action: #selector(backBtnHandler))

    navMapping[.none] = UIBarButtonItem(title: NSLocalizedString("command-install", bundle: engineBundle, comment: ""), style: .plain,
                                     target: self,
                                     action: nil)
    navMapping[.none]!.isEnabled = false

    navMapping[.next] = UIBarButtonItem(title: NSLocalizedString("command-next", bundle: engineBundle, comment: ""), style: .plain,
                                    target: self,
                                    action: #selector(nextBtnHandler))

    navMapping[.install] = UIBarButtonItem(title: NSLocalizedString("command-install", bundle: engineBundle, comment: ""), style: .plain,
                                     target: self,
                                     action: #selector(installBtnHandler))
  }

  private var leftNavigationMode: NavigationMode {
    get {
      return leftNavMode
    }

    set(mode) {
      if mayPick {
        leftNavMode = mode

        navigationItem.leftBarButtonItem = navMapping[mode]
      }
    }
  }

  private var rightNavigationMode: NavigationMode {
    get {
      return rightNavMode
    }

    set(mode) {
      if mayPick {
        rightNavMode = mode

        navigationItem.rightBarButtonItem = navMapping[mode]
      }
    }
  }

  public func tabBarController(_ tabBarController: UITabBarController, shouldSelect viewController: UIViewController) -> Bool {
    if viewController.view.tag == 1, languages.count <= 1 {
      return false
    } else {
      return true
    }
  }

  public func tabBarController(_ tabBarController: UITabBarController, didSelect item: UIViewController) {
    if item.view.tag == 0 {
      leftNavigationMode = .cancel
      rightNavigationMode = .next
    } else if item.view.tag == 1 {
      leftNavigationMode = .back

      if languageTable.indexPathsForSelectedRows?.count ?? 0 == 0 {
        rightNavigationMode = .none
      } else {
        rightNavigationMode = .install
      }
    }
  }

  @objc func cancelBtnHandler() {
    // If it is not the root view of a navigationController, just pop it off the stack.
    if let navVC = self.navigationController, navVC.viewControllers[0] != self {
      navVC.popViewController(animated: true)
    } else { // Otherwise, if the root view of a navigation controller, dismiss it outright.  (pop not available)
      dismiss(animated: true)
    }
    self.pickingCompletionHandler(nil)
    self.associators.forEach { $0.pickerDismissed() }
  }

  @objc func backBtnHandler() {
    guard let tabVC = iphoneTabViewController else {
      return
    }
    // Should only occur for iPhone layouts
    tabVC.selectedIndex -= 1
    // Is not automatically called!
    tabBarController(tabVC, didSelect: tabVC.viewControllers![tabVC.selectedIndex])
  }

  @objc func nextBtnHandler() {
    guard let tabVC = iphoneTabViewController else {
      return
    }
    // Should only occur for iPhone layouts
    tabVC.selectedIndex += 1
    // Is not automatically called!
    tabBarController(tabVC, didSelect: tabVC.viewControllers![tabVC.selectedIndex])
  }

  @objc func installBtnHandler() {
    let selectedItems = self.languageTable.indexPathsForSelectedRows ?? []
    let selectedLanguageCodes = selectedItems.map { self.languages[$0.row].id }

    let selectedResources = self.package.installableResourceSets.flatMap { $0.filter { selectedLanguageCodes.contains($0.languageID) }} as! [Resource]

    // Always reload after installing or updating resources.
    Manager.shared.inputViewController.setShouldReload()
    self.pickingCompletionHandler(selectedResources.map { $0.typedFullID })

    // Prevent swipe dismissal.
    if #available(iOS 13.0, *) {
      self.isModalInPresentation = true
    }

    // No more selection-manipulation allowed.
    // This matters when there's no welcome page available.
    languageTable.isUserInteractionEnabled = false
    // Prevent extra 'install' commands and nav-bar related manipulation.
    self.navigationItem.leftBarButtonItem?.isEnabled = false
    self.rightNavigationMode = .none
    self.mayPick = false

    let dismissalBlock = {
      self.associators.forEach { $0.pickerFinalized() }
    }

    // First, show the package's welcome - if it exists.
    if let welcomeVC = PackageWebViewController(for: package, page: .welcome) {
      // Prevent swipe dismissal.
      if #available(iOS 13.0, *) {
        welcomeVC.isModalInPresentation = true
      }

      let subNavVC = UINavigationController(rootViewController: welcomeVC)
      _ = subNavVC.view

      let doneItem = UIBarButtonItem(title: NSLocalizedString("command-done",
                                                              bundle: engineBundle,
                                                              comment: ""),
                                     style: .plain,
                                     target: self,
                                     action: #selector(self.onWelcomeDismissed))

      // The view's navigation buttoms need to be set on its controller's navigation item,
      // not the UINavigationController's navigationItem.
      welcomeVC.navigationItem.rightBarButtonItem = doneItem

      // We need to listen to delegated presentation events on the view-controller being presented.
      // This version handles iOS 13's "page sheet" slide-dismissal.
      subNavVC.presentationController?.delegate = self

      self.present(subNavVC, animated: true, completion: nil)
      self.welcomeView = welcomeVC.view

      self.dismissalBlock = {
        // Tells the user that we've received the 'done' command.
        doneItem.isEnabled = false
        dismissalBlock()
      }
    } else {
      self.dismissalBlock = dismissalBlock
      onWelcomeDismissed()
    }
  }

  public func presentationControllerDidDismiss(_ presentationController: UIPresentationController) {
    onWelcomeDismissed()
  }

  @objc private func onWelcomeDismissed() {
    if let dismissalBlock = self.dismissalBlock {
      dismissalBlock()
      self.dismissalBlock = nil

      // Tell our owner (the AssociatingPackageInstaller) that all UI interactions are done.
      // Triggers synchronization code, so make sure it only runs once!
      self.uiCompletionHandler()
    }

    // Show a spinner forever.
    // When installation is complete, this controller's view will be dismissed,
    // removing said spinner.
    let activitySpinner = Alerts.constructActivitySpinner()

    // Determine the top-most view.  If we're presenting the welcome page, THAT.
    // If we aren't, our directly-owned view should be the top-most.
    let activeView: UIView = self.welcomeView ?? view

    activitySpinner.center = activeView.center
    activitySpinner.startAnimating()
    activeView.addSubview(activitySpinner)
    
    activitySpinner.centerXAnchor.constraint(equalTo: activeView.centerXAnchor).isActive = true
    activitySpinner.centerYAnchor.constraint(equalTo: activeView.centerYAnchor).isActive = true

    // Note:  we do NOT block user interaction; we merely bind them to the active view.
    // Why prevent them from reading more on the welcome page while they wait?
  }

  public func tableView(_ tableView: UITableView, titleForHeaderInSection: Int) -> String? {
    return NSLocalizedString("installer-section-available-languages", bundle: engineBundle, comment: "")
  }

  public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    switch section {
      case 0:
        return languages.count
      default:
        return 0
    }
  }

  public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "any"
    var cell: UITableViewCell

    if let reusedCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      cell = reusedCell
    } else {
      let selectionColor = UIView()

      selectionColor.backgroundColor = Colors.selectionPrimary

      cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
      cell.selectedBackgroundView = selectionColor
    }

    cell.isUserInteractionEnabled = true
    cell.backgroundColor = .none

    switch indexPath.section {
      case 0:
        let index = indexPath.row
        cell.detailTextLabel?.text = languages[index].name

        // Check:  is the language ALREADY installed?
        // The checkmark is not properly managed by default.
        let shouldCheck = languageTable.indexPathsForSelectedRows?.contains(indexPath) ?? false
        if self.preinstalledLanguageCodes.contains(languageCodeForCellAt(indexPath)) {
          setCellStyle(cell, style: shouldCheck ? .install : .preinstalled)
        } else {
          setCellStyle(cell, style: shouldCheck ? .install : .none)
        }
        return cell
      default:
        return cell
    }
  }

  private func languageCodeForCellAt(_ indexPath: IndexPath) -> String {
    return languages[indexPath.row].id
  }

  private func setCellStyle(_ cell: UITableViewCell, style: CellStyle) {
    var textColor: UIColor
    if #available(*, iOS 13.0) {
      textColor = .label
    } else {
      textColor = .black
    }

    switch style {
      case .none:
        cell.detailTextLabel?.textColor = textColor

        cell.accessoryType = .none
      case .preinstalled:
        cell.detailTextLabel?.textColor = .systemGray

        cell.accessoryType = .checkmark
        cell.tintColor = .systemGray
      case .install:
        cell.detailTextLabel?.textColor = textColor

        cell.accessoryType = .checkmark
        cell.tintColor = .systemBlue
    }
  }

  public func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    guard let cell = tableView.cellForRow(at: indexPath) else {
      return
    }

    guard cell.isUserInteractionEnabled == true else {
      tableView.deselectRow(at: indexPath, animated: false)
      return
    }

    rightNavigationMode = .install
    setCellStyle(cell, style: .install)

    associators.forEach { $0.selectLanguages( Set([languages[indexPath.row].id]) ) }
  }

  public func tableView(_ tableView: UITableView, didDeselectRowAt indexPath: IndexPath) {
    guard let cell = tableView.cellForRow(at: indexPath) else {
      return
    }

    if languageTable.indexPathsForSelectedRows?.count ?? 0 == 0 && self.preinstalledLanguageCodes.count == 0 {
      rightNavigationMode = .none
    } else {
      rightNavigationMode = .install
    }

    let wasPreinstalled = self.preinstalledLanguageCodes.contains(languageCodeForCellAt(indexPath))
    setCellStyle(cell, style: wasPreinstalled ? .preinstalled : .none)

    associators.forEach { $0.deselectLanguages( Set([languages[indexPath.row].id]) ) }
  }

  internal func progressUpdate<Package: TypedKeymanPackage<Resource>>(_ status: AssociatingPackageInstaller<Resource, Package>.Progress) where Resource.Package == Package {
    switch(status) {
        case .starting, .inProgress:
          // nothing worth note
          break
        // All UI interactions have been completed AND installation is fully complete.
        case .complete, .cancelled:
          if let nvc = self.navigationController {
          self.dismiss(animated: true)
            nvc.popToRootViewController(animated: false)
          } else { // Otherwise, if the root view of a navigation controller, dismiss it outright.  (pop not available)
            self.dismiss(animated: true)
          }
      }
  }
}
