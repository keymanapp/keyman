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

public class PackageInstallViewController<Resource: LanguageResource>: UIViewController, UITableViewDelegate, UITableViewDataSource, UITabBarControllerDelegate {
  private enum NavigationMode: Int {  // b/c hashable
    // left nav
    case cancel
    case back

    // right nav
    case none
    case next
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
  var wkWebView: WKWebView?
  let completionHandler: CompletionHandler
  let defaultLanguageCode: String
  let associators: [LanguagePickAssociator]
  let languages: [Language]

  private var leftNavMode: NavigationMode = .cancel
  private var rightNavMode: NavigationMode = .none
  private var navMapping: [NavigationMode : UIBarButtonItem] = [:]

  public init(for package: Resource.Package,
              defaultLanguageCode: String? = nil,
              languageAssociators: [LanguagePickAssociator] = [],
              completionHandler: @escaping CompletionHandler) {
    self.package = package
    self.completionHandler = completionHandler
    self.languages = package.languages

    let packageFirstLangCode = package.installableResourceSets[0][0].languageID
    self.defaultLanguageCode = package.languages.contains(where: { $0.id == defaultLanguageCode }) ? defaultLanguageCode! : packageFirstLangCode
    self.associators = languageAssociators

    var xib: String
    if Device.current.isPad {
      xib = "PackageInstallView_iPad"
    } else {
      xib = "PackageInstallView_iPhone"
    }

    super.init(nibName: xib, bundle: Bundle.init(for: PackageInstallViewController.self))

    _ = view
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override public func viewDidLoad() {
    wkWebView = WKWebView.init(frame: webViewContainer.frame)
    wkWebView!.backgroundColor = .white
    wkWebView!.translatesAutoresizingMaskIntoConstraints = false
    webViewContainer.addSubview(wkWebView!)

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

  override public func viewWillAppear(_ animated: Bool) {
    if let readmeURL = package.readmePageURL {
      wkWebView?.loadFileURL(readmeURL, allowingReadAccessTo: package.sourceFolder)
    } else {
      wkWebView?.loadHTMLString(package.infoHtml(), baseURL: nil)
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
      leftNavMode = mode

      navigationItem.leftBarButtonItem = navMapping[mode]
    }
  }

  private var rightNavigationMode: NavigationMode {
    get {
      return rightNavMode
    }

    set(mode) {
      rightNavMode = mode

      navigationItem.rightBarButtonItem = navMapping[mode]
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
    self.completionHandler(nil)
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
    // If it is not the root view of a navigationController, just pop it off the stack.
    if let navVC = self.navigationController, navVC.viewControllers[0] != self {
      navVC.popViewController(animated: true)
    } else { // Otherwise, if the root view of a navigation controller, dismiss it outright.  (pop not available)
      dismiss(animated: true)
    }

    let selectedItems = self.languageTable.indexPathsForSelectedRows ?? []
    let selectedLanguageCodes = selectedItems.map { self.languages[$0.row].id }

    let selectedResources = self.package.installableResourceSets.flatMap { $0.filter { selectedLanguageCodes.contains($0.languageID) }} as! [Resource]

    self.completionHandler(selectedResources.map { $0.typedFullID })
    self.associators.forEach { $0.pickerFinalized() }
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

      // The checkmark is not properly managed by default.
      let shouldCheck = languageTable.indexPathsForSelectedRows?.contains(indexPath) ?? false
      // Note for later:  also ensure that it wasn't already installed.  (exception to rule above)
      cell.accessoryType = shouldCheck ? .checkmark : .none
    } else {
      let selectionColor = UIView()

      if #available(iOSApplicationExtension 11.0, *) {
        selectionColor.backgroundColor = UIColor(named: "SelectionPrimary")
      } else {
        selectionColor.backgroundColor = Colors.selectionPrimary
      }

      cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
      cell.selectedBackgroundView = selectionColor
    }

    switch indexPath.section {
      case 0:
        let index = indexPath.row
        cell.detailTextLabel?.text = languages[index].name
        return cell
      default:
        return cell
    }
  }

  public func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    navigationItem.rightBarButtonItem?.isEnabled = true
    tableView.cellForRow(at: indexPath)?.accessoryType = .checkmark

    associators.forEach { $0.selectLanguages( Set([languages[indexPath.row].id]) ) }
  }

  public func tableView(_ tableView: UITableView, didDeselectRowAt indexPath: IndexPath) {
    if languageTable.indexPathsForSelectedRows?.count ?? 0 == 0 {
      rightNavigationMode = .none
    } else {
      rightNavigationMode = .install
    }
    tableView.cellForRow(at: indexPath)?.accessoryType = .none

    associators.forEach { $0.deselectLanguages( Set([languages[indexPath.row].id]) ) }
  }
}
