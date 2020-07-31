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

public class PackageInstallViewController<Resource: LanguageResource>: UIViewController, UITableViewDelegate, UITableViewDataSource {

  public typealias CompletionHandler = ([Resource.FullID]?) -> Void

  // Needed to support iOS 9 + 10.
  @IBOutlet weak var webViewContainer: UIView!
  @IBOutlet weak var lblVersion: UILabel!
  @IBOutlet weak var lblCopyright: UILabel!
  @IBOutlet weak var languageTable: UITableView!

  // May be altered programatically.
  @IBOutlet weak var ipadTagWidthConstraint: NSLayoutConstraint?

  let package: Resource.Package
  var wkWebView: WKWebView?
  let completionHandler: CompletionHandler
  let isCustom: Bool
  let languages: [Language]

  public init(for package: Resource.Package, isCustom: Bool, completionHandler: @escaping CompletionHandler) {
    self.package = package
    self.completionHandler = completionHandler
    self.isCustom = isCustom
    self.languages = package.languages

    var xib: String
    if Device.current.isPad {
      xib = "PackageInstallView_iPad"
    } else {
      xib = "PackageInstallView_base"
    }

    super.init(nibName: xib, bundle: Bundle.init(for: PackageInstallViewController.self))

    _ = view
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override public func loadView() {
    super.loadView()

    wkWebView = WKWebView.init(frame: webViewContainer.frame)
    wkWebView!.backgroundColor = .white
    wkWebView!.translatesAutoresizingMaskIntoConstraints = false
    webViewContainer.addSubview(wkWebView!)

//    // Ensure the web view fills its available space.
//    wkWebView?.autoresizingMask = [.flexibleWidth, .flexibleHeight]
    wkWebView!.topAnchor.constraint(equalTo: webViewContainer.topAnchor).isActive = true
    wkWebView!.leadingAnchor.constraint(equalTo: webViewContainer.leadingAnchor).isActive = true

    wkWebView!.bottomAnchor.constraint(equalTo: webViewContainer.bottomAnchor).isActive = true
    wkWebView!.trailingAnchor.constraint(equalTo: webViewContainer.trailingAnchor).isActive = true

    let cancelBtn = UIBarButtonItem(title: NSLocalizedString("command-cancel", bundle: engineBundle, comment: ""), style: .plain,
                                    target: self,
                                    action: #selector(cancelBtnHandler))
    let installBtn = UIBarButtonItem(title: NSLocalizedString("command-install", bundle: engineBundle, comment: ""), style: .plain,
                                     target: self,
                                     action: #selector(installBtnHandler))

    navigationItem.leftBarButtonItem = cancelBtn
    navigationItem.rightBarButtonItem = installBtn
    navigationItem.title = package.name

    let versionFormat = NSLocalizedString("installer-label-version", bundle: engineBundle, comment: "")
    lblVersion.text = String.localizedStringWithFormat(versionFormat, package.version.description)
    if let copyright = package.metadata.info?.copyright?.description {
      lblCopyright.text = copyright
    } else {
      lblCopyright.isHidden = true
    }

    languageTable.delegate = self
    languageTable.dataSource = self

    let defaultRow = languages.firstIndex(where: { $0.id == package.installableResourceSets[0][0].languageID })!
    let defaultIndexPath = IndexPath(row: defaultRow, section: 0)
    languageTable.selectRow(at: defaultIndexPath, animated: false, scrollPosition: .top)
    languageTable.cellForRow(at: defaultIndexPath)?.accessoryType = .checkmark

    // If we're using the iPad layout and there's only one language in the package,
    // hide the language picker.
    if languages.count == 1 {
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
      } else {
        // Do iPhone layout single-lang-tag things.
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

  @objc func cancelBtnHandler() {
    dismiss(animated: true, completion: {
      self.completionHandler(nil)
    })
  }

  @objc func installBtnHandler() {
    dismiss(animated: true, completion: {
      let selectedItems = self.languageTable.indexPathsForSelectedRows ?? []
      let selectedLanguageCodes = selectedItems.map { self.languages[$0.row].id }

      let selectedResources = self.package.installableResourceSets.flatMap { $0.filter { selectedLanguageCodes.contains($0.languageID) }} as! [Resource]

      self.completionHandler(selectedResources.map { $0.typedFullID })
    })
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
  }

  public func tableView(_ tableView: UITableView, didDeselectRowAt indexPath: IndexPath) {
    if languageTable.indexPathsForSelectedRows?.count ?? 0 == 0 {
      navigationItem.rightBarButtonItem?.isEnabled = false
    }
    tableView.cellForRow(at: indexPath)?.accessoryType = .none
  }
}
