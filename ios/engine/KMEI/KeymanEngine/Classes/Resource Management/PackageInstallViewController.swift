//
//  PackageInstallViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 12/18/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import WebKit

public class PackageInstallViewController<Resource: LanguageResource>: UIViewController, UITableViewDelegate, UITableViewDataSource {

  public typealias CompletionHandler = ([Resource.FullID]?) -> Void

  // Needed to support iOS 9 + 10.
  @IBOutlet weak var webViewContainer: UIView!
  @IBOutlet weak var lblVersion: UILabel!
  @IBOutlet weak var lblCopyright: UILabel!
  @IBOutlet weak var languageTable: UITableView!

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

    super.init(nibName: "PackageInstallView", bundle: Bundle.init(for: PackageInstallViewController.self))

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

    lblVersion.text = "Version: \(package.version)"
    if let copyright = package.metadata.info?.copyright?.description {
      lblCopyright.text = copyright
    } else {
      lblCopyright.isHidden = true
    }

    languageTable.delegate = self
    languageTable.dataSource = self

    let defaultRow = languages.firstIndex(where: { $0.id == package.installableResourceSets[0][0].languageID })!
    languageTable.selectRow(at: IndexPath(row: defaultRow, section: 0), animated: false, scrollPosition: .top)
  }

  override public func viewWillAppear(_ animated: Bool) {
    if let welcomeURL = package.welcomePageURL {
      wkWebView?.loadFileURL(welcomeURL, allowingReadAccessTo: package.sourceFolder)
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
    return "Available languages"
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
  }

  public func tableView(_ tableView: UITableView, didDeselectRowAt indexPath: IndexPath) {
    if languageTable.indexPathsForSelectedRows?.count ?? 0 == 0 {
      navigationItem.rightBarButtonItem?.isEnabled = false
    }
  }
}
