//
//  ResourceInfoViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 1/17/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit
import WebKit
import os.log

class ResourceInfoViewController<Resource: LanguageResource>: UIViewController, UIAlertViewDelegate, UITableViewDelegate, UITableViewDataSource {
  // The data backing our UI text in the UITableView.
  private var infoArray = [[String: String]]()
  
  let resource: Resource
  let package: Resource.Package?
  let mayDelete: Bool
  
  @IBOutlet weak var scrollView: UIScrollView!
  @IBOutlet weak var contentView: UIView!
  
  @IBOutlet weak var tableView: UITableView!
  @IBOutlet weak var qrImageView: UIImageView!
  @IBOutlet weak var shareLabel: UILabel!
  @IBOutlet weak var tableHeightConstraint: NSLayoutConstraint!
  @IBOutlet weak var labelHeightConstraint: NSLayoutConstraint!
  
  init(for resource: Resource, mayDelete: Bool = false) {
    self.resource = resource
    self.package = ResourceFileManager.shared.getInstalledPackage(for: resource)
    self.mayDelete = mayDelete
    
    super.init(nibName: "ResourceInfoView", bundle: Bundle.init(for: ResourceInfoViewController.self))
  }
  
  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()
    
    var versionLabelKey: String
    var helpLabelKey: String
    var uninstallLabelKey: String
    
    if Resource.self == InstallableLexicalModel.self {
      versionLabelKey = "info-label-version-lexical-model"
      helpLabelKey = "info-command-help-lexical-model"
      uninstallLabelKey = "command-uninstall-lexical-model"
    } else {
      versionLabelKey = "info-label-version-keyboard"
      helpLabelKey = "info-command-help-keyboard"
      uninstallLabelKey = "command-uninstall-keyboard"
    }
    
    infoArray = [[String: String]]()
    infoArray.append([
      "title": NSLocalizedString(versionLabelKey, bundle: engineBundle, comment: ""),
      "subtitle": resource.version
    ])
    
    if package?.pageURL(for: .welcome) != nil {
      infoArray.append([
        "title": NSLocalizedString(helpLabelKey, bundle: engineBundle, comment: ""),
        "subtitle": ""
      ])
    }
    infoArray.append([
      "title": NSLocalizedString(uninstallLabelKey, bundle: engineBundle, comment: ""),
      "subtitle": ""
    ])
    
    tableView.dataSource = self
    tableView.delegate = self
    
    tableView.reloadData()
    
    // Generate & display the QR code!
    if package?.distributionMethod ?? .unknown == .cloud {
      if let resourceURL = resource.sharableURL {
        if let qrImg = generateQRCode(from: resourceURL) {
          qrImageView.image = qrImg
        } else {
          let message = "Unable to generate QR code for URL: \(resourceURL)"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        }
      } else {
        // No resource-sharing link available.  Hide the text label!
        shareLabel.isHidden = true
      }
    } else {
      shareLabel.isHidden = true
    }
    
    self.title = resource.name
  }
  
  // Should be supported in iOS 7+.  We only support 9+, so we should be fine here.
  // Many thanks to https://www.hackingwithswift.com/example-code/media/how-to-create-a-qr-code
  func generateQRCode(from string: String) -> UIImage? {
    let data = string.data(using: String.Encoding.ascii)
    
    if let filter = CIFilter(name: "CIQRCodeGenerator") {
      filter.setValue(data, forKey: "inputMessage")
      let transform = CGAffineTransform(scaleX: 5, y: 5) // Results in 175 px x 175 px
      
      if let output = filter.outputImage?.transformed(by: transform) {
        return UIImage(ciImage: output)
      }
    }
    
    return nil
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    navigationController?.setToolbarHidden(true, animated: true)
    os_log("didAppear: ResourceInfoViewController", log:KeymanEngineLogger.resources, type: .info)
  }
  
  func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }
  
  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    if package?.pageURL(for: .welcome) == nil {
      return 2
    } else {
      return 3
    }
  }
  
  func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      return cell
    }
    return UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
  }
  
  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    if package?.pageURL(for: .welcome) != nil {
      if indexPath.row == 1 {
        let welcomeVC = PackageWebViewController(for: package!, page: .welcome)!
        self.navigationController?.pushViewController(welcomeVC, animated: true)
      } else if indexPath.row == 2 {
        showDeleteResource()
      }
    } else if indexPath.row == 1 {
      showDeleteResource()
    }
  }
  
  func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    cell.accessoryType = .none
    cell.textLabel?.text = infoArray[indexPath.row]["title"]
    cell.detailTextLabel?.text = infoArray[indexPath.row]["subtitle"]
    cell.tag = indexPath.row
    
    if package?.pageURL(for: .welcome) != nil {
      if indexPath.row == 1 {
        cell.accessoryType = .disclosureIndicator
      } else if indexPath.row == 2 && !mayDelete {
        cell.isUserInteractionEnabled = false
        cell.textLabel?.isEnabled = false
        cell.detailTextLabel?.isEnabled = false
      }
    } else if indexPath.row == 1 && !mayDelete {
      cell.isUserInteractionEnabled = false
      cell.textLabel?.isEnabled = false
      cell.detailTextLabel?.isEnabled = false
    }
  }
  
  private func showDeleteResource() {
    var uninstallLabelKey: String
    
    if Resource.self == InstallableLexicalModel.self {
      uninstallLabelKey = "command-uninstall-lexical-model-confirm"
    } else {
      uninstallLabelKey = "command-uninstall-keyboard-confirm"
    }
    
    let uninstallHelp = NSLocalizedString(uninstallLabelKey, bundle: engineBundle, comment: "")
    let alertController = UIAlertController(title: title ?? "", message: uninstallHelp,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: NSLocalizedString("command-cancel", bundle: engineBundle, comment: ""),
                                            style: UIAlertAction.Style.cancel,
                                            handler: nil))
    alertController.addAction(UIAlertAction(title: NSLocalizedString("command-uninstall", bundle: engineBundle, comment: ""),
                                            style: UIAlertAction.Style.default,
                                            handler: deleteHandler))
    
    self.present(alertController, animated: true, completion: nil)
  }
  
  func deleteHandler(withAction action: UIAlertAction) {
    if let lexicalModel = resource as? InstallableLexicalModel {
      if Manager.shared.removeLexicalModel(withFullID: lexicalModel.typedFullID) {
        navigationController?.popToRootViewController(animated: true)
      }
    } else if let keyboard = resource as? InstallableKeyboard {
      if Manager.shared.removeKeyboard(withFullID: keyboard.typedFullID) {
        navigationController?.popToRootViewController(animated: true)
      }
    }
  }
  
  override func viewDidLayoutSubviews() {
    super.updateViewConstraints()
    
    // Sets scrolling height
    scrollView.contentSize = contentView.frame.size
    
    // Set the table's height to match its contents, plus a bit of space
    // so that the separator below the final item appears.
    tableHeightConstraint.constant = self.tableView.contentSize.height + 4
    
    labelHeightConstraint.constant = self.shareLabel.intrinsicContentSize.height
  }
}
