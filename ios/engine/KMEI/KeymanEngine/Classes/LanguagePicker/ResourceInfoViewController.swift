//
//  ResourceInfoViewController.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 1/17/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import UIKit

/**
 * At present, this class only supports keyboard resources.  The base design is partially refactored to
 * eventually support lexical model resources as well, but additional work is needed before this class
 * will be ready... possibly as a common base class. 
 */
class ResourceInfoViewController: UIViewController, UIAlertViewDelegate, UITableViewDelegate, UITableViewDataSource {
  // Collectively used to determine if a keyboard may be deleted.
  var keyboardCount: Int = 0
  var keyboardIndex: Int = 0
  var isCustomKeyboard: Bool = false // also used to toggle QR code gen + display

  var keyboardCopyright: String = ""

  private var infoArray = [[String: String]]()

  let resource: AnyLanguageResource

  @IBOutlet weak var scrollView: UIScrollView!
  @IBOutlet weak var contentView: UIView!

  @IBOutlet weak var tableView: UITableView!
  @IBOutlet weak var qrImageView: UIImageView!
  @IBOutlet weak var shareLabel: UILabel!
  @IBOutlet weak var tableHeightConstraint: NSLayoutConstraint!
  @IBOutlet weak var labelHeightConstraint: NSLayoutConstraint!

  init(for resource: AnyLanguageResource) {
    self.resource = resource

    super.init(nibName: "ResourceInfoView", bundle: Bundle.init(for: ResourceInfoViewController.self))
  }

  required init?(coder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    infoArray = [[String: String]]()
    infoArray.append([
      "title": "Keyboard version",
      "subtitle": resource.version
      ])

    if !isCustomKeyboard {
      infoArray.append([
        "title": "Help link",
        "subtitle": ""
        ])
    }
    infoArray.append([
      "title": "Uninstall keyboard",
      "subtitle": ""
      ])

    tableView.dataSource = self
    tableView.delegate = self

    tableView.reloadData()
    
    // Generate & display the QR code!
    if let resourceURL = resource.sharableURL {
      if let qrImg = generateQRCode(from: resourceURL) {
        qrImageView.image = qrImg
      } else {
        log.info("Unable to generate QR code for URL: \(resourceURL)")
      }
    } else {
      // No resource-sharing link available.  Hide the text label!
      shareLabel.isHidden = true
    }
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
    log.info("didAppear: ResourceInfoViewController")
}

  func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }

  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    if isCustomKeyboard {
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
    if !isCustomKeyboard {
      if indexPath.row == 1 {
        let url = URL(string: "\(KeymanHosts.HELP_KEYMAN_COM)/keyboard/\(resource.id)/\(resource.version)/")!
        if let openURL = Manager.shared.openURL {
          _ = openURL(url)
        } else {
          log.error("openURL not set in Manager. Failed to open \(url)")
        }
      } else if indexPath.row == 2 {
        showDeleteKeyboard()
      }
    } else if indexPath.row == 1 {
      showDeleteKeyboard()
    }
  }

  private func fetchedKeyboardData(_ data: Data) {
    guard let json = (try? JSONSerialization.jsonObject(with: data, options: [])) as? [AnyHashable: Any] else {
      return
    }
    let keyboards = json[Key.language] as? [Any]
    let kbDict = keyboards?[0] as? [AnyHashable: Any]
    var info = infoArray[1]
    let copyright = kbDict?[Key.keyboardCopyright] as? String ?? "Unknown"

    info["subtitle"] = copyright
    infoArray[1] = info
    tableView.reloadData()
  }

  func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    cell.accessoryType = .none
    cell.textLabel?.text = infoArray[indexPath.row]["title"]
    cell.detailTextLabel?.text = infoArray[indexPath.row]["subtitle"]
    cell.tag = indexPath.row

    if !isCustomKeyboard {
      if indexPath.row == 1 {
        cell.accessoryType = .disclosureIndicator
      } else if indexPath.row == 2 && !canDeleteKeyboard {
        cell.isUserInteractionEnabled = false
        cell.textLabel?.isEnabled = false
        cell.detailTextLabel?.isEnabled = false
      }
    } else if indexPath.row == 1 && !canDeleteKeyboard {
      cell.isUserInteractionEnabled = false
      cell.textLabel?.isEnabled = false
      cell.detailTextLabel?.isEnabled = false
    }
  }

  private var canDeleteKeyboard: Bool {
    if !Manager.shared.canRemoveKeyboards {
      return false
    }

    if !Manager.shared.canRemoveDefaultKeyboard {
      return keyboardIndex != 0
    }

    if keyboardIndex > 0 {
      return true
    }
    return keyboardCount > 1
  }

  private func showDeleteKeyboard() {
    let alertController = UIAlertController(title: title ?? "", message: "Would you like to delete this keyboard?",
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: "Cancel",
                                            style: UIAlertAction.Style.cancel,
                                            handler: nil))
    alertController.addAction(UIAlertAction(title: "Delete",
                                            style: UIAlertAction.Style.default,
                                            handler: deleteHandler))

    self.present(alertController, animated: true, completion: nil)
  }

  func deleteHandler(withAction action: UIAlertAction) {
    if Manager.shared.removeKeyboard(at: keyboardIndex) {
        navigationController?.popToRootViewController(animated: true)
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
