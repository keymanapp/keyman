//
//  LexicalModelInfoViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/19/19.
//  Copyright © 2019 SIL International. All rights reserved.
//
import UIKit
import Foundation

class LexicalModelInfoViewController: UITableViewController, UIAlertViewDelegate {
  var lexicalModelCount: Int = 0
  var lexicalModelIndex: Int = 0
  var lexicalModelID: String = ""
  var languageID: String = ""
  var lexicalModelVersion: String = ""
  var lexicalModelCopyright: String = ""
  var isCustomLexicalModel: Bool = false
  
  private var infoArray = [[String: String]]()
  
  override func viewDidLoad() {
    super.viewDidLoad()
  
    infoArray = [[String: String]]()
    infoArray.append([
        "title": NSLocalizedString("info-label-version-lexical-model", bundle: engineBundle, comment: ""),
        "subtitle": lexicalModelVersion
        ])
  
    if !isCustomLexicalModel {
      infoArray.append([
          "title": NSLocalizedString("info-command-help", bundle: engineBundle, comment: ""),
          "subtitle": ""
          ])
    }
    infoArray.append([
        "title": NSLocalizedString("command-uninstall-lexical-model", bundle: engineBundle, comment: ""),
        "subtitle": ""
        ])
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    navigationController?.setToolbarHidden(true, animated: true)
    log.info("didAppear: LexicalModelInfoViewController")
  }
  
  override func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }
  
  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    if isCustomLexicalModel {
      return 2
    } else {
      return 3
    }
  }
  
  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      return cell
    }
    return UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
  }
  
  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    if !isCustomLexicalModel {
      if indexPath.row == 1 {
        let url = URL(string: "\(KeymanHosts.HELP_KEYMAN_COM)/lexicalModel/\(lexicalModelID)/\(lexicalModelVersion)/")!
        if let openURL = Manager.shared.openURL {
          _ = openURL(url)
        } else {
          log.error("openURL not set in Manager. Failed to open \(url)")
        }
      } else if indexPath.row == 2 {
        showDeleteLexicalModel()
      }
    } else if indexPath.row == 1 {
      showDeleteLexicalModel()
    }
  }
  
  private func fetchedLexicalModelData(_ data: Data) {
    guard let json = (try? JSONSerialization.jsonObject(with: data, options: [])) as? [AnyHashable: Any] else {
      return
    }
    let lexicalModels = json[Key.language] as? [Any]
    let kbDict = lexicalModels?[0] as? [AnyHashable: Any]
    var info = infoArray[1]
    let copyright = kbDict?[Key.lexicalModelCopyright] as? String ?? "Unknown"
  
    info["subtitle"] = copyright
    infoArray[1] = info
    tableView.reloadData()
  }
  
  override func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    cell.accessoryType = .none
    cell.textLabel?.text = infoArray[indexPath.row]["title"]
    cell.detailTextLabel?.text = infoArray[indexPath.row]["subtitle"]
    cell.tag = indexPath.row
  
    if !isCustomLexicalModel {
      if indexPath.row == 1 {
        cell.accessoryType = .disclosureIndicator
      }
    }
  }
  
  private func showDeleteLexicalModel() {
    let helpText = NSLocalizedString("command-uninstall-lexical-model-confirm", bundle: engineBundle, comment: "")
    let alertController = UIAlertController(title: title ?? "", message: helpText,
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
    if Manager.shared.removeLexicalModel(at: lexicalModelIndex) {
      navigationController?.popToRootViewController(animated: true)
    }
  }
}
