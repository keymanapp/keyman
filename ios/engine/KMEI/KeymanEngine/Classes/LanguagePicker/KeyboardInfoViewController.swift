//
//  KeyboardInfoViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-12.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

class KeyboardInfoViewController: UITableViewController, UIAlertViewDelegate {
  var keyboardCount: Int = 0
  var keyboardIndex: Int = 0
  var keyboardID: String = ""
  var languageID: String = ""
  var keyboardVersion: String = ""
  var keyboardCopyright: String = ""
  var isCustomKeyboard: Bool = false

  private var infoArray = [[String: String]]()

  override func viewDidLoad() {
    super.viewDidLoad()

    infoArray = [[String: String]]()
    infoArray.append([
      "title": "Keyboard version",
      "subtitle": keyboardVersion
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
  }

  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    navigationController?.setToolbarHidden(true, animated: true)
  }

  override func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }

  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    if isCustomKeyboard {
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
    if !isCustomKeyboard {
      if indexPath.row == 1 {
        let url = URL(string: "http://help.keyman.com/keyboard/\(keyboardID)/\(keyboardVersion)/")!
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

  override func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
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
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: "Cancel",
                                            style: UIAlertActionStyle.cancel,
                                            handler: nil))
    alertController.addAction(UIAlertAction(title: "Delete",
                                            style: UIAlertActionStyle.default,
                                            handler: deleteHandler))
    
    self.present(alertController, animated: true, completion: nil)
  }

  func deleteHandler(withAction action: UIAlertAction) {
    if Manager.shared.removeKeyboard(at: keyboardIndex) {
        navigationController?.popToRootViewController(animated: true)
    }
  }
}
