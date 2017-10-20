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
      "title": "Delete",
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
        let url = URL(string:"http://help.keyman.com/keyboard/\(keyboardID)/\(keyboardVersion)/")!
        if let openURL = Manager.shared.openURL {
          openURL(url)
        } else {
          Manager.shared.kmLog("openURL not set in Manager. Failed to open \(url)", checkDebugPrinting: false)
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
    let keyboards = json[kKeymanLanguageKey] as? [Any]
    let kbDict = keyboards?[0] as? [AnyHashable: Any]
    var info = infoArray[1]
    let copyright = kbDict?[kKeymanKeyboardCopyrightKey] as? String ?? "Unknown"

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

  private var isCurrentKeyboard: Bool {
    return Manager.shared.keyboardID == keyboardID &&
      Manager.shared.languageID == languageID
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
    let alert = UIAlertView(title: title ?? "",
                            message: "Would you like to delete this keyboard?",
                            delegate: self,
                            cancelButtonTitle: "Cancel",
                            otherButtonTitles: "Delete")
    alert.tag = 1
    alert.show()
  }

  func alertView(_ alertView: UIAlertView, clickedButtonAt buttonIndex: Int) {
    if buttonIndex == alertView.cancelButtonIndex {
      return
    }

    if alertView.tag == 1 {
      let userData = Manager.shared.activeUserDefaults()
      let userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) as! [[String: String]]
      let kbDict = userKeyboards[keyboardIndex]

      if Manager.shared.removeKeyboard(at: keyboardIndex) {
        if isCurrentKeyboard {
          // Select default keyboard

          let kbID = userKeyboards[0][kKeymanKeyboardIdKey]!
          let langID = userKeyboards[0][kKeymanLanguageIdKey]!
          let kbName = userKeyboards[0][kKeymanKeyboardNameKey]
          let langName = userKeyboards[0][kKeymanLanguageNameKey]
          let font = userKeyboards[0][kKeymanFontKey]
          let oskFont = userKeyboards[0][kKeymanOskFontKey]
          Manager.shared.setKeyboard(withID: kbID, languageID: langID, keyboardName: kbName,
                                     languageName: langName, font: font, oskFont: oskFont)
        }
        NotificationCenter.default.post(name: NSNotification.Name.keymanKeyboardRemoved,
                                        object: self, userInfo: [ kKeymanKeyboardInfoKey: kbDict ]
        )
        navigationController?.popToRootViewController(animated: true)
      }
    }
  }
}
