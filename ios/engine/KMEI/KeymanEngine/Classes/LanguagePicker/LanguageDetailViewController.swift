//
//  LanguageDetailViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-14.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

class LanguageDetailViewController: UITableViewController, UIAlertViewDelegate {
  var languageIndex = 0
  var languageName = ""
  var languageID = ""
  var keyboards: [[String: Any]] = []
  private var userKeyboards: [String: [String: Any]] = [:]
  private var isUpdate = false

  override func loadView() {
    super.loadView()
    loadUserKeyboards()
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardDownloadStarted),
                                           name: NSNotification.Name.keymanKeyboardDownloadStarted, object: nil)
    NotificationCenter.default.addObserver(self, selector: #selector(self.keyboardDownloadFailed),
                                           name: NSNotification.Name.keymanKeyboardDownloadFailed, object: nil)
  }

  deinit {
    NotificationCenter.default.removeObserver(self)
  }

  override func numberOfSections(in tableView: UITableView) -> Int {
    return keyboards.count
  }

  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return 1
  }

  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      return cell
    }

    let cell = KeyboardNameTableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    let selectionColor = UIView()
    selectionColor.backgroundColor = UIColor(red: 204.0 / 255.0, green: 136.0 / 255.0, blue: 34.0 / 255.0, alpha: 1.0)
    cell.selectedBackgroundView = selectionColor
    return cell
  }

  override func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    let cell = cell as! KeyboardNameTableViewCell
    cell.indexPath = indexPath
    cell.textLabel?.text = (keyboards[indexPath.section][kKeymanNameKey] as! String)
    let keyboardID = keyboards[indexPath.section][kKeymanIdKey] as! String
    if isAdded(languageID: languageID, keyboardID: keyboardID) {
      cell.accessoryType = .checkmark
      cell.isUserInteractionEnabled = false
      cell.textLabel?.isEnabled = false
      cell.detailTextLabel?.isEnabled = false
    } else {
      cell.accessoryType = .none
      cell.isUserInteractionEnabled = true
      cell.textLabel?.isEnabled = true
      cell.detailTextLabel?.isEnabled = true
    }

    let kbState = KMManager.sharedInstance().stateForKeyboard(withID: keyboardID)
    cell.setKeyboardState(kbState, selected: false, defaultAccessoryType: cell.accessoryType)
  }

  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    tableView.cellForRow(at: indexPath)?.isSelected = false
    let kbID = keyboards[indexPath.section][kKeymanIdKey] as! String
    let kbName = keyboards[indexPath.section][kKeymanNameKey]

    let state = KMManager.sharedInstance().stateForKeyboard(withID: kbID)
    if state != kKMKeyboardStateDownloading {
      if state == kKMKeyboardStateNeedsDownload {
        isUpdate = false
      } else {
        isUpdate = true
      }

      let alert = UIAlertView(title: "\(languageName): \(kbName!)",
                              message: "Would you like to download this keyboard?",
                              delegate: self,
                              cancelButtonTitle: "Cancel",
                              otherButtonTitles: "Download")
      alert.tag = indexPath.section
      alert.show()
    }
  }

  func alertView(_ alertView: UIAlertView, clickedButtonAt buttonIndex: Int) {
    // Keyboard download confirmation alert (tag is used for keyboard index).
    if buttonIndex != alertView.cancelButtonIndex {
      KMManager.sharedInstance().downloadKeyboard(forLanguageIndex: languageIndex,
                                                  keyboardIndex: alertView.tag, isUpdate: isUpdate)
    }
  }

  @objc func keyboardDownloadStarted(_ notification: Notification) {
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)

    let toolbarFrame = navigationController!.toolbar.frame
    let labelFrame = CGRect(origin: toolbarFrame.origin,
                            size: CGSize(width: toolbarFrame.width * 0.95, height: toolbarFrame.height * 0.7))
    let label = UILabel(frame: labelFrame)
    label.backgroundColor = UIColor.clear
    label.textColor = UIColor.white
    label.textAlignment = .center
    label.center = CGPoint(x: toolbarFrame.width * 0.5, y: toolbarFrame.height * 0.5)
    label.text = "Downloading\u{2026}"
    label.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                              .flexibleBottomMargin, .flexibleWidth, .flexibleHeight]
    label.tag = toolbarLabelTag

    let indicatorView = UIActivityIndicatorView(activityIndicatorStyle: .gray)
    indicatorView.center = CGPoint(x: toolbarFrame.width - indicatorView.frame.width,
                                   y: toolbarFrame.height * 0.5)
    indicatorView.autoresizingMask = [.flexibleLeftMargin, .flexibleTopMargin, .flexibleBottomMargin]
    indicatorView.tag = toolbarActivityIndicatorTag
    indicatorView.startAnimating()

    navigationController?.toolbar.viewWithTag(toolbarButtonTag)?.removeFromSuperview()
    navigationController?.toolbar.viewWithTag(toolbarLabelTag)?.removeFromSuperview()
    navigationController?.toolbar.viewWithTag(toolbarActivityIndicatorTag)?.removeFromSuperview()
    navigationController?.toolbar.addSubview(label)
    navigationController?.toolbar.addSubview(indicatorView)
    navigationController?.setToolbarHidden(false, animated: true)
  }

  @objc func keyboardDownloadFailed(_ notification: Notification) {
    view.isUserInteractionEnabled = true
    navigationItem.setHidesBackButton(false, animated: true)
  }

  private func loadUserKeyboards() {
    let userData = KMManager.sharedInstance().activeUserDefaults()
    guard let userKbList = userData?.array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]],
      !userKbList.isEmpty else {
      userKeyboards = [:]
      return
    }

    userKeyboards = [:]
    for kb in userKbList {
      let langID = kb[kKeymanLanguageIdKey]
      let kbID = kb[kKeymanKeyboardIdKey]
      let dictKey = "\(langID!)_\(kbID!)"
      userKeyboards[dictKey] = kb
    }
  }

  private func isAdded(languageID: String?, keyboardID: String?) -> Bool {
    guard let languageID = languageID, let keyboardID = keyboardID else {
      return false
    }
    return userKeyboards["\(languageID)_\(keyboardID)"] != nil
  }
}
