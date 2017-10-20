//
//  KeyboardPickerViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-12.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

private let errorAlertTag = -1
private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

public class KeyboardPickerViewController: UITableViewController, UIAlertViewDelegate {
  private var userKeyboards: [[String: String]] = []
  private var updateQueue: [[String: String]]?
  private var _isDoneButtonEnabled = false
  private var isDidUpdateCheck = false

  public override func viewDidLoad() {
    super.viewDidLoad()

    title = "Keyboards"
    setIsDoneButtonEnabled(false)
    isDidUpdateCheck = false
    updateQueue = nil
    if Manager.shared.canAddNewKeyboards {
      let addButton = UIBarButtonItem(barButtonSystemItem: .add, target: self,
                                      action: #selector(self.addClicked))
      navigationItem.rightBarButtonItem = addButton
    }

    navigationController?.toolbar?.barTintColor = UIColor(red: 0.5, green: 0.75,
                                                          blue: 0.25, alpha: 0.9)

    NotificationCenter.default.addObserver(self,
      selector: #selector(self.keyboardDownloadStarted),
      name: NSNotification.Name.keymanKeyboardDownloadStarted,
      object: nil)
    NotificationCenter.default.addObserver(self,
      selector: #selector(self.keyboardDownloadFinished),
      name: NSNotification.Name.keymanKeyboardDownloadCompleted,
      object: nil)
    NotificationCenter.default.addObserver(self,
      selector: #selector(self.keyboardDownloadFailed),
      name: NSNotification.Name.keymanKeyboardDownloadFailed,
      object: nil)
    NotificationCenter.default.addObserver(self,
      selector: #selector(self.keyboardChanged),
      name: NSNotification.Name.keymanKeyboardChanged,
      object: nil)
  }

  deinit {
    NotificationCenter.default.removeObserver(self)
  }

  public override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)

    loadUserKeyboards()
    scroll(toSelectedKeyboard: false)
  }

  public override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    if isDidUpdateCheck || !checkUpdates() {
      return
    }

    let toolbarFrame = navigationController!.toolbar!.frame
    let button = UIButton(type: .roundedRect)
    button.addTarget(self, action: #selector(self.updateClicked), for: .touchUpInside)

    button.frame = CGRect(x: toolbarFrame.origin.x, y: toolbarFrame.origin.y,
                          width: toolbarFrame.width * 0.95, height: toolbarFrame.height * 0.7)
    button.center = CGPoint(x: toolbarFrame.width / 2, y: toolbarFrame.height / 2)
    button.tintColor = UIColor(red: 0.75, green: 1.0, blue: 0.5, alpha: 1.0)
    button.setTitleColor(UIColor.white, for: .normal)
    button.setTitle("Update available", for: .normal)
    button.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                               .flexibleBottomMargin, .flexibleWidth, .flexibleHeight]
    button.tag = toolbarButtonTag
    navigationController?.toolbar?.addSubview(button)

    navigationController?.setToolbarHidden(false, animated: true)
    scroll(toSelectedKeyboard: false)
  }

  public override func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }

  public override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return userKeyboards.count
  }

  public override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      return cell
    }

    let cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    let selectionColor = UIView()
    selectionColor.backgroundColor = UIColor(red: 204.0 / 255.0, green: 136.0 / 255.0, blue: 34.0 / 255.0, alpha: 1.0)
    cell.selectedBackgroundView = selectionColor
    return cell
  }

  // TODO: Refactor. Duplicated in KeyboardInfoViewController
  public override func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
    if !Manager.shared.canRemoveKeyboards {
      return false
    }

    if !Manager.shared.canRemoveDefaultKeyboard {
      return indexPath.row != 0
    }
    if indexPath.row > 0 {
      return true
    }
    return userKeyboards.count > 1
  }

  public override func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCellEditingStyle,
                                 forRowAt indexPath: IndexPath) {
    if editingStyle != .delete {
      return
    }

    let kbDict = userKeyboards[indexPath.row]
    let selKbID = kbDict[Key.keyboardId]
    let selLangID = kbDict[Key.languageId]
    let isCurrentKB = isCurrentKeyboard(languageID: selLangID, keyboardID: selKbID)

    if Manager.shared.removeKeyboard(at: indexPath.row) {
      let userData = Manager.shared.activeUserDefaults()
      userKeyboards = userData.array(forKey: Key.userKeyboardsList) as! [[String : String]]
      if isCurrentKB {
        let kbID = userKeyboards[0][Key.keyboardId]
        let langID = userKeyboards[0][Key.languageId]
        let kbName = userKeyboards[0][Key.keyboardName]
        let langName = userKeyboards[0][Key.languageName]
        let font = userKeyboards[0][Key.font]
        let oskFont = userKeyboards[0][Key.oskFont]
        Manager.shared.setKeyboard(withID: kbID!, languageID: langID!, keyboardName: kbName,
                                   languageName: langName, font: font, oskFont: oskFont)
      }
      NotificationCenter.default.post(name: NSNotification.Name.keymanKeyboardRemoved,
        object: self, userInfo: [Key.keyboardInfo: kbDict])
      self.tableView.reloadData()
    }
    setIsDoneButtonEnabled(true)
  }

  public override func tableView(_ tableView: UITableView,
                                 accessoryButtonTappedForRowWith indexPath: IndexPath) {
    showKeyboardInfoView(with: indexPath.row)
  }

  func showKeyboardInfoView(with index: Int) {
    setIsDoneButtonEnabled(true)
    let kbID  = userKeyboards[index][Key.keyboardId]!
    let langID = userKeyboards[index][Key.languageId]!
    let kbName = userKeyboards[index][Key.keyboardName]
    let isCustom = userKeyboards[index][Key.customKeyboard] == "Y"
    let kbVersion = userKeyboards[index][Key.keyboardVersion]
      ?? Manager.shared.latestKeyboardFileVersion(withID: kbID)
      ?? "1.0"

    let infoView = KeyboardInfoViewController()
    infoView.title = kbName
    infoView.keyboardCount = userKeyboards.count
    infoView.keyboardIndex = index
    infoView.keyboardID = kbID
    infoView.languageID = langID
    infoView.keyboardVersion = kbVersion
    infoView.isCustomKeyboard = isCustom
    navigationController?.pushViewController(infoView, animated: true)
  }

  public override func tableView(_ tableView: UITableView,
                                 willDisplay cell: UITableViewCell,
                                 forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    let languageID = userKeyboards[indexPath.row][Key.languageId]
    let keyboardID = userKeyboards[indexPath.row][Key.keyboardId]

    cell.textLabel?.text = userKeyboards[indexPath.row][Key.languageName]
    cell.detailTextLabel?.text = userKeyboards[indexPath.row][Key.keyboardName]
    cell.tag = indexPath.row

    if isCurrentKeyboard(languageID: languageID, keyboardID: keyboardID) {
      cell.selectionStyle = .blue
      cell.isSelected = true
      cell.accessoryType = .detailDisclosureButton
    } else {
      cell.selectionStyle = .none
      cell.isSelected = false
      cell.accessoryType = .detailDisclosureButton
    }
  }

  public override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    switchKeyboard(indexPath.row)
  }

  @objc func keyboardDownloadStarted(_ notification: Notification) {
    view.isUserInteractionEnabled = false
    navigationItem.leftBarButtonItem?.isEnabled = false
    navigationItem.rightBarButtonItem?.isEnabled = false
  }

  @objc func keyboardDownloadFinished(_ notification: Notification) {
    if view == navigationController?.topViewController?.view {
      if updateQueue == nil {
        return
      }
      Manager.shared.shouldReloadKeyboard = true

      // Update keyboard version
      let downloadedKbInfo = notification.userInfo?[Key.keyboardInfo] as? [String: String]
      if let kbID = downloadedKbInfo?[Key.keyboardId],
         let currentKbInfo = Manager.shared.keyboardsInfo?[kbID],
         let kbVersion = currentKbInfo[Key.keyboardVersion] {
        Manager.shared.updateKeyboardVersion(forID: kbID, newKeyboardVersion: kbVersion)
      }

      updateQueue!.remove(at: 0)
      if !updateQueue!.isEmpty {
        let langID = updateQueue![0][Key.languageId]!
        let kbID = updateQueue![0][Key.keyboardId]!
        Manager.shared.downloadKeyboard(withID: kbID, languageID: langID, isUpdate: true)
      } else {
        loadUserKeyboards()
        view.isUserInteractionEnabled = true
        navigationItem.leftBarButtonItem?.isEnabled = true
        if navigationItem.rightBarButtonItem != nil {
          navigationItem.rightBarButtonItem?.isEnabled = true
        }
        updateQueue = nil
        let label = navigationController?.toolbar?.viewWithTag(toolbarLabelTag) as? UILabel
        label?.text = "Keyboards successfully updated!"
        navigationController?.toolbar?.viewWithTag(toolbarActivityIndicatorTag)?.removeFromSuperview()
        Timer.scheduledTimer(timeInterval: 3.0, target: self, selector: #selector(self.hideToolbarDelayed),
                             userInfo: nil, repeats: false)
      }
    } else {
      let label = navigationController?.toolbar?.viewWithTag(toolbarLabelTag) as? UILabel
      label?.text = "Keyboard successfully downloaded!"
      navigationController?.toolbar?.viewWithTag(toolbarActivityIndicatorTag)?.removeFromSuperview()
      Timer.scheduledTimer(timeInterval: 3.0, target: self, selector: #selector(self.hideToolbarDelayed),
                           userInfo: nil, repeats: false)

      view.isUserInteractionEnabled = true
      navigationItem.leftBarButtonItem?.isEnabled = true
      if navigationItem.rightBarButtonItem != nil {
        navigationItem.rightBarButtonItem?.isEnabled = true
      }

      let kbInfo = notification.userInfo?[Key.keyboardInfo] as? [String: String]
      let langID = kbInfo![Key.languageId]!
      let kbID  = kbInfo![Key.keyboardId]!
      let key  = "\(langID)_\(kbID)"
      let keyboardDict = Manager.shared.keyboardsDictionary[key]
      let langName  = keyboardDict![Key.languageName]!
      let kbName  = keyboardDict![Key.keyboardName]!
      let isRTL  = keyboardDict?[Key.keyboardRTL] == "Y"
      let font  = keyboardDict?[Key.font]
      let oskFont = keyboardDict?[Key.oskFont]

      // Add keyboard.
      Manager.shared.addKeyboard(withID: kbID, languageID: langID, keyboardName: kbName,
                                 languageName: langName, isRTL: isRTL, isCustom: false, font: font,
                                 oskFont: oskFont)
      Manager.shared.setKeyboard(withID: kbID, languageID: langID, keyboardName: kbName,
                                 languageName: langName, font: font, oskFont: oskFont)
      navigationController?.popToRootViewController(animated: true)
    }
  }

  @objc func keyboardDownloadFailed(_ notification: Notification) {
    view.isUserInteractionEnabled = true
    navigationItem.leftBarButtonItem?.isEnabled = true
    if let item = navigationItem.rightBarButtonItem {
      item.isEnabled = true
    }

    let title: String
    if view == navigationController?.topViewController?.view {
      updateQueue = nil
      title = "Keyboard Update Error"
    } else {
      title = "Keyboard Download Error"
    }
    navigationController?.setToolbarHidden(true, animated: true)

    let error = notification.userInfo?[NSUnderlyingErrorKey] as? Error
    let alert = UIAlertView(title: title, message: error?.localizedDescription ?? "",
                            delegate: self, cancelButtonTitle: "OK", otherButtonTitles: "")
    alert.tag = errorAlertTag
    alert.show()
  }

  @objc func keyboardChanged(_ notification: Notification) {
  }

  private func switchKeyboard(_ index: Int) {
    // Switch keyboard and register to user defaults.
    let langID = userKeyboards[index][Key.languageId]!
    let kbID = userKeyboards[index][Key.keyboardId]!
    let langName = userKeyboards[index][Key.languageName]
    let kbName = userKeyboards[index][Key.keyboardName]
    let font = userKeyboards[index][Key.font]
    let oskFont = userKeyboards[index][Key.oskFont]

    if Manager.shared.setKeyboard(withID: kbID, languageID: langID, keyboardName: kbName,
                                  languageName: langName, font: font, oskFont: oskFont) {
      tableView.reloadData()
    }

    if !_isDoneButtonEnabled {
      Manager.shared.dismissKeyboardPicker(self)
    }
  }

  private func loadUserKeyboards() {
    let userData = Manager.shared.activeUserDefaults()

    if let userKeyboards = userData.array(forKey: Key.userKeyboardsList) as? [[String: String]] {
      self.userKeyboards = userKeyboards
    } else {
      let kbVersion = Manager.shared.latestKeyboardFileVersion(withID: DefaultKeyboard.keyboardID)
      userKeyboards = [[
        Key.keyboardId: DefaultKeyboard.keyboardID,
        Key.languageId: DefaultKeyboard.languageID,
        Key.keyboardName: DefaultKeyboard.keyboardName,
        Key.languageName: DefaultKeyboard.languageName,
        Key.keyboardVersion: kbVersion ?? "nil",
        Key.keyboardRTL: DefaultKeyboard.keyboardRTL,
        Key.font: DefaultKeyboard.keyboardFont
      ]]

      userData.set(userKeyboards, forKey: Key.userKeyboardsList)
      userData.synchronize()
    }

    tableView.reloadData()
  }

  private func isAdded(languageID langID: String, keyboardID kbID: String) -> Bool {
    return userKeyboards.contains { keyboard in
      let languageID = keyboard[Key.languageId]
      let keyboardID = keyboard[Key.keyboardId]
      return languageID == langID && keyboardID == kbID
    }
  }

  @objc func doneClicked(_ sender: Any) {
    Manager.shared.dismissKeyboardPicker(self)
  }

  @objc func cancelClicked(_ sender: Any) {
    Manager.shared.dismissKeyboardPicker(self)
  }

  @objc func addClicked(_ sender: Any) {
    showAddKeyboard()
  }

  @objc func updateClicked(_ sender: Any) {
    navigationController?.toolbar?.viewWithTag(toolbarButtonTag)?.removeFromSuperview()
    let toolbarFrame = navigationController!.toolbar!.frame
    let width = toolbarFrame.width * 0.95
    let height = toolbarFrame.height * 0.7
    let labelFrame = CGRect(x: toolbarFrame.origin.x, y: toolbarFrame.origin.y,
                            width: width, height: height)

    let label = UILabel(frame: labelFrame)
    label.backgroundColor = UIColor.clear
    label.textColor = UIColor.white
    label.textAlignment = .center
    label.center = CGPoint(x: width * 0.5, y: height * 0.5)
    label.text = "Updating\u{2026}"
    label.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                              .flexibleBottomMargin, .flexibleWidth, .flexibleHeight]
    label.tag = toolbarLabelTag

    let indicatorView = UIActivityIndicatorView(activityIndicatorStyle: .gray)
    indicatorView.center = CGPoint(x: width - indicatorView.frame.width, y: height * 0.5)
    indicatorView.autoresizingMask = [.flexibleLeftMargin, .flexibleTopMargin, .flexibleBottomMargin]
    indicatorView.tag = toolbarActivityIndicatorTag
    indicatorView.startAnimating()
    navigationController?.toolbar?.addSubview(label)
    navigationController?.toolbar?.addSubview(indicatorView)
    setIsDoneButtonEnabled(true)
    updateKeyboards()
  }

  private func checkUpdates() -> Bool {
    if Manager.shared.keyboardsInfo == nil {
      return false
    }

    isDidUpdateCheck = true
    return userKeyboards.contains { keyboard in
      let kbID = keyboard[Key.keyboardId]!
      return Manager.shared.stateForKeyboard(withID: kbID) == .needsUpdate
    }
  }

  private func updateKeyboards() {
    updateQueue = [[String: String]]()
    var kbIDs = Set<String>()
    for kb in userKeyboards {
      guard let kbID = kb[Key.keyboardId] else {
        continue
      }
      let kbState = Manager.shared.stateForKeyboard(withID: kbID)
      if kbState == .needsUpdate {
        if !kbIDs.contains(kbID) {
          kbIDs.insert(kbID)
          updateQueue!.append(kb)
        }
      }
    }

    if !updateQueue!.isEmpty {
      let langID = updateQueue![0][Key.languageId]!
      let kbID = updateQueue![0][Key.keyboardId]!
      Manager.shared.downloadKeyboard(withID: kbID, languageID: langID, isUpdate: true)
    }
  }

  private func scroll(toSelectedKeyboard animated: Bool) {
    let index = userKeyboards.index { kb in
      let langID = kb[Key.languageId]
      let kbID = kb[Key.keyboardId]
      return isCurrentKeyboard(languageID: langID, keyboardID: kbID)
    }

    if let index = index {
      let indexPath = IndexPath(row: index, section: 0)
      tableView.scrollToRow(at: indexPath, at: .middle, animated: animated)

    }
  }

  private func setIsDoneButtonEnabled(_ value: Bool) {
    _isDoneButtonEnabled = value
    if _isDoneButtonEnabled {
      let doneButton = UIBarButtonItem(title: "Done", style: .plain, target: self,
                                       action: #selector(self.doneClicked))
      navigationItem.leftBarButtonItem = doneButton
    } else {
      let cancelButton = UIBarButtonItem(barButtonSystemItem: .cancel, target: self,
                                         action: #selector(self.cancelClicked))
      navigationItem.leftBarButtonItem = cancelButton
    }
  }

  private func isCurrentKeyboard(languageID: String?, keyboardID: String?) -> Bool {
    return Manager.shared.keyboardID == keyboardID &&
      Manager.shared.languageID == languageID
  }

  @objc func hideToolbarDelayed(_ timer: Timer) {
    navigationController?.setToolbarHidden(true, animated: true)
  }

  @objc public func showAddKeyboard() {
    let button: UIButton? = (navigationController?.toolbar?.viewWithTag(toolbarButtonTag) as? UIButton)
    button?.isEnabled = false
    let vc = LanguageViewController()
    navigationController?.pushViewController(vc, animated: true)
    setIsDoneButtonEnabled(true)
  }
}
