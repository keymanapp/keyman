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
    let selKbID = kbDict[kKeymanKeyboardIdKey]
    let selLangID = kbDict[kKeymanLanguageIdKey]
    let isCurrentKB = isCurrentKeyboard(languageID: selLangID, keyboardID: selKbID)

    if Manager.shared.removeKeyboard(at: indexPath.row) {
      let userData = Manager.shared.activeUserDefaults()
      userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) as! [[String : String]]
      if isCurrentKB {
        let kbID = userKeyboards[0][kKeymanKeyboardIdKey]
        let langID = userKeyboards[0][kKeymanLanguageIdKey]
        let kbName = userKeyboards[0][kKeymanKeyboardNameKey]
        let langName = userKeyboards[0][kKeymanLanguageNameKey]
        let font = userKeyboards[0][kKeymanFontKey]
        let oskFont = userKeyboards[0][kKeymanOskFontKey]
        Manager.shared.setKeyboard(withID: kbID!, languageID: langID!, keyboardName: kbName,
                                   languageName: langName, font: font, oskFont: oskFont)
      }
      NotificationCenter.default.post(name: NSNotification.Name.keymanKeyboardRemoved,
        object: self, userInfo: [kKeymanKeyboardInfoKey: kbDict])
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
    let kbID  = userKeyboards[index][kKeymanKeyboardIdKey]!
    let langID = userKeyboards[index][kKeymanLanguageIdKey]!
    let kbName = userKeyboards[index][kKeymanKeyboardNameKey]
    let isCustom = userKeyboards[index][kKeymanCustomKeyboardKey] == "Y"
    let kbVersion = userKeyboards[index][kKeymanKeyboardVersionKey]
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
    let languageID = userKeyboards[indexPath.row][kKeymanLanguageIdKey]
    let keyboardID = userKeyboards[indexPath.row][kKeymanKeyboardIdKey]

    cell.textLabel?.text = userKeyboards[indexPath.row][kKeymanLanguageNameKey]
    cell.detailTextLabel?.text = userKeyboards[indexPath.row][kKeymanKeyboardNameKey]
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
      let downloadedKbInfo = notification.userInfo?[kKeymanKeyboardInfoKey] as? [String: String]
      if let kbID = downloadedKbInfo?[kKeymanKeyboardIdKey],
         let currentKbInfo = Manager.shared.keyboardsInfo?[kbID],
         let kbVersion = currentKbInfo[kKeymanKeyboardVersionKey] {
        Manager.shared.updateKeyboardVersion(forID: kbID, newKeyboardVersion: kbVersion)
      }

      updateQueue!.remove(at: 0)
      if !updateQueue!.isEmpty {
        let langID = updateQueue![0][kKeymanLanguageIdKey]!
        let kbID = updateQueue![0][kKeymanKeyboardIdKey]!
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

      let kbInfo = notification.userInfo?[kKeymanKeyboardInfoKey] as? [String: String]
      let langID = kbInfo![kKeymanLanguageIdKey]!
      let kbID  = kbInfo![kKeymanKeyboardIdKey]!
      let key  = "\(langID)_\(kbID)"
      let keyboardDict = Manager.shared.keyboardsDictionary[key]
      let langName  = keyboardDict![kKeymanLanguageNameKey]!
      let kbName  = keyboardDict![kKeymanKeyboardNameKey]!
      let isRTL  = keyboardDict?[kKeymanKeyboardRTLKey] == "Y"
      let font  = keyboardDict?[kKeymanFontKey]
      let oskFont = keyboardDict?[kKeymanOskFontKey]

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
    let langID = userKeyboards[index][kKeymanLanguageIdKey]!
    let kbID = userKeyboards[index][kKeymanKeyboardIdKey]!
    let langName = userKeyboards[index][kKeymanLanguageNameKey]
    let kbName = userKeyboards[index][kKeymanKeyboardNameKey]
    let font = userKeyboards[index][kKeymanFontKey]
    let oskFont = userKeyboards[index][kKeymanOskFontKey]

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

    if let userKeyboards = userData.array(forKey: kKeymanUserKeyboardsListKey) as? [[String: String]] {
      self.userKeyboards = userKeyboards
    } else {
      let kbVersion = Manager.shared.latestKeyboardFileVersion(withID: kKeymanDefaultKeyboardID)
      userKeyboards = [[
        kKeymanKeyboardIdKey: kKeymanDefaultKeyboardID,
        kKeymanLanguageIdKey: kKeymanDefaultLanguageID,
        kKeymanKeyboardNameKey: kKeymanDefaultKeyboardName,
        kKeymanLanguageNameKey: kKeymanDefaultLanguageName,
        kKeymanKeyboardVersionKey: kbVersion ?? "nil",
        kKeymanKeyboardRTLKey: kKeymanDefaultKeyboardRTL,
        kKeymanFontKey: kKeymanDefaultKeyboardFont
      ]]

      userData.set(userKeyboards, forKey: kKeymanUserKeyboardsListKey)
      userData.synchronize()
    }

    tableView.reloadData()
  }

  private func isAdded(languageID langID: String, keyboardID kbID: String) -> Bool {
    return userKeyboards.contains { keyboard in
      let languageID = keyboard[kKeymanLanguageIdKey]
      let keyboardID = keyboard[kKeymanKeyboardIdKey]
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
      let kbID = keyboard[kKeymanKeyboardIdKey]!
      return Manager.shared.stateForKeyboard(withID: kbID) == .needsUpdate
    }
  }

  private func updateKeyboards() {
    updateQueue = [[String: String]]()
    var kbIDs = Set<String>()
    for kb in userKeyboards {
      guard let kbID = kb[kKeymanKeyboardIdKey] else {
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
      let langID = updateQueue![0][kKeymanLanguageIdKey]!
      let kbID = updateQueue![0][kKeymanKeyboardIdKey]!
      Manager.shared.downloadKeyboard(withID: kbID, languageID: langID, isUpdate: true)
    }
  }

  private func scroll(toSelectedKeyboard animated: Bool) {
    let index = userKeyboards.index { kb in
      let langID = kb[kKeymanLanguageIdKey]
      let kbID = kb[kKeymanKeyboardIdKey]
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
