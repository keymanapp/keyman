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

class KeyboardPickerViewController: UITableViewController, UIAlertViewDelegate {
  private var userKeyboards: [InstallableKeyboard] = []
  private var updateQueue: [InstallableKeyboard]?
  private var _isDoneButtonEnabled = false
  private var isDidUpdateCheck = false

  private var keyboardDownloadStartedObserver: NotificationObserver?
  private var keyboardDownloadCompletedObserver: NotificationObserver?
  private var keyboardDownloadFailedObserver: NotificationObserver?

  override func viewDidLoad() {
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

    keyboardDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadStarted,
      observer: self,
      function: KeyboardPickerViewController.keyboardDownloadStarted)
    keyboardDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadCompleted,
      observer: self,
      function: KeyboardPickerViewController.keyboardDownloadCompleted)
    keyboardDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadFailed,
      observer: self,
      function: KeyboardPickerViewController.keyboardDownloadFailed)
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)

    loadUserKeyboards()
    scroll(toSelectedKeyboard: false)
  }

  override func viewDidAppear(_ animated: Bool) {
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

  override func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }

  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return userKeyboards.count
  }

  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
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
  override func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
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

  override func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCellEditingStyle,
                          forRowAt indexPath: IndexPath) {
    if editingStyle != .delete {
      return
    }

    if Manager.shared.removeKeyboard(at: indexPath.row) {
      let kb = userKeyboards[indexPath.row]
      if isCurrentKeyboard(languageID: kb.id, keyboardID: kb.languageID) {
        let userData = Manager.shared.activeUserDefaults()
        userKeyboards = userData.userKeyboards!
        _ = Manager.shared.setKeyboard(userKeyboards[0])
      }
      self.tableView.reloadData()
    }
    setIsDoneButtonEnabled(true)
  }

  override func tableView(_ tableView: UITableView,
                          accessoryButtonTappedForRowWith indexPath: IndexPath) {
    showKeyboardInfoView(with: indexPath.row)
  }

  func showKeyboardInfoView(with index: Int) {
    setIsDoneButtonEnabled(true)
    let kb = userKeyboards[index]
    let version = kb.version

    let infoView = KeyboardInfoViewController()
    infoView.title = kb.name
    infoView.keyboardCount = userKeyboards.count
    infoView.keyboardIndex = index
    infoView.keyboardID = kb.id
    infoView.languageID = kb.languageID
    infoView.keyboardVersion = version
    infoView.isCustomKeyboard = kb.isCustom
    navigationController?.pushViewController(infoView, animated: true)
  }

  override func tableView(_ tableView: UITableView,
                          willDisplay cell: UITableViewCell,
                          forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    let kb = userKeyboards[indexPath.row]

    cell.textLabel?.text = kb.languageName
    cell.detailTextLabel?.text = kb.name
    cell.tag = indexPath.row

    if isCurrentKeyboard(languageID: kb.languageID, keyboardID: kb.id) {
      cell.selectionStyle = .blue
      cell.isSelected = true
      cell.accessoryType = .detailDisclosureButton
    } else {
      cell.selectionStyle = .none
      cell.isSelected = false
      cell.accessoryType = .detailDisclosureButton
    }
  }

  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    switchKeyboard(indexPath.row)
  }

  private func keyboardDownloadStarted(_ keyboards: [InstallableKeyboard]) {
    view.isUserInteractionEnabled = false
    navigationItem.leftBarButtonItem?.isEnabled = false
    navigationItem.rightBarButtonItem?.isEnabled = false
  }

  private func keyboardDownloadCompleted(_ keyboards: [InstallableKeyboard]) {
    if view == navigationController?.topViewController?.view {
      if updateQueue == nil {
        return
      }
      Manager.shared.shouldReloadKeyboard = true

      // Update keyboard version
      for keyboard in keyboards {
        if let currentKbInfo = Manager.shared.keyboardsInfo?[keyboard.id] {
          Manager.shared.updateKeyboardVersion(forID: keyboard.id, newKeyboardVersion: currentKbInfo.version)
        }
      }

      updateQueue!.remove(at: 0)
      if !updateQueue!.isEmpty {
        let langID = updateQueue![0].languageID
        let kbID = updateQueue![0].id
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

      // Add keyboard.
      for keyboard in keyboards {
        Manager.shared.addKeyboard(keyboard)
        _ = Manager.shared.setKeyboard(keyboard)
      }

      navigationController?.popToRootViewController(animated: true)
    }
  }

  private func keyboardDownloadFailed(_ notification: KeyboardDownloadFailedNotification) {
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

    let alert = UIAlertView(title: title, message: notification.error.localizedDescription,
                            delegate: self, cancelButtonTitle: "OK", otherButtonTitles: "")
    alert.tag = errorAlertTag
    alert.show()
  }

  private func switchKeyboard(_ index: Int) {
    // Switch keyboard and register to user defaults.
    if Manager.shared.setKeyboard(userKeyboards[index]) {
      tableView.reloadData()
    }

    if !_isDoneButtonEnabled {
      Manager.shared.dismissKeyboardPicker(self)
    }
  }

  private func loadUserKeyboards() {
    let userData = Manager.shared.activeUserDefaults()

    if let userKeyboards = userData.userKeyboards {
      self.userKeyboards = userKeyboards
    } else {
      userKeyboards = [Constants.defaultKeyboard]
      userData.userKeyboards = userKeyboards
      userData.synchronize()
    }

    tableView.reloadData()
  }

  private func isAdded(languageID langID: String, keyboardID kbID: String) -> Bool {
    return userKeyboards.contains { kb in kb.languageID == langID && kb.id == kbID }
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
      let kbID = keyboard.id
      return Manager.shared.stateForKeyboard(withID: kbID) == .needsUpdate
    }
  }

  private func updateKeyboards() {
    updateQueue = []
    var kbIDs = Set<String>()
    for kb in userKeyboards {
      let kbState = Manager.shared.stateForKeyboard(withID: kb.id)
      if kbState == .needsUpdate {
        if !kbIDs.contains(kb.id) {
          kbIDs.insert(kb.id)
          updateQueue!.append(kb)
        }
      }
    }

    if !updateQueue!.isEmpty {
      let langID = updateQueue![0].languageID
      let kbID = updateQueue![0].id
      Manager.shared.downloadKeyboard(withID: kbID, languageID: langID, isUpdate: true)
    }
  }

  private func scroll(toSelectedKeyboard animated: Bool) {
    let index = userKeyboards.index { kb in
      return isCurrentKeyboard(languageID: kb.languageID, keyboardID: kb.id)
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

  func showAddKeyboard() {
    let button: UIButton? = (navigationController?.toolbar?.viewWithTag(toolbarButtonTag) as? UIButton)
    button?.isEnabled = false
    let vc = LanguageViewController()
    navigationController?.pushViewController(vc, animated: true)
    setIsDoneButtonEnabled(true)
  }
}
