//
//  KeyboardPickerViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-12.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

class KeyboardPickerViewController: KeyboardSwitcherViewController {
  private var updateQueue: [InstallableKeyboard]?
  private var _isDoneButtonEnabled = false
  private var isDidUpdateCheck = false

  private var keyboardDownloadStartedObserver: NotificationObserver?
  private var keyboardDownloadCompletedObserver: NotificationObserver?
  private var keyboardDownloadFailedObserver: NotificationObserver?
  private var lexicalModelDownloadStartedObserver: NotificationObserver?
  private var lexicalModelDownloadCompletedObserver: NotificationObserver?
  private var lexicalModelDownloadFailedObserver: NotificationObserver?
  
  override func viewDidLoad() {
    super.viewDidLoad()

    self.accessoryType = .detailDisclosureButton

    // adding keyboards is unique to this subclass, so is updating them
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
    
    lexicalModelDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadStarted,
      observer: self,
      function: KeyboardPickerViewController.lexicalModelDownloadStarted)
    lexicalModelDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadCompleted,
      observer: self,
      function: KeyboardPickerViewController.lexicalModelDownloadCompleted)
    lexicalModelDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadFailed,
      observer: self,
      function: KeyboardPickerViewController.lexicalModelDownloadFailed)
    
    log.info("didLoad: KeyboardPickerViewController (registered lexicalModelDownloadCompleted et al)")
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
    log.info("didAppear: KeyboardPickerViewController")
  }
  
  override func switchKeyboard(_ index: Int) {
    // Switch keyboard and register to user defaults.
    if Manager.shared.setKeyboard(userKeyboards[index]) {
      tableView.reloadData()
    }
    
    if !_isDoneButtonEnabled {
      Manager.shared.dismissKeyboardPicker(self)
    }
  }
  
  // MARK: - table view delegate UITableViewDelegate

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
  
  // MARK: - keyboard and lexical model downloading

  private func keyboardDownloadStarted(_ keyboards: [InstallableKeyboard]) {
    log.info("keyboardDownloadStarted")
    view.isUserInteractionEnabled = false
    navigationItem.leftBarButtonItem?.isEnabled = false
    navigationItem.rightBarButtonItem?.isEnabled = false
  }
    
  private func lexicalModelDownloadStarted(_ lexicalModels: [InstallableLexicalModel]) {
    log.info("lexicalModelDownloadStarted")
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
        Manager.shared.updateUserKeyboards(with: keyboard)
      }

      updateQueue!.remove(at: 0)
      if !updateQueue!.isEmpty {
        let langID = updateQueue![0].languageID
        let kbID = updateQueue![0].id
        Manager.shared.downloadKeyboard(withID: kbID, languageID: langID, isUpdate: true)
      } else {
        loadUserKeyboards()
        restoreNavigation()

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

      restoreNavigation()

      // Add keyboard.
      for keyboard in keyboards {
        Manager.shared.addKeyboard(keyboard)
        _ = Manager.shared.setKeyboard(keyboard)
      }

      navigationController?.popToRootViewController(animated: true)
    }
    log.info("keyboardDownloadCompleted KeyboardPicker")
  }
  
  private func restoreNavigation() {
    view.isUserInteractionEnabled = true
    navigationItem.leftBarButtonItem?.isEnabled = true
    if navigationItem.rightBarButtonItem != nil {
      navigationItem.rightBarButtonItem?.isEnabled = true
    }
  }
  
  private func lexicalModelDownloadCompleted(_ lexicalModels: [InstallableLexicalModel]) {
    log.info("lexicalModelDownloadCompleted KeyboardPicker")
    // Add models.
    for lexicalModel in lexicalModels {
      Manager.shared.addLexicalModel(lexicalModel)
      _ = Manager.shared.registerLexicalModel(lexicalModel)
    }
    restoreNavigation()
    navigationController?.popToRootViewController(animated: true)
  }
  
  private func keyboardDownloadFailed(_ notification: KeyboardDownloadFailedNotification) {
    log.info("keyboardDownloadFailed KeyboardPicker")
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
    
    let alertController = UIAlertController(title: title, message: notification.error.localizedDescription,
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertActionStyle.cancel,
                                            handler: nil))
    
    self.present(alertController, animated: true, completion: nil)
  }
  
  private func lexicalModelDownloadFailed(_ notification: LexicalModelDownloadFailedNotification) {
    log.info("lexicalModelDownloadFailed KeyboardPicker")
    restoreNavigation()
    
    let title: String
    if view == navigationController?.topViewController?.view {
      updateQueue = nil
      title = "Dictionary Update Error"
    } else {
      title = "Dictionary Download Error"
    }
    navigationController?.setToolbarHidden(true, animated: true)
    
    let alertController = UIAlertController(title: title, message: notification.error.localizedDescription,
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertActionStyle.cancel,
                                            handler: nil))
    
    self.present(alertController, animated: true, completion: nil)
  }
  
  // MARK: - keyboard adding

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

  // so KeyboardSwitcherViewController can override this to do nothing
  public func checkUpdates() -> Bool {
    if Manager.shared.apiKeyboardRepository.languages == nil {
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

  @objc func hideToolbarDelayed(_ timer: Timer) {
    navigationController?.setToolbarHidden(true, animated: true)
  }

  override func showAddKeyboard() {
    let button: UIButton? = (navigationController?.toolbar?.viewWithTag(toolbarButtonTag) as? UIButton)
    button?.isEnabled = false
    let vc = LanguageViewController(Manager.shared.apiKeyboardRepository)
    navigationController?.pushViewController(vc, animated: true)
    setIsDoneButtonEnabled(true)
  }
}
