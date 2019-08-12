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
  private var userKeyboards: [String: InstallableKeyboard] = [:]
  private var isUpdate = false
  private let language: Language

  private var keyboardDownloadStartedObserver: NotificationObserver?
  private var keyboardDownloadFailedObserver: NotificationObserver?

  init(language: Language) {
    self.language = language
    super.init(nibName: nil, bundle: nil)
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func loadView() {
    super.loadView()
    loadUserKeyboards()
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    keyboardDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadStarted,
      observer: self,
      function: LanguageDetailViewController.keyboardDownloadStarted)
    keyboardDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadFailed,
      observer: self,
      function: LanguageDetailViewController.keyboardDownloadFailed)
    log.info("viewDidLoad: LanguageDetailViewController (registered for keyboardDownloadStarted)")
  }
  
  override open func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    
    log.info("willAppear: LanguageDetailViewController")
  }

  // MARK: - Table view data source UITableViewDataSource

  override func numberOfSections(in tableView: UITableView) -> Int {
    return language.keyboards!.count
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
    let keyboard = language.keyboards![indexPath.section]
    let cell = cell as! KeyboardNameTableViewCell
    cell.indexPath = indexPath
    cell.textLabel?.text = keyboard.name
    if isAdded(languageID: language.id, keyboardID: keyboard.id) {
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

    let kbState = Manager.shared.stateForKeyboard(withID: keyboard.id)
    cell.setKeyboardState(kbState, selected: false, defaultAccessoryType: cell.accessoryType)
  }

  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    tableView.cellForRow(at: indexPath)?.isSelected = false
    let keyboardIndex = indexPath.section
    let keyboard = language.keyboards![keyboardIndex]

    let state = Manager.shared.stateForKeyboard(withID: keyboard.id)
    if state != .downloading {
      if state == .needsDownload {
        isUpdate = false
      } else {
        isUpdate = true
      }
    
        let alertController = UIAlertController(title: "\(language.name): \(keyboard.name)",
                                                message: "Would you like to download this keyboard?",
                                                preferredStyle: UIAlertControllerStyle.alert)
        alertController.addAction(UIAlertAction(title: "Cancel",
                                                style: UIAlertActionStyle.cancel,
                                                handler: nil))
        alertController.addAction(UIAlertAction(title: "Download",
                                                style: UIAlertActionStyle.default,
                                                handler: {_ in self.downloadHandler(keyboardIndex)} ))
        
        self.present(alertController, animated: true, completion: nil)
    }
  }

  func downloadHandler(_ keyboardIndex: Int) {
    Manager.shared.downloadKeyboard(withID: language.keyboards![keyboardIndex].id,
                                      languageID: language.id, isUpdate: isUpdate)
  }

  private func keyboardDownloadStarted() {
    log.info("keyboardDownloadStarted: LanguageDetailViewController")
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

  private func keyboardDownloadFailed() {
    log.info("keyboardDownloadFailed: LanguageDetailViewController")
    view.isUserInteractionEnabled = true
    navigationItem.setHidesBackButton(false, animated: true)
  }

  private func loadUserKeyboards() {
    guard let userKbList = Storage.active.userDefaults.userKeyboards, !userKbList.isEmpty else {
      userKeyboards = [:]
      return
    }

    userKeyboards = [:]
    for kb in userKbList {
      let dictKey = "\(kb.languageID)_\(kb.id)"
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
