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
  private let activityViewTag = -2

  private var userKeyboards: [String: InstallableKeyboard] = [:]
  private var isUpdate = false
  private var language: Language
  private var keyboardRepository: KeyboardRepository?
  private var pendingFetch: Bool = false

  private var keyboardDownloadStartedObserver: NotificationObserver?
  private var keyboardDownloadFailedObserver: NotificationObserver?

  convenience init(language: Language) {
    self.init(nil, language: language)
  }
  
  init(_ keyboardRepository: KeyboardRepository?, language: Language) {
    self.language = language
    super.init(nibName: nil, bundle: nil)
    
    self.keyboardRepository = keyboardRepository
    keyboardRepository?.delegate = self
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  override func loadView() {
    super.loadView()
    
    if keyboardRepository != nil {
      if let languageDict = keyboardRepository?.languages {
        self.postLanguageLoad(languageDict: languageDict)
      } else {
        log.info("Fetching repository from API for keyboard download (LanguageDetailViewController)")
        pendingFetch = true
        keyboardRepository?.fetch()
      }
    }
    
    loadUserKeyboards()
  }
  
  func postLanguageLoad(languageDict: [String: Language]) {
    pendingFetch = false
    
    if let language = languageDict[self.language.id] {
      self.language = language
      
      // Case 1 - called immediately during loadView() - UI is built after the load, so we're fine.
      // Case 2 - called by keyboardRepositoryDidFetch() - UI updated by self.tableView.reloadData()
    }
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
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    navigationController?.setToolbarHidden(true, animated: true)
    // if no rows to show yet, show a loading indicator
    if pendingFetch {
      showActivityView()
      log.info("didAppear: LanguageDetailViewController, but fetch is not yet complete")
    } else {
      log.info("didAppear: LanguageDetailViewController")
    }
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

    let kbState = ResourceDownloadManager.shared.stateForKeyboard(withID: keyboard.id)
    cell.setKeyboardState(kbState, selected: false, defaultAccessoryType: cell.accessoryType)
  }

  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    tableView.cellForRow(at: indexPath)?.isSelected = false
    let keyboardIndex = indexPath.section
    let keyboard = language.keyboards![keyboardIndex]

    let state = ResourceDownloadManager.shared.stateForKeyboard(withID: keyboard.id)
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
    ResourceDownloadManager.shared.downloadKeyboard(withID: language.keyboards![keyboardIndex].id,
                                      languageID: language.id, isUpdate: isUpdate)
  }
  
  func showActivityView() {
    view.isUserInteractionEnabled = false
    let indicatorView = UIActivityIndicatorView(activityIndicatorStyle: .whiteLarge)
    let activityView = UIView(frame: indicatorView.bounds.insetBy(dx: -10.0, dy: -10.0))
    activityView.backgroundColor = UIColor(white: 0.5, alpha: 0.8)
    activityView.layer.cornerRadius = 6.0
    activityView.center = view.center
    activityView.tag = activityViewTag
    activityView.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                                     .flexibleBottomMargin]

    indicatorView.center = CGPoint(x: activityView.bounds.size.width * 0.5, y: activityView.bounds.size.height * 0.5)
    indicatorView.startAnimating()
    activityView.addSubview(indicatorView)
    view.addSubview(activityView)
  }

  func dismissActivityView() {
    let activityView = view.viewWithTag(activityViewTag)
    activityView?.removeFromSuperview()
    view.isUserInteractionEnabled = true
  }
  
  private func showConnectionErrorAlert() {
    dismissActivityView()
    let alertController = UIAlertController(title: "Connection Error",
                                            message: "Could not reach Keyman server. Please try again later.",
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertActionStyle.default,
                                            handler: errorAcknowledgmentHandler))
    
    self.present(alertController, animated: true, completion: nil)
  }
  
  func errorAcknowledgmentHandler(withAction action: UIAlertAction) {
    navigationController?.popToRootViewController(animated: true)
  }

  private func keyboardDownloadStarted() {
    log.info("keyboardDownloadStarted: LanguageDetailViewController")
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)
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

// MARK: - KeyboardRepositoryDelegate
extension LanguageDetailViewController: KeyboardRepositoryDelegate {
  func keyboardRepositoryDidFetch(_ repository: KeyboardRepository) {
    if let languageDict = repository.languages {
      self.postLanguageLoad(languageDict: languageDict)
    }
    self.dismissActivityView()
    self.tableView.reloadData()
    if self.numberOfSections(in: self.tableView) == 0 {
      self.showConnectionErrorAlert()
    }
  }

  func keyboardRepository(_ repository: KeyboardRepository, didFailFetch error: Error) {
    dismissActivityView()
    showConnectionErrorAlert()
  }

  private func languageList(_ languageDict: [String: Language]) -> [Language] {
    return languageDict.values.sorted { a, b -> Bool in
      a.name.localizedCaseInsensitiveCompare(b.name) == .orderedAscending
    }
  }
}
