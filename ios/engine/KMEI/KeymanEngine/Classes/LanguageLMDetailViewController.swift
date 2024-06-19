//
//  LanguageLMDetailViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 7/19/19.
//  Copyright © 2019 SIL International. All rights reserved.
//


import UIKit
import os.log

private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

class LanguageLMDetailViewController: UITableViewController, UIAlertViewDelegate {
  private var userLexicalModels: [String: InstallableLexicalModel] = [:]
  private var isUpdate = false // unused currently, may be used when we switch to HTTPDownloader
  private let language: Language
  public var packages: [(InstallableLexicalModel, URL)]
  
  private var lexicalModelDownloadStartedObserver: NotificationObserver?
  //NOTE: there is no need for a CompletedObserver, as our parent LexicalModelPickerViewController
  //  is registered for that and deals with it by popping us out to root.
  private var lexicalModelDownloadFailedObserver: NotificationObserver?

  private var onSuccessClosure: ((InstallableLexicalModel) -> Void)?
  
  init(language: Language, packages: [(InstallableLexicalModel, URL)], onSuccess: ((InstallableLexicalModel) -> Void)?) {
    self.language = language
    self.packages = packages
    self.onSuccessClosure = onSuccess
    super.init(nibName: nil, bundle: nil)
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func loadView() {
    super.loadView()
    loadUserLexicalModels()
    tableView.dataSource = self
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()
    lexicalModelDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadStarted,
      observer: self,
      function: LanguageLMDetailViewController.lexicalModelDownloadStarted)
    lexicalModelDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadFailed,
      observer: self,
      function: LanguageLMDetailViewController.lexicalModelDownloadFailed)
    os_log("viewDidLoad: LanguageLMDetailViewController (registered for lexicalModelDownloadStarted)", log:KeymanEngineLogger.ui, type: .info)
  }
  
  override open func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    
    os_log("viewWillAppear: LanguageLMDetailViewController", log:KeymanEngineLogger.ui, type: .info)
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    os_log("viewDidAppear: LanguageLMDetailViewController", log:KeymanEngineLogger.ui, type: .info)

    navigationController?.setToolbarHidden(true, animated: true)
  }
  
  // MARK: - Table view data source UITableViewDataSource

  override func numberOfSections(in tableView: UITableView) -> Int {
    return packages.count
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
    selectionColor.backgroundColor = Colors.selectionPrimary
    cell.selectedBackgroundView = selectionColor
    return cell
  }
  
  override func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    let lexicalModel = packages[indexPath.section].0
    let cell = cell as! KeyboardNameTableViewCell
    cell.indexPath = indexPath
    cell.textLabel?.text = lexicalModel.name
    if isAdded(languageID: language.id, lexicalModelID: lexicalModel.id) {
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

    let state = ResourceFileManager.shared.installState(forPackage: KeymanPackage.Key(id: lexicalModel.id, type: .lexicalModel))
    cell.setInstallState(state, selected: false, defaultAccessoryType: cell.accessoryType)
  }
  
  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    tableView.cellForRow(at: indexPath)?.isSelected = false
    let lexicalModelIndex = indexPath.section
    let lexicalModel = packages[lexicalModelIndex].0

    let state = ResourceFileManager.shared.installState(forPackage: KeymanPackage.Key(id: lexicalModel.id, type: .lexicalModel))
    if state != .downloading {
      if state == .none {
        isUpdate = false
      } else {
        isUpdate = true
      }

      let format = NSLocalizedString("menu-lexical-model-install-title", bundle: engineBundle, comment: "")
      let alertController = UIAlertController(title: String.localizedStringWithFormat(format, language.name, lexicalModel.name),
        message: NSLocalizedString("menu-lexical-model-install-message", bundle: engineBundle, comment: ""),
        preferredStyle: UIAlertController.Style.alert)
      alertController.addAction(UIAlertAction(title: NSLocalizedString("command-cancel", bundle: engineBundle, comment: ""),
                                              style: UIAlertAction.Style.cancel,
                                              handler: nil))
      alertController.addAction(UIAlertAction(title: NSLocalizedString("command-install", bundle: engineBundle, comment: ""),
                                              style: UIAlertAction.Style.default,
                                              handler: {_ in self.downloadHandler(lexicalModelIndex)} ))
      
      self.present(alertController, animated: true, completion: nil)
    }
  }
  
  func downloadHandler(_ lexicalModelIndex: Int) {
    let package = packages[lexicalModelIndex]
    let lmFullID = package.0.fullID
    let completionClosure: ResourceDownloadManager.CompletionHandler<LexicalModelKeymanPackage> = { package, error in
      try? ResourceDownloadManager.shared.standardLexicalModelInstallCompletionBlock(forFullID: lmFullID)(package, error)

      if let lm = package?.findResource(withID: lmFullID) {
        self.onSuccessClosure?(lm)
      }
    }

    ResourceDownloadManager.shared.downloadPackage(withKey: package.0.packageKey, from: package.1, withNotifications: true, completionBlock: completionClosure)
  }
  
  private func lexicalModelDownloadStarted() {
    os_log("lexicalModelDownloadStarted: LanguageLMDetailViewController", log:KeymanEngineLogger.resources, type: .info)
    view.isUserInteractionEnabled = false
    
    navigationItem.setHidesBackButton(true, animated: true)
    navigationController?.setToolbarHidden(false, animated: true)
  }
  
  private func lexicalModelDownloadFailed() {
    os_log("lexicalModelDownloadFailed: LanguageLMDetailViewController", log:KeymanEngineLogger.resources, type: .info)
   view.isUserInteractionEnabled = true
    navigationItem.setHidesBackButton(false, animated: true)
  }
  
  private func loadUserLexicalModels() {
    guard let userLmList = Storage.active.userDefaults.userLexicalModels, !userLmList.isEmpty else {
      userLexicalModels = [:]
      return
    }
    
    userLexicalModels = [:]
    for lm in userLmList {
      let dictKey = "\(lm.languageID)_\(lm.id)"
      userLexicalModels[dictKey] = lm
    }
  }
  
  private func isAdded(languageID: String?, lexicalModelID: String?) -> Bool {
    guard let languageID = languageID, let lexicalModelID = lexicalModelID else {
      return false
    }
    return userLexicalModels["\(languageID)_\(lexicalModelID)"] != nil
  }
}
