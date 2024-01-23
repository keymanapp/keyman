//
//  LexicalModelPickerViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/19/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import UIKit
import os.log

private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

class LexicalModelPickerViewController: UITableViewController, UIAlertViewDelegate {
  private var userLexicalModels: [InstallableLexicalModel] = [InstallableLexicalModel]()
  public var language: Language!
  private var _isDoneButtonEnabled = false
  
  private var lexicalModelDownloadStartedObserver: NotificationObserver?
  private var lexicalModelDownloadCompletedObserver: NotificationObserver?
  private var lexicalModelDownloadFailedObserver: NotificationObserver?
  
  public init(_ language: Language) {
    self.language = language
    super.init(nibName: nil, bundle: nil)
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()

    let format = NSLocalizedString("menu-lexical-model-title", bundle: engineBundle, comment: "")
    title = String.localizedStringWithFormat(format, language.name)
    if Manager.shared.canAddNewLexicalModels {
      let addButton = UIBarButtonItem(barButtonSystemItem: .add, target: self,
                                      action: #selector(self.addClicked))
      navigationItem.rightBarButtonItem = addButton
    }
    
    navigationController?.toolbar?.barTintColor = Colors.statusToolbar
    
    lexicalModelDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadStarted,
      observer: self,
      function: LexicalModelPickerViewController.lexicalModelDownloadStarted)
    lexicalModelDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadCompleted,
      observer: self,
      function: LexicalModelPickerViewController.lexicalModelDownloadCompleted)
    lexicalModelDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadFailed,
      observer: self,
      function: LexicalModelPickerViewController.lexicalModelDownloadFailed)
    
    os_log("viewDidLoad: LexicalModelPickerViewController (registered lexicalModelDownloadCompleted et al)", log:KeymanEngineLogger.ui, type: .info)
  }
  
  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    
    loadUserLexicalModels()
    scroll(toSelectedLexicalModel: false)
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    os_log("viewDidAppear: LexicalModelPickerViewController", log:KeymanEngineLogger.ui, type: .info)

    scroll(toSelectedLexicalModel: false)
}
  
  // MARK: - Table view data source UITableViewDataSource
  
  override func numberOfSections(in tableView: UITableView) -> Int {
    return 1
  }
  
  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return userLexicalModels.count
  }
  
  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      return cell
    }
    
    let cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    let selectionColor = UIView()
    selectionColor.backgroundColor = Colors.selectionPrimary
    cell.selectedBackgroundView = selectionColor
    return cell
  }

  override func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
    if !Manager.shared.canRemoveLexicalModels {
      return false
    }
    
    return true
  }
  
  override func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCell.EditingStyle,
                          forRowAt indexPath: IndexPath) {
    if editingStyle != .delete {
      return
    }
    
    let globalIndex = local2globalIndex(indexPath.row)
    if Manager.shared.removeLexicalModel(at: globalIndex) {
      loadUserLexicalModels()
    }
  }
  
  override func tableView(_ tableView: UITableView,
                          accessoryButtonTappedForRowWith indexPath: IndexPath) {
    showLexicalModelInfoView(with: indexPath.row)
  }
  
  func showLexicalModelInfoView(with index: Int) {
    let lm = userLexicalModels[index]

    let infoView = ResourceInfoViewController(for: lm, mayDelete: Manager.shared.canRemoveLexicalModels)
    navigationController?.pushViewController(infoView, animated: true)
  }
  
  // MARK: - UITableViewDelegate

  override func tableView(_ tableView: UITableView,
                          willDisplay cell: UITableViewCell,
                          forRowAt indexPath: IndexPath) {
    cell.selectionStyle = .none
    let lm = userLexicalModels[indexPath.row]
    
    cell.textLabel?.text = lm.name
    cell.detailTextLabel?.text = lm.id // maybe put a longer description here?
    cell.tag = indexPath.row
    
    if Manager.shared.currentLexicalModelID == lm.fullID {
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
    switchLexicalModel(indexPath.row)
  }
  
  func local2globalIndex(_ localIndex: Int) -> Int {
    let globalIndex: Int
    if let lang = self.language {
      let lm = userLexicalModels[localIndex]
      globalIndex = Storage.active.userDefaults.userLexicalModels?.firstIndex(where: {
        $0.languageID == lang.id && $0.id == lm.id
      }) ?? 0
    } else {
      globalIndex = localIndex
    }
    return globalIndex
  }
  
  private func lexicalModelDownloadStarted() {
    view.isUserInteractionEnabled = false
    navigationItem.leftBarButtonItem?.isEnabled = false
    navigationItem.rightBarButtonItem?.isEnabled = false
  }
  
  private func lexicalModelDownloadCompleted() {
    os_log("lexicalModelDownloadCompleted LexicalModelPicker", log:KeymanEngineLogger.ui, type: .info)

    // Actually used now.
    view.isUserInteractionEnabled = true
    navigationItem.leftBarButtonItem?.isEnabled = true
    if navigationItem.rightBarButtonItem != nil {
      navigationItem.rightBarButtonItem?.isEnabled = true
    }
    
    navigationController?.popToRootViewController(animated: true)
  }
  
  private func lexicalModelDownloadFailed(_ notification: PackageDownloadFailedNotification) {
    view.isUserInteractionEnabled = true
    navigationItem.leftBarButtonItem?.isEnabled = true
    if let item = navigationItem.rightBarButtonItem {
      item.isEnabled = true
    }
    
    let title: String = NSLocalizedString("notification-download-failure-lexical-model", bundle: engineBundle, comment: "")
    navigationController?.setToolbarHidden(true, animated: true)
    
    let alertController = UIAlertController(title: title, message: notification.error.localizedDescription,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: NSLocalizedString("command-ok", bundle: engineBundle, comment: ""),
                                            style: UIAlertAction.Style.cancel,
                                            handler: { _ in
                                              self.navigationController?.popToRootViewController(animated: true)
                                            }))
    
    self.present(alertController, animated: true, completion: nil)
  }
  
  private func switchLexicalModel(_ model: InstallableLexicalModel) {
    // Switch lexicalModel and register to user defaults.
    if Manager.shared.registerLexicalModel(model) {
      tableView.reloadData()
    }
    
    // Register the lexical model in defaults!
    Storage.active.userDefaults.set(preferredLexicalModelID: model.id, forKey: model.languageID)
    
    if !_isDoneButtonEnabled {
      Manager.shared.dismissLexicalModelPicker(self)
    }
  }
  
  private func switchLexicalModel(_ index: Int) {
    switchLexicalModel(userLexicalModels[index])
  }
  
  // if we have a language set, filter models down to those of that language
  private func loadUserLexicalModels() {
    if let lang = self.language {
      userLexicalModels =  Storage.active.userDefaults.userLexicalModels?.filter({
        $0.languageID == lang.id
      }) ?? []
    } else {
      userLexicalModels = Storage.active.userDefaults.userLexicalModels ?? []
    }
    tableView.reloadData()
  }
  
  // MARK: - lexical model adding functionaliry
  
  @objc func doneClicked(_ sender: Any) {
    Manager.shared.dismissLexicalModelPicker(self)
  }
  
  @objc func cancelClicked(_ sender: Any) {
    Manager.shared.dismissLexicalModelPicker(self)
  }
  
  @objc func addClicked(_ sender: Any) {
    showAddLexicalModel()
  }
  
  private func scroll(toSelectedLexicalModel animated: Bool) {
    let index = userLexicalModels.firstIndex { lm in
      return Manager.shared.currentLexicalModelID == lm.fullID
    }
    
    if let index = index {
      let indexPath = IndexPath(row: index, section: 0)
      tableView.scrollToRow(at: indexPath, at: .middle, animated: animated)
      
    }
  }
  
  private func setIsDoneButtonEnabled(_ value: Bool) {
    _isDoneButtonEnabled = value
    if _isDoneButtonEnabled {
      let doneButton = UIBarButtonItem(title: NSLocalizedString("command-done", bundle: engineBundle, comment: ""), style: .plain, target: self,
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
  
  func showAddLexicalModel() {
    //get list of lexical models for this languageID and show it
    Queries.LexicalModel.fetch(forLanguageCode: language.id) { result, error in
      if let error = error {
        let errorMessage = "Failed to fetch lexical model list for \(self.language.id). Error: \(error.localizedDescription)"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, errorMessage)
        DispatchQueue.main.async {
          self.lexicalModelDownloadFailed(PackageDownloadFailedNotification(packageKey: nil, error: error))
        }
        return
      }

      guard let result = result else {
        self.noModelsAvailable(cause: "nil")
        return
      }

      if result.count == 0 {
        self.noModelsAvailable(cause: "empty")
      } else {
        let message = "Fetched lexical model list for \(self.language.id)."
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        let packages: [(InstallableLexicalModel, URL)] = result.map { ($0.modelFor(languageID: self.language.id)!, URL.init(string: $0.packageFilename)!) }
        // show the list of lexical models (on the main thread)
        DispatchQueue.main.async {
          let button: UIButton? = (self.navigationController?.toolbar?.viewWithTag(toolbarButtonTag) as? UIButton)
          button?.isEnabled = false
          let vc = LanguageLMDetailViewController(language: self.language, packages: packages, onSuccess: { lm in
            self.switchLexicalModel(lm)
          })
          self.navigationController?.pushViewController(vc, animated: true)
        }
      }
    }
  }

  func noModelsAvailable(cause: String = "nil") {
    let message = NSLocalizedString("menu-lexical-model-none-message", bundle: engineBundle, comment: "")
    let logMessage = "No lexical models available for language \(language.id): (\(cause))"
    os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, logMessage)

    let alertController = UIAlertController(title: title, message: message,
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: NSLocalizedString("command-ok", bundle: engineBundle, comment: ""),
                                            style: UIAlertAction.Style.default,
                                            handler: { _ in
                                              self.navigationController?.popViewController(animated: true)
                                            }))

    self.present(alertController, animated: true, completion: nil)
  }
  
}

