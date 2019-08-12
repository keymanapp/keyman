//
//  LexicalModelPickerViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 3/19/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import Foundation
import UIKit

private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

class LexicalModelPickerViewController: UITableViewController, UIAlertViewDelegate {
  private var userLexicalModels: [InstallableLexicalModel] = [InstallableLexicalModel]()
  public var language: Language!
  private var updateQueue: [InstallableLexicalModel]?
  private var _isDoneButtonEnabled = false
  private var isDidUpdateCheck = false
  
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
    
    title = "\(language.name) Dictionaries"
    isDidUpdateCheck = false
    updateQueue = nil
    if Manager.shared.canAddNewLexicalModels {
      let addButton = UIBarButtonItem(barButtonSystemItem: .add, target: self,
                                      action: #selector(self.addClicked))
      navigationItem.rightBarButtonItem = addButton
    }
    
    navigationController?.toolbar?.barTintColor = UIColor(red: 0.5, green: 0.75,
                                                          blue: 0.25, alpha: 0.9)
    
    lexicalModelDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadStarted,
      observer: self,
      function: LexicalModelPickerViewController.lexicalModelDownloadStarted)
    lexicalModelDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadCompleted,
      observer: self,
      function: LexicalModelPickerViewController.lexicalModelDownloadCompleted)
    lexicalModelDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadFailed,
      observer: self,
      function: LexicalModelPickerViewController.lexicalModelDownloadFailed)
    
    log.info("didLoad: LexicalModelPickerViewController (registered lexicalModelDownloadCompleted et al)")
  }
  
  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    
    loadUserLexicalModels()
    scroll(toSelectedLexicalModel: false)
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    log.info("didAppear: LexicalModelPickerViewController")
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
    selectionColor.backgroundColor = UIColor(red: 204.0 / 255.0, green: 136.0 / 255.0, blue: 34.0 / 255.0, alpha: 1.0)
    cell.selectedBackgroundView = selectionColor
    return cell
  }
  
  // TODO: Refactor. Duplicated in LexicalModelInfoViewController
  override func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
    if !Manager.shared.canRemoveLexicalModels {
      return false
    }
    
    return true
  }
  
  override func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCellEditingStyle,
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
    let version = lm.version
    
    let infoView = LexicalModelInfoViewController()
    infoView.title = lm.name
    infoView.lexicalModelCount = userLexicalModels.count
    infoView.lexicalModelIndex = local2globalIndex(index)
    infoView.lexicalModelID = lm.id
    infoView.languageID = lm.languageID
    infoView.lexicalModelVersion = version
    infoView.isCustomLexicalModel = lm.isCustom
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
  
  private func lexicalModelDownloadStarted(_ lexicalModels: [InstallableLexicalModel]) {
    view.isUserInteractionEnabled = false
    navigationItem.leftBarButtonItem?.isEnabled = false
    navigationItem.rightBarButtonItem?.isEnabled = false
  }
  
  private func lexicalModelDownloadCompleted(_ lexicalModels: [InstallableLexicalModel]) {
    log.info("lexicalModelDownloadCompleted LexicalModelPicker")
    if view == navigationController?.topViewController?.view {
      if updateQueue == nil {
        return
      }
      Manager.shared.shouldReloadLexicalModel = true
      
      // Update lexicalModel version
      for lexicalModel in lexicalModels {
        Manager.shared.updateUserLexicalModels(with: lexicalModel)
      }
      
      updateQueue!.remove(at: 0)
      if !updateQueue!.isEmpty {
        let langID = updateQueue![0].languageID
        let lmID = updateQueue![0].id
        Manager.shared.downloadLexicalModel(withID: lmID, languageID: langID, isUpdate: true)
      } else {
        loadUserLexicalModels()
        view.isUserInteractionEnabled = true
        navigationItem.leftBarButtonItem?.isEnabled = true
        if navigationItem.rightBarButtonItem != nil {
          navigationItem.rightBarButtonItem?.isEnabled = true
        }
        updateQueue = nil
        let label = navigationController?.toolbar?.viewWithTag(toolbarLabelTag) as? UILabel
        label?.text = "Dictionaries successfully updated!"
        navigationController?.toolbar?.viewWithTag(toolbarActivityIndicatorTag)?.removeFromSuperview()
        Timer.scheduledTimer(timeInterval: 3.0, target: self, selector: #selector(self.hideToolbarDelayed),
                             userInfo: nil, repeats: false)
      }
    } else {
      let label = navigationController?.toolbar?.viewWithTag(toolbarLabelTag) as? UILabel
      label?.text = "Dictionary successfully downloaded!"
      navigationController?.toolbar?.viewWithTag(toolbarActivityIndicatorTag)?.removeFromSuperview()
      Timer.scheduledTimer(timeInterval: 3.0, target: self, selector: #selector(self.hideToolbarDelayed),
                           userInfo: nil, repeats: false)
      
      view.isUserInteractionEnabled = true
      navigationItem.leftBarButtonItem?.isEnabled = true
      if navigationItem.rightBarButtonItem != nil {
        navigationItem.rightBarButtonItem?.isEnabled = true
      }
      
      // Add lexicalModel.
      for lexicalModel in lexicalModels {
        if lexicalModel.languageID == language?.id {
          Manager.shared.addLexicalModel(lexicalModel)
          switchLexicalModel(lexicalModel)
        }
      }
      
      navigationController?.popToRootViewController(animated: true)
    }
  }
  
  private func lexicalModelDownloadFailed(_ notification: LexicalModelDownloadFailedNotification) {
    view.isUserInteractionEnabled = true
    navigationItem.leftBarButtonItem?.isEnabled = true
    if let item = navigationItem.rightBarButtonItem {
      item.isEnabled = true
    }
    
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
    updateLexicalModels()
  }
  
  private func checkUpdates() -> Bool {
    if Manager.shared.apiLexicalModelRepository.lexicalModels == nil {
      return false
    }
    
    isDidUpdateCheck = true
    return userLexicalModels.contains { lexicalModel in
      let lmID = lexicalModel.id
      return Manager.shared.stateForLexicalModel(withID: lmID) == .needsUpdate
    }
  }
  
  private func updateLexicalModels() {
    updateQueue = []
    var lmIDs = Set<String>()
    for lm in userLexicalModels {
      let lmState = Manager.shared.stateForLexicalModel(withID: lm.id)
      if lmState == .needsUpdate {
        if !lmIDs.contains(lm.id) {
          lmIDs.insert(lm.id)
          updateQueue!.append(lm)
        }
      }
    }
    
    if !updateQueue!.isEmpty {
      let langID = updateQueue![0].languageID
      let lmID = updateQueue![0].id
      Manager.shared.downloadLexicalModel(withID: lmID, languageID: langID, isUpdate: true)
    }
  }
  
  private func scroll(toSelectedLexicalModel animated: Bool) {
    let index = userLexicalModels.index { lm in
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
  
  func showAddLexicalModel() {
    //get list of lexical models for this languageID and show it
    func listCompletionHandler(lexicalModels: [LexicalModel]?, error: Error?) -> Void {
      if let error = error {
        log.info("Failed to fetch lexical model list for "+language.id+". error: "+(error as! String))
        DispatchQueue.main.async {
          self.lexicalModelDownloadFailed(LexicalModelDownloadFailedNotification(lmOrLanguageID: self.language.id, error: error))
        }
      } else if nil == lexicalModels {
        log.info("No lexical models available for language \(language.id) (nil)")
      } else if 0 == lexicalModels?.count {
        log.info("No lexical models available for language \(language.id) (empty)")
      } else {
        log.info("Fetched lexical model list for "+language.id+".")
        // show the list of lexical models (on the main thread)
        DispatchQueue.main.async {
          let button: UIButton? = (self.navigationController?.toolbar?.viewWithTag(toolbarButtonTag) as? UIButton)
          button?.isEnabled = false
          let vc = LanguageLMDetailViewController(language: self.language)
          vc.lexicalModels = lexicalModels!
          self.navigationController?.pushViewController(vc, animated: true)
        }
      }
    }
    
    Manager.shared.apiLexicalModelRepository.fetchList(languageID: language.id, completionHandler: listCompletionHandler)
  }
  
}

