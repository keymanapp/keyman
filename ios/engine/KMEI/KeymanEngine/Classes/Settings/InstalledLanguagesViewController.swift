//
//  InstalledLanguagesViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 5/20/19.
//  Copyright © 2019 SIL International. All rights reserved.
//
//
import QuartzCore
import UIKit
import os.log

private let activityViewTag = -2
private let toolbarButtonTag = 100
private let toolbarLabelTag = 101
private let toolbarActivityIndicatorTag = 102

public class InstalledLanguagesViewController: UITableViewController, UIAlertViewDelegate {
  private var userKeyboards: [String: InstallableKeyboard] = [:]
  private var userLexicalModels: [String: InstallableLexicalModel] = [:]
  private var sectionIndexTitles: [String] = []
  private var indices: [Int] = []
  private var selectedSection = 0
  private var installedLanguages: [String: Language]
  private var languages: [Language] = []
  
  private var isDidUpdateCheck = false
  
  private var packageDownloadStartedObserver: NotificationObserver?
  private var packageDownloadCompletedObserver: NotificationObserver?
  private var packageDownloadFailedObserver: NotificationObserver?
  private var batchUpdateStartedObserver: NotificationObserver?
  private var batchUpdateCompletedObserver: NotificationObserver?
  
  public init(_ givenLanguages: [String: Language]? = nil) {
    if(givenLanguages == nil) {
      self.installedLanguages = InstalledLanguagesViewController.loadUserLanguages()
    } else {
      self.installedLanguages = givenLanguages!
    }
    super.init(nibName: nil, bundle: nil)
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override public func loadView() {
    super.loadView()
    languages = languageList(installedLanguages)
  }
  
  private func languageList(_ languageDict: [String: Language]) -> [Language] {
    return languageDict.values.sorted { a, b -> Bool in
      a.name.localizedCaseInsensitiveCompare(b.name) == .orderedAscending
    }
  }
  
  override public func viewDidLoad() {
    super.viewDidLoad()
    
    title = NSLocalizedString("menu-installed-languages-title", bundle: engineBundle, comment: "")
    selectedSection = NSNotFound
    packageDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadStarted,
      observer: self,
      function: InstalledLanguagesViewController.packageDownloadStarted)
    packageDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadCompleted,
      observer: self,
      function: InstalledLanguagesViewController.packageDownloadCompleted)
    packageDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.packageDownloadFailed,
      observer: self,
      function: InstalledLanguagesViewController.packageDownloadFailed)
    
    batchUpdateStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.batchUpdateStarted,
      observer: self,
      function: InstalledLanguagesViewController.batchUpdateStarted)
    batchUpdateCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.batchUpdateCompleted,
      observer: self,
      function: InstalledLanguagesViewController.batchUpdateCompleted)
    
    if Manager.shared.canAddNewKeyboards {
      let addButton = UIBarButtonItem(barButtonSystemItem: .add, target: self,
                                      action: #selector(self.addClicked))
      navigationItem.rightBarButtonItem = addButton
    }
    
    os_log("viewDidLoad: InstalledLanguagesViewController (registered for keyboardDownloadStarted)", log:KeymanEngineLogger.resources, type: .info)
  }
  
  override public func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    loadUserKeyboards()
    loadUserLexicalModels()
  }
  
  override public func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    
    // if no rows to show yet, show a loading indicator
    if numberOfSections(in: tableView) == 0 {
      showActivityView()
    }
    os_log("didAppear: InstalledLanguagesViewController", log:KeymanEngineLogger.ui, type: .info)
    
    // Are there updates worth doing?
    if isDidUpdateCheck {
      // Nope; don't make an 'update' button.
      return
    }
    
    if ResourceDownloadManager.shared.updatesAvailable == false {
      // No updates available?  Don't do update-y things.
      return
    }
    
    // We do have updates - prepare the UI!
    if let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar {
      toolbar.displayButton(NSLocalizedString("notification-update-available", bundle: engineBundle, comment: ""), with: self, callback: #selector(self.updateClicked))
    }
  }
  
  @objc func updateClicked(_ sender: Any) {
    if let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar {
      toolbar.displayStatus(NSLocalizedString("notification-update-processing", bundle: engineBundle, comment: ""), withIndicator: true)
    }
    
    // Do the actual updates!
    // TODO:  Consider prompting per resource, rather than wholesale as a group.
    // (This would be an enhancement, though.)
    let availableUpdates = ResourceDownloadManager.shared.getKeysForUpdatablePackages()
    ResourceDownloadManager.shared.performBatchUpdate(forPackageKeys: availableUpdates,
                                                      withNotifications: true,
                                                      completionBlock: { successes, failures in
      // TODO:  future feature:  consider reworking the notification listener for use with this callback.
      //        (See `batchUpdateCompleted` later in this file.)
    })
  }
  
  override public func numberOfSections(in tableView: UITableView) -> Int {
    return languages.count
  }
  
  private static func loadUserLanguages() -> [String: Language] {
    //iterate the list of installed languages and save their names
    // Get keyboards list if it exists in user defaults, otherwise create a new one
    let userDefaults = Storage.active.userDefaults
    let userKeyboards = userDefaults.userKeyboards ?? []
    
    var keyboardLanguages = [String: Language]()
    for k in userKeyboards {
      let l = k.languageID
      var kbds: [Keyboard]
      if let existingLanguage = keyboardLanguages[l] {
        kbds = existingLanguage.keyboards ?? []
        kbds.append(Keyboard(name: k.name, id: k.id, filename: "no filename", isDefault: nil, isRTL: k.isRTL, lastModified: Date(), fileSize: 0, version: k.version, languages: nil, font: nil, oskFont: nil))
      } else {
        kbds = [Keyboard(name: k.name, id: k.id, filename: "no filename", isDefault: nil, isRTL: k.isRTL, lastModified: Date(), fileSize: 0, version: k.version, languages: nil, font: nil, oskFont: nil)]
      }
      let userDefaults : UserDefaults = Storage.active.userDefaults
      let lmListInstalled: [InstallableLexicalModel] = userDefaults.userLexicalModelsForLanguage(languageID: l) ?? []
      let lmList = SettingsViewController.installed2API(lmListInstalled)
      keyboardLanguages[l] = Language(name: k.languageName, id: k.languageID, keyboards: kbds, lexicalModels: lmList, font: nil, oskFont: nil)
    }
    // there shouldn't be any lexical models for languages that don't have a keyboard installed
    //  but check
    let userLexicalModels = userDefaults.userLexicalModels ?? []
    for lm in userLexicalModels {
      let l = lm.languageID
      if let langName = keyboardLanguages[l]?.name {
        let message = "keyboard language \(l) \(langName) has lexical model"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
      } else {
        // Legacy behavior:  we automatically install all MTNT language codes, even without
        // a matching keyboard for the more specific variant(s).
        let message = "lexical model language \(l) has no keyboard installed!"
        os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
        SentryManager.breadcrumb(message)
      }
    }
    
    return keyboardLanguages
  }
  
  override public func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return 1
  }
  
  override public func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifierType1 = "CellType1"
    let cellIdentifierType2 = "CellType2"
    let keyboards = languages[indexPath.section].keyboards!
    let cellIdentifier = (keyboards.count < 2) ? cellIdentifierType1 : cellIdentifierType2
    let cell: UITableViewCell
    if let reusedCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      cell = reusedCell
    } else {
      let selectionColor = UIView()
      
      selectionColor.backgroundColor = Colors.selectionPrimary
      
      if keyboards.count < 2 {
        cell = KeyboardNameTableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
      } else {
        cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
      }
      cell.selectedBackgroundView = selectionColor
      cell.accessoryType = .disclosureIndicator
    }
    
    let formatString = NSLocalizedString("settings-keyboards-installed-count", bundle: engineBundle, comment: "")
    cell.detailTextLabel?.text = keyboards.count < 2 ?
    (keyboards.first?.name ?? "") :
    String.localizedStringWithFormat(formatString, keyboards.count)
    return cell
  }
  
  // Create sections by first letter
  override public func sectionIndexTitles(for tableView: UITableView) -> [String] {
    if !sectionIndexTitles.isEmpty {
      return sectionIndexTitles
    }
    
    sectionIndexTitles = []
    indices = []
    for (index, language) in languages.enumerated() {
      let firstLetter = String(language.name.prefix(1)).uppercased()
      if !sectionIndexTitles.contains(firstLetter) {
        sectionIndexTitles.append(firstLetter)
        indices.append(index)
      }
    }
    return sectionIndexTitles
  }
  
  override public func tableView(_ tableView: UITableView, sectionForSectionIndexTitle title: String, at index: Int) -> Int {
    return indices[index]
  }
  
  override public func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    guard let language = languages[safe: indexPath.section] else {
      return
    }
    
    cell.textLabel?.text = language.name
  }
  
  override public func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    selectedSection = indexPath.section
    tableView.cellForRow(at: indexPath)?.isSelected = false
    let title = tableView.cellForRow(at: indexPath)?.textLabel?.text ?? ""
    showLanguageSettingsView(title: title, languageIndex: indexPath.section)
  }
  
  override public func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: IndexPath) {
    let title = tableView.cellForRow(at: indexPath)?.textLabel?.text ?? ""
    showLanguageSettingsView(title: title, languageIndex: indexPath.section)
  }
  
  private func showLanguageSettingsView(title: String, languageIndex: Int) {
    let langSettingsView = LanguageSettingsViewController(languages[languageIndex])
    langSettingsView.title = title
    navigationController?.pushViewController(langSettingsView, animated: true)
  }
  
  func errorAcknowledgmentHandler(withAction action: UIAlertAction) {
    if !languages.isEmpty {
      navigationController?.popToRootViewController(animated: true)
    }
  }
  
  private func restoreNavigation() {
    view.isUserInteractionEnabled = true
    navigationItem.setHidesBackButton(false, animated: true)
    navigationItem.leftBarButtonItem?.isEnabled = true
    if navigationItem.rightBarButtonItem != nil {
      navigationItem.rightBarButtonItem?.isEnabled = true
    }
  }
  
  private func packageDownloadStarted(key: KeymanPackage.Key) {
    let message = "download started for \(key.type.rawValue): InstalledLanguagesViewController"
    os_log("%{public}s", log:KeymanEngineLogger.resources, type: .info, message)
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)
    
    guard let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar else {
      return
    }
    
    var msg: String
    switch(key.type) {
    case .keyboard:
      msg = NSLocalizedString("notification-downloading-keyboard", bundle: engineBundle, comment: "")
    case .lexicalModel:
      msg = NSLocalizedString("notification-downloading-lexical-model", bundle: engineBundle, comment: "")
    }
    
    toolbar.displayStatus(msg, withIndicator: true)
  }
  
  private func packageDownloadCompleted(package: KeymanPackage) {
    os_log("lexicalModelDownloadCompleted: InstalledLanguagesViewController", log:KeymanEngineLogger.resources, type: .info)
    
    DispatchQueue.main.async {
      var msg: String
      
      switch(package.resourceType()) {
      case .keyboard:
        Manager.shared.inputViewController.setShouldReload()
        msg = NSLocalizedString("notification-download-success-keyboard", bundle: engineBundle, comment: "")
      case .lexicalModel:
        msg = NSLocalizedString("notification-download-success-lexical-model", bundle: engineBundle, comment: "")
      }
      
      if let toolbar = self.navigationController?.toolbar as? ResourceDownloadStatusToolbar {
        toolbar.displayStatus(msg, withIndicator: false, duration: 3.0)
      }
      
      self.restoreNavigation()
      
      // Do NOT do this for keyboards; they're not yet installed and have a managed installer
      // yet to operate.
      if package.resourceType() == .lexicalModel {
        self.navigationController?.popToRootViewController(animated: true)
      }
    }
  }
  
  private func packageDownloadFailed(notification: PackageDownloadFailedNotification) {
    guard let packageKey = notification.packageKey else {
      return
    }
    
    os_log("keyboardDownloadFailed: InstalledLanguagesViewController", log:KeymanEngineLogger.resources, type: .info)
    
    DispatchQueue.main.async {
      var msg: String
      switch(packageKey.type) {
      case .keyboard:
        msg = NSLocalizedString("notification-download-failure-keyboard", bundle: engineBundle, comment: "")
      case .lexicalModel:
        msg = NSLocalizedString("notification-download-failure-lexical-model", bundle: engineBundle, comment: "")
      }
      
      if let toolbar = self.navigationController?.toolbar as? ResourceDownloadStatusToolbar {
        toolbar.displayStatus(msg, withIndicator: false, duration: 3.0)
      }
      
      self.restoreNavigation()
      
      self.navigationController?.popToRootViewController(animated: true)
    }
  }
  
  private func batchUpdateStarted(_: [AnyLanguageResource]) {
    os_log("batchUpdateStarted: InstalledLanguagesViewController", log:KeymanEngineLogger.resources, type: .info)
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)
    
    guard let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar else {
      return
    }
    
    toolbar.displayStatus(NSLocalizedString("notification-update-available", bundle: engineBundle, comment: ""), withIndicator: true)
  }
  
  private func batchUpdateCompleted(results: BatchUpdateCompletedNotification) {
    DispatchQueue.main.async {
      if let toolbar = self.navigationController?.toolbar as? ResourceDownloadStatusToolbar {
        if results.failures.count == 0 {
          let formatString = NSLocalizedString("notification-update-success", bundle: engineBundle, comment: "")
          toolbar.displayStatus(String.localizedStringWithFormat(formatString, results.successes.count), withIndicator: false, duration: 3.0)
        } else {
          // Needs to be a short string on iPhones.
          // Possible TODO:  condition on "is an iPad", provide more info on iPads?
          let formatString = NSLocalizedString("notification-update-failed", bundle: engineBundle, comment: "")
          toolbar.displayStatus(String.localizedStringWithFormat(formatString, results.failures.count), withIndicator: false, duration: 3.0)
        }
      }
      
      self.restoreNavigation()
    }
  }
  
  func showActivityView() {
    view.isUserInteractionEnabled = false
    let indicatorView = UIActivityIndicatorView(style: .whiteLarge)
    let activityView = UIView(frame: indicatorView.bounds.insetBy(dx: -10.0, dy: -10.0))
    activityView.backgroundColor = Colors.spinnerBackground
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
  
  func loadUserKeyboards() {
    userKeyboards = [:]
    guard let userKbList = Storage.active.userDefaults.userKeyboards else {
      return
    }
    
    for kb in userKbList {
      let dictKey = "\(kb.languageID)_\(kb.id)"
      userKeyboards[dictKey] = kb
    }
    tableView.reloadData()
  }
  
  func loadUserLexicalModels() {
    userLexicalModels = [:]
    guard let userLmList = Storage.active.userDefaults.userLexicalModels else {
      return
    }
    
    for lm in userLmList {
      let dictKey = "\(lm.languageID)_\(lm.id)"
      userLexicalModels[dictKey] = lm
    }
    tableView.reloadData()
  }
  
  private func showConnectionErrorAlert() {
    dismissActivityView()
    Alerts.showDownloadErrorAlert(in: self, handler: errorAcknowledgmentHandler)
  }
}

extension InstalledLanguagesViewController {
  
  public func launchKeyboardSearch() {
    let keyboardSearchVC = KeyboardSearchViewController(keyboardSelectionBlock: KeyboardSearchViewController.defaultDownloadClosure() { result in
      switch result {
      case .cancelled:
        break
      case .error(let error):
        // Note: Errors may result from network issues.
        if let error = error {
          let errorMessage = "\(String(describing: error))"
          os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
        }
      case .success(let package, let fullID):
        ResourceFileManager.shared.doInstallPrompt(for: package as! KeyboardKeymanPackage,
                                                   defaultLanguageCode: fullID.languageID,
                                                   in: self.navigationController!,
                                                   withAssociators: [.lexicalModels])
      }
    })
    
    navigationController!.pushViewController(keyboardSearchVC, animated: true)
  }
  
  @objc func addClicked(_ sender: Any) {
    launchKeyboardSearch()
  }
}

