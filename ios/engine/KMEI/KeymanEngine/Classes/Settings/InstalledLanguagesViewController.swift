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
  private let keyboardRepository: KeyboardRepository?
  private let lexicalModelRepository: LexicalModelRepository?
  
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
    self.keyboardRepository = Manager.shared.apiKeyboardRepository
    self.lexicalModelRepository = nil
    super.init(nibName: nil, bundle: nil)
//    keyboardRepository.delegate = self
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override public func loadView() {
    super.loadView()
    languages = languageList(installedLanguages)
  }
  
  override public func viewDidLoad() {
    super.viewDidLoad()
    
    title = "Installed Languages"
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
    
    log.info("viewDidLoad: InstalledLanguagesViewController (registered for keyboardDownloadStarted)")
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
    log.info("didAppear: InstalledLanguagesViewController")
    
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
      toolbar.displayButton("Update available", with: self, callback: #selector(self.updateClicked))
    }
  }
  
  @objc func updateClicked(_ sender: Any) {
    if let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar {
      toolbar.displayStatus("Updating\u{2026}", withIndicator: true)
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
        log.info("keyboard language \(l) \(langName) has lexical model")
      } else {
        log.error("lexical model language \(l) has no keyboard installed!")
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

      if #available(iOSApplicationExtension 11.0, *) {
        selectionColor.backgroundColor = UIColor(named: "SelectionPrimary")
      } else {
        selectionColor.backgroundColor = Colors.selectionPrimary
      }

      if keyboards.count < 2 {
        cell = KeyboardNameTableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
      } else {
        cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
      }
      cell.selectedBackgroundView = selectionColor
      cell.accessoryType = .disclosureIndicator
    }
    cell.detailTextLabel?.text = keyboards.count < 2 ? (keyboards.first?.name ?? "") : "\(keyboards.count) installed"
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
    let langSettingsView = LanguageSettingsViewController(keyboardRepository, languages[languageIndex])
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
  
  private func showDownloading(_ downloadLabel: String) {
    guard let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar else {
      return
    }
    
    toolbar.displayStatus("Downloading \(downloadLabel)\u{2026}", withIndicator: true)
  }

  private func packageDownloadStarted(key: KeymanPackage.Key) {
    switch(key.type) {
      case .keyboard:
        keyboardDownloadStarted()
      case .lexicalModel:
        lexicalModelDownloadStarted()
    }
  }

  private func packageDownloadCompleted(package: KeymanPackage) {
    switch(package.resourceType()) {
      case .keyboard:
        keyboardDownloadCompleted()
      case .lexicalModel:
        lexicalModelDownloadCompleted()
    }
  }

  private func packageDownloadFailed(notification: PackageDownloadFailedNotification) {
    guard let packageKey = notification.packageKey else {
      return
    }

    switch(packageKey.type) {
      case .keyboard:
        keyboardDownloadFailed()
      case .lexicalModel:
        lexicalModelDownloadFailed()
    }
  }
  
  private func keyboardDownloadStarted() {
    log.info("keyboardDownloadStarted: InstalledLanguagesViewController")
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)
    showDownloading("keyboard")
  }
  
  private func lexicalModelDownloadStarted() {
    log.info("lexicalModelDownloadStarted")
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)
    showDownloading("dictionary")
  }
  
  private func keyboardDownloadCompleted() {
    log.info("keyboardDownloadCompleted: InstalledLanguagesViewController")
    Manager.shared.shouldReloadKeyboard = true
    
    if let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar {
      toolbar.displayStatus("Keyboard successfully downloaded!", withIndicator: false, duration: 3.0)
    }
    restoreNavigation()
    
    navigationController?.popToRootViewController(animated: true)
  }
  
  private func lexicalModelDownloadCompleted() {
    log.info("lexicalModelDownloadCompleted: InstalledLanguagesViewController")
    
    if let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar {
      toolbar.displayStatus("Dictionary successfully downloaded!", withIndicator: false, duration: 3.0)
    }
    
    restoreNavigation()
    navigationController?.popToRootViewController(animated: true)
  }
  
  private func keyboardDownloadFailed() {
    log.info("keyboardDownloadFailed: InstalledLanguagesViewController")
    restoreNavigation()
  }
  
  private func lexicalModelDownloadFailed() {
    log.info("lexicalModelDownloadFailed: InstalledLanguagesViewController")
    restoreNavigation()
    
    let title = "Dictionary Download Error"
    navigationController?.setToolbarHidden(true, animated: true)
    
    let alertController = UIAlertController(title: title, message: "",
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertAction.Style.cancel,
                                            handler: nil))
    
    self.present(alertController, animated: true, completion: nil)
  }
  
  private func batchUpdateStarted(_: [AnyLanguageResource]) {
    log.info("batchUpdateStarted: InstalledLanguagesViewController")
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)
    
    guard let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar else {
      return
    }
    
    toolbar.displayStatus("Updating\u{2026}", withIndicator: true)
  }

  private func batchUpdateCompleted(results: BatchUpdateCompletedNotification) {
    if let toolbar = navigationController?.toolbar as? ResourceDownloadStatusToolbar {
      if results.failures.count == 0 {
        toolbar.displayStatus("\(results.successes.count) update(s) successfully downloaded!", withIndicator: false, duration: 3.0)
      } else {
        toolbar.displayStatus("Updates complete: \(results.successes.count) successful, \(results.failures.count) failed", withIndicator: false, duration: 3.0)
      }
    }
    
    restoreNavigation()
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
    let alertController = UIAlertController(title: "Connection Error",
                                            message: "Could not reach Keyman server. Please try again later.",
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertAction.Style.default,
                                            handler: errorAcknowledgmentHandler))
    
    self.present(alertController, animated: true, completion: nil)
  }
}

// MARK: - KeyboardRepositoryDelegate
extension InstalledLanguagesViewController: KeyboardRepositoryDelegate {
  public func keyboardRepositoryDidFetch(_ repository: KeyboardRepository) {
    if let languageDict = repository.languages {
      languages = languageList(languageDict)
    }
    self.dismissActivityView()
    self.tableView.reloadData()
    if self.numberOfSections(in: self.tableView) == 0 {
      self.showConnectionErrorAlert()
    }
  }
  
  public func keyboardRepository(_ repository: KeyboardRepository, didFailFetch error: Error) {
    dismissActivityView()
    showConnectionErrorAlert()
  }
  
  private func languageList(_ languageDict: [String: Language]) -> [Language] {
    return languageDict.values.sorted { a, b -> Bool in
      a.name.localizedCaseInsensitiveCompare(b.name) == .orderedAscending
    }
  }
}

extension InstalledLanguagesViewController {
  
  @objc func addClicked(_ sender: Any) {
    let keyboardSearchVC = KeyboardSearchViewController(keyboardSelectionBlock: KeyboardSearchViewController.defaultKeyboardInstallationClosure(), lexicalModelSelectionBlock: KeyboardSearchViewController.defaultLexicalModelInstallationClosure())
    navigationController!.pushViewController(keyboardSearchVC, animated: true)
  }
}

