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

class InstalledLanguagesViewController: UITableViewController, UIAlertViewDelegate {
  private var userKeyboards: [String: InstallableKeyboard] = [:]
  private var userLexicalModels: [String: InstallableLexicalModel] = [:]
  private var sectionIndexTitles: [String] = []
  private var indices: [Int] = []
  private var selectedSection = 0
  private var installedLanguages: [String: Language]
  private var languages: [Language] = []
  private let keyboardRepository: KeyboardRepository?
  private let lexicalModelRepository: LexicalModelRepository?
  
  private var keyboardDownloadStartedObserver: NotificationObserver?
  private var keyboardDownloadCompletedObserver: NotificationObserver?
  private var keyboardDownloadFailedObserver: NotificationObserver?
  private var lexicalModelDownloadStartedObserver: NotificationObserver?
  private var lexicalModelDownloadCompletedObserver: NotificationObserver?
  private var lexicalModelDownloadFailedObserver: NotificationObserver?

  init(_ givenLanguages: [String: Language]) {
    self.installedLanguages = givenLanguages
    self.keyboardRepository = nil
    self.lexicalModelRepository = nil
    super.init(nibName: nil, bundle: nil)
//    keyboardRepository.delegate = self
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func loadView() {
    super.loadView()
    languages = languageList(installedLanguages)
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()
    title = "Installed Languages"
    selectedSection = NSNotFound
    keyboardDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadStarted,
      observer: self,
      function: InstalledLanguagesViewController.keyboardDownloadStarted)
    keyboardDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadCompleted,
      observer: self,
      function: InstalledLanguagesViewController.keyboardDownloadCompleted)
    keyboardDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.keyboardDownloadFailed,
      observer: self,
      function: InstalledLanguagesViewController.keyboardDownloadFailed)
    
    lexicalModelDownloadStartedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadStarted,
      observer: self,
      function: InstalledLanguagesViewController.lexicalModelDownloadStarted)
    lexicalModelDownloadCompletedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadCompleted,
      observer: self,
      function: InstalledLanguagesViewController.lexicalModelDownloadCompleted)
    lexicalModelDownloadFailedObserver = NotificationCenter.default.addObserver(
      forName: Notifications.lexicalModelDownloadFailed,
      observer: self,
      function: InstalledLanguagesViewController.lexicalModelDownloadFailed)
    
    if Manager.shared.canAddNewKeyboards {
      let addButton = UIBarButtonItem(barButtonSystemItem: .add, target: self,
                                      action: #selector(self.addClicked))
      navigationItem.rightBarButtonItem = addButton
    }
    
    log.info("viewDidLoad: InstalledLanguagesViewController (registered for keyboardDownloadStarted)")
  }
  
  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    loadUserKeyboards()
    loadUserLexicalModels()
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    navigationController?.setToolbarHidden(true, animated: true)
    // if no rows to show yet, show a loading indicator
    if numberOfSections(in: tableView) == 0 {
      showActivityView()
    }
    log.info("didAppear: InstalledLanguagesViewController")
  }
  
  override func numberOfSections(in tableView: UITableView) -> Int {
    return languages.count
  }
  
  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return 1
  }
  
  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifierType1 = "CellType1"
    let cellIdentifierType2 = "CellType2"
    let keyboards = languages[indexPath.section].keyboards!
    let cellIdentifier = (keyboards.count < 2) ? cellIdentifierType1 : cellIdentifierType2
    let cell: UITableViewCell
    if let reusedCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      cell = reusedCell
    } else {
      let selectionColor = UIView()
      selectionColor.backgroundColor = UIColor(red: 204.0 / 255.0, green: 136.0 / 255.0,
                                               blue: 34.0 / 255.0, alpha: 1.0)
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
  override func sectionIndexTitles(for tableView: UITableView) -> [String] {
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
  
  override func tableView(_ tableView: UITableView, sectionForSectionIndexTitle title: String, at index: Int) -> Int {
    return indices[index]
  }
  
  override func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    guard let language = languages[safe: indexPath.section] else {
      return
    }
    
    cell.textLabel?.text = language.name
  }
  
  override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    selectedSection = indexPath.section
    tableView.cellForRow(at: indexPath)?.isSelected = false
    let title = tableView.cellForRow(at: indexPath)?.textLabel?.text ?? ""
    showLanguageSettingsView(title: title, languageIndex: indexPath.section)
  }
  
  override func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: IndexPath) {
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
  
  func downloadHandler(_ keyboardIndex: Int) {
    let language = languages[selectedSection]
    let keyboard = language.keyboards![keyboardIndex]
    Manager.shared.downloadKeyboard(withID: keyboard.id, languageID: language.id, isUpdate: false)
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
    guard let toolbar = navigationController?.toolbar else {
      return
    }
    
    let labelFrame = CGRect(origin: toolbar.frame.origin,
                            size: CGSize(width: toolbar.frame.width * 0.95,
                                         height: toolbar.frame.height * 0.7))
    let label = UILabel(frame: labelFrame)
    label.backgroundColor = UIColor.clear
    label.textColor = UIColor.white
    label.textAlignment = .center
    label.center = CGPoint(x: toolbar.frame.width * 0.5, y: toolbar.frame.height * 0.5)
    label.text = "Downloading \(downloadLabel)\u{2026}"
    label.autoresizingMask = [.flexibleLeftMargin, .flexibleRightMargin, .flexibleTopMargin,
                              .flexibleBottomMargin, .flexibleWidth, .flexibleHeight]
    label.tag = toolbarLabelTag
    
    let indicatorView = UIActivityIndicatorView(activityIndicatorStyle: .gray)
    indicatorView.center = CGPoint(x: toolbar.frame.width - indicatorView.frame.width,
                                   y: toolbar.frame.height * 0.5)
    indicatorView.autoresizingMask = [.flexibleLeftMargin, .flexibleTopMargin, .flexibleBottomMargin]
    indicatorView.tag = toolbarActivityIndicatorTag
    indicatorView.startAnimating()
    toolbar.viewWithTag(toolbarButtonTag)?.removeFromSuperview()
    toolbar.viewWithTag(toolbarLabelTag)?.removeFromSuperview()
    toolbar.viewWithTag(toolbarActivityIndicatorTag)?.removeFromSuperview()
    toolbar.addSubview(label)
    toolbar.addSubview(indicatorView)
    navigationController?.setToolbarHidden(false, animated: true)
  }
  
  private func hideDownloading() {
    navigationController?.toolbar?.viewWithTag(toolbarActivityIndicatorTag)?.removeFromSuperview()
    navigationController?.toolbar?.viewWithTag(toolbarLabelTag)?.removeFromSuperview()
    Timer.scheduledTimer(timeInterval: 3.0, target: self, selector: #selector(self.hideToolbarDelayed),
                         userInfo: nil, repeats: false)
  }
  
  private func keyboardDownloadStarted() {
    log.info("keyboardDownloadStarted: InstalledLanguagesViewController")
    view.isUserInteractionEnabled = false
    navigationItem.setHidesBackButton(true, animated: true)
    showDownloading("keyboard")
  }
  
  private func lexicalModelDownloadStarted(_ lexicalModels: [InstallableLexicalModel]) {
    log.info("lexicalModelDownloadStarted")
    showDownloading("dictionary")
  }
  
  private func keyboardDownloadCompleted(_ keyboards: [InstallableKeyboard]) {
    log.info("keyboardDownloadCompleted: InstalledLanguagesViewController")
    Manager.shared.shouldReloadKeyboard = true
    
    // Update keyboard version
    for keyboard in keyboards {
      Manager.shared.updateUserKeyboards(with: keyboard)
    }
    let label = navigationController?.toolbar?.viewWithTag(toolbarLabelTag) as? UILabel
    label?.text = "Keyboard successfully downloaded!"
    hideDownloading()
    restoreNavigation()
    
    // Add keyboard.
    for keyboard in keyboards {
      Manager.shared.addKeyboard(keyboard)
      _ = Manager.shared.setKeyboard(keyboard)
    }
    
    navigationController?.popToRootViewController(animated: true)
  }
  
  private func lexicalModelDownloadCompleted(_ lexicalModels: [InstallableLexicalModel]) {
    log.info("lexicalModelDownloadCompleted: InstalledLanguagesViewController")
    // Add models.
    for lexicalModel in lexicalModels {
      Manager.shared.addLexicalModel(lexicalModel)
      _ = Manager.shared.registerLexicalModel(lexicalModel)
    }
    hideDownloading()
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
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertActionStyle.cancel,
                                            handler: nil))
    
    self.present(alertController, animated: true, completion: nil)
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
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertActionStyle.default,
                                            handler: errorAcknowledgmentHandler))
    
    self.present(alertController, animated: true, completion: nil)
  }
}

// MARK: - KeyboardRepositoryDelegate
extension InstalledLanguagesViewController: KeyboardRepositoryDelegate {
  func keyboardRepositoryDidFetch(_ repository: KeyboardRepository) {
    if let languageDict = repository.languages {
      languages = languageList(languageDict)
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

// MARK: - LexicalModelRepositoryDelegate

extension InstalledLanguagesViewController: LexicalModelRepositoryDelegate {
  /** lexicalModelRepositoryDidFetchList callback on successful fetching of list of lexical models for a language
  *  caller should wrap in DispatchQueue.main.async {} if not already on main thread
  */
  func lexicalModelRepositoryDidFetchList(_ repository: LexicalModelRepository) {
    if let languageDict = repository.languages {
      languages = languageList(languageDict)
    }
    self.dismissActivityView()
    self.tableView.reloadData()
    if self.numberOfSections(in: self.tableView) == 0 {
      self.showConnectionErrorAlert()
    }
  }
  
  /** lexicalModelRepository didFailFetch callback on failure to fetch list of lexical models for a language
   *  caller should wrap in DispatchQueue.main.async {} if not already on main thread
   */
  func lexicalModelRepository(_ repository: LexicalModelRepository, didFailFetch error: Error) {
    dismissActivityView()
    showConnectionErrorAlert()
  }
  
  @objc func hideToolbarDelayed(_ timer: Timer) {
    navigationController?.setToolbarHidden(true, animated: true)
  }
  
  @objc func addClicked(_ sender: Any) {
    showAddKeyboard()
  }
  
  func showAddKeyboard() {
    let button: UIButton? = (navigationController?.toolbar?.viewWithTag(toolbarButtonTag) as? UIButton)
    button?.isEnabled = false
    let vc = LanguageViewController(keyboardRep: Manager.shared.apiKeyboardRepository, modelRep: Manager.shared.apiLexicalModelRepository)
    navigationController?.pushViewController(vc, animated: true)
  }
}

