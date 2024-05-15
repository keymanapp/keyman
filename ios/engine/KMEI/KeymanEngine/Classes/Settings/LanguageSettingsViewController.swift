//
//  LanguageSettingsViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 5/29/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import UIKit
import Sentry
import os.log

private let toolbarButtonTag = 100

class LanguageSettingsViewController: UITableViewController {
  let language: Language
  private var settingsArray = [[String: String]]()
  
  private var doPredictionsSwitch: UISwitch?
  private var doCorrectionsSwitch: UISwitch?
  private var doCorrectionsLabel: UILabel?
  private var correctionsCell: UITableViewCell?
  
  public init(_ inLanguage: Language) {
    language = inLanguage
    super.init(nibName: nil, bundle: nil)
  }
  
  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  open override func loadView() {
    super.loadView()
    tableView?.delegate = self
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()
    let titleFormat = NSLocalizedString("menu-langsettings-title", bundle: engineBundle, comment: "")
    title = String.localizedStringWithFormat(titleFormat, language.name)
    let message = "viewDidLoad: LanguageSettingsViewController title: \(title ?? "<empty>")"
    os_log("%{public}s", log:KeymanEngineLogger.ui, type: .info, message)
    
    if Manager.shared.canAddNewKeyboards {
      let addButton = UIBarButtonItem(barButtonSystemItem: .add, target: self,
                                      action: #selector(self.addClicked))
      navigationItem.rightBarButtonItem = addButton
    }
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    
    os_log("viewDidAppear: LanguageSettingsViewController", log:KeymanEngineLogger.ui, type: .info)
  }
  
  
  // MARK: - Table view data source UITableViewDataSource
  
  override func numberOfSections(in tableView: UITableView) -> Int {
    return 2
  }
  
  override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    if 0 == section {
      // so far as I know, there is always at least one keyboard or else we won't show
      let kbct = language.keyboards!.count
      return kbct
    } else {
      return 3  // 4 in future when we can manage a user dictionary
    }
  }
  
  public func frameAtRightOfCell(cell cellFrame: CGRect, controlSize: CGSize) -> CGRect {
    let rightOffset = cellFrame.size.width
    let switchWidth: CGFloat = 20
    let switchX = rightOffset - switchWidth
    let switchHeight = controlSize.height
    let cellSwitchHeightDiff = cellFrame.size.height - switchHeight
    let switchY = cellFrame.origin.y + 0.5 * cellSwitchHeightDiff
    
    let switchFrame = CGRect(x: switchX,
                             y: switchY,
                             width: switchWidth,
                             height: cellFrame.size.height)
    return switchFrame
  }
  
  @objc
  func predictionSwitchValueChanged(source: UISwitch) {
    let value = source.isOn;
    let userDefaults = Storage.active.userDefaults
    userDefaults.set(predictSetting: value, forLanguageID: self.language.id)
    
    // Reactively set the corrections switch interactivity state.
    self.doCorrectionsSwitch?.isHidden = !value
    self.doCorrectionsLabel?.isEnabled = value
    self.correctionsCell?.isUserInteractionEnabled = value
    
    if let lm = Manager.shared.preferredLexicalModel(userDefaults, forLanguage: self.language.id) {
      if Manager.shared.currentKeyboardID?.languageID == self.language.id {
        // re-register the model - that'll enact the settings.
        _ = Manager.shared.registerLexicalModel(lm)
      }
      // Based on how Manager chooses the model in setKeyboard.
    } else if let lm = userDefaults.userLexicalModels?.first(where: { $0.languageID == self.language.id }) {
      if Manager.shared.currentKeyboardID?.languageID == self.language.id {
        _ = Manager.shared.registerLexicalModel(lm)
      }
    }
  }
  
  @objc
  func correctionSwitchValueChanged(source: UISwitch) {
    let value = source.isOn;
    let userDefaults = Storage.active.userDefaults
    userDefaults.set(correctSetting: value, forLanguageID: self.language.id)
    
    if let lm = Manager.shared.preferredLexicalModel(userDefaults, forLanguage: self.language.id) {
      if Manager.shared.currentKeyboardID?.languageID == self.language.id {
        // re-register the model - that'll enact the settings.
        _ = Manager.shared.registerLexicalModel(lm)
      }
      // Based on how Manager chooses the model in setKeyboard.
    } else if let lm = userDefaults.userLexicalModels?.first(where: { $0.languageID == self.language.id }) {
      if Manager.shared.currentKeyboardID?.languageID == self.language.id {
        _ = Manager.shared.registerLexicalModel(lm)
      }
    }
  }
  
  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let userDefaults = Storage.active.userDefaults
    
    let cellIdentifier = 0 == indexPath.section ?  "KeyboardInLanguageSettingsCell" : "LanguageSettingsCell"
    
    let reusableCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier)
    if let cell = reusableCell {
      return cell
    }
    
    let cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    if 1 == indexPath.section {
      if 0 == indexPath.row {
        cell.accessoryType = .none
        doPredictionsSwitch = UISwitch()
        doPredictionsSwitch!.translatesAutoresizingMaskIntoConstraints = false
        
        let switchFrame = frameAtRightOfCell(cell: cell.frame, controlSize: doPredictionsSwitch!.frame.size)
        doPredictionsSwitch!.frame = switchFrame
        
        doPredictionsSwitch!.isOn = userDefaults.predictSettingForLanguage(languageID: self.language.id)
        doPredictionsSwitch!.addTarget(self, action: #selector(self.predictionSwitchValueChanged), for: .valueChanged)
        cell.addSubview(doPredictionsSwitch!)
        cell.contentView.isUserInteractionEnabled = false
        
        doPredictionsSwitch!.rightAnchor.constraint(equalTo: cell.layoutMarginsGuide.rightAnchor).isActive = true
        doPredictionsSwitch!.centerYAnchor.constraint(equalTo: cell.layoutMarginsGuide.centerYAnchor).isActive = true
      } else if 1 == indexPath.row {
        correctionsCell = cell
        cell.accessoryType = .none
        doCorrectionsSwitch = UISwitch()
        doCorrectionsSwitch!.translatesAutoresizingMaskIntoConstraints = false
        
        let switchFrame = frameAtRightOfCell(cell: cell.frame, controlSize: doCorrectionsSwitch!.frame.size)
        doCorrectionsSwitch!.frame = switchFrame
        
        doCorrectionsSwitch!.isOn = userDefaults.correctSettingForLanguage(languageID: self.language.id)
        doCorrectionsSwitch!.addTarget(self, action: #selector(self.correctionSwitchValueChanged), for: .valueChanged)
        cell.addSubview(doCorrectionsSwitch!)
        cell.contentView.isUserInteractionEnabled = false
        
        doCorrectionsSwitch!.rightAnchor.constraint(equalTo: cell.layoutMarginsGuide.rightAnchor).isActive = true
        doCorrectionsSwitch!.centerYAnchor.constraint(equalTo: cell.layoutMarginsGuide.centerYAnchor).isActive = true
        
        // Disable interactivity if the prediction toggle is set to 'off'.
        doCorrectionsSwitch!.isHidden = !userDefaults.predictSettingForLanguage(languageID: self.language.id)
        cell.isUserInteractionEnabled = userDefaults.predictSettingForLanguage(languageID: self.language.id)
      } else { // rows 3 and 4
        cell.accessoryType = .disclosureIndicator
      }
    }
    let selectionColor = UIView()
    selectionColor.backgroundColor = Colors.selectionPrimary
    cell.selectedBackgroundView = selectionColor
    cell.textLabel?.font = cell.textLabel?.font?.withSize(16.0)
    return cell
  }
  
  // MARK: - UITableViewDelegate
  
  // fixed font style. use custom view (UILabel) if you want something different
  override func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
    var title: String
    switch (section) {
    case 0:
      // We do NOT use a general-use "keyboards" entry because different languages have different
      // pluralization rules, and it's a major assumption to assume which is preferred for general use.
      title = NSLocalizedString("menu-langsettings-section-keyboards", bundle: engineBundle, comment: "")
    case 1:
      title = NSLocalizedString("menu-langsettings-section-settings", bundle: engineBundle, comment: "")
    default:
      title = "unknown header"
    }
    return title
  }
  
  override func tableView(_ tableView: UITableView, heightForHeaderInSection section: Int) -> CGFloat
  {
    if (section == 0 || section == 1)
    {
      return 32.0;
    }
    else
    {
      return CGFloat.leastNormalMagnitude;
    }
  }
  
  override func tableView(_ tableView: UITableView, heightForFooterInSection section: Int) -> CGFloat {
    return CGFloat.leastNonzeroMagnitude
  }
  
  override func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    
    cell.isUserInteractionEnabled = true
    cell.textLabel?.isEnabled = true
    cell.detailTextLabel?.isEnabled = true
    if 0 == indexPath.section { // keyboard list
      guard let keyboard = language.keyboards?[safe: indexPath.row] else {
        return
      }
      cell.textLabel?.text = keyboard.name
      cell.detailTextLabel?.text = keyboard.version + " " + keyboard.id
      
      cell.accessoryType = .disclosureIndicator
    } else { // language settings
      cell.accessoryType = .none
      switch indexPath.row {
      case 0:
        cell.textLabel?.text = NSLocalizedString("menu-langsettings-toggle-predict", bundle: engineBundle, comment: "")
      case 1:
        doCorrectionsLabel = cell.textLabel
        cell.textLabel?.text = NSLocalizedString("menu-langsettings-toggle-correct", bundle: engineBundle, comment: "")
        cell.textLabel?.isEnabled = !(doCorrectionsSwitch?.isHidden ?? false)
      case 2:
        cell.textLabel?.text = NSLocalizedString("menu-langsettings-label-lexical-models", bundle: engineBundle, comment: "")
        cell.accessoryType = .disclosureIndicator
        let modelCt = language.lexicalModels?.count ?? 0
        if modelCt != 1 {
          let formatString = NSLocalizedString("menu-langsettings-lexical-model-count", bundle: engineBundle, comment: "")
          cell.detailTextLabel?.text = String.localizedStringWithFormat(formatString, modelCt)
        } else {
          cell.detailTextLabel?.text = "\(language.lexicalModels![0].name)"
        }
        
      default:
        cell.textLabel?.text = NSLocalizedString("alert-error-title", bundle: engineBundle, comment: "")
      }
    }
  }
  
  override open func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    tableView.cellForRow(at: indexPath)?.isSelected = false
    performAction(for: indexPath)
  }
  
  override open func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: IndexPath) {
    performAction(for: indexPath)
  }
  
  override func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
    if !Manager.shared.canRemoveKeyboards {
      return false
    }
    
    // No deletion of the settings toggles!
    if indexPath.section != 0 {
      return false
    }
    
    // Will we have at least one keyboard left somewhere if this one is deleted?  (Even if not same language)
    if Storage.active.userDefaults.userKeyboards?.count ?? 0 > 1 {
      return true
    }
    
    return false
  }
  
  override func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCell.EditingStyle,
                          forRowAt indexPath: IndexPath) {
    if editingStyle == .delete {
      if let globalIndex = getKeyboardIndex(kb: (language.keyboards?[safe: indexPath.row])!) {
        if Manager.shared.removeKeyboard(at: globalIndex) {
          // For now, a pop-back will be sufficient.
          navigationController?.popToRootViewController(animated: true)
        }
      }
    }
    
    // Do nothing for now.
  }
  
  @objc func addClicked(_ sender: Any) {
    let keyboardSearchVC = KeyboardSearchViewController(languageCode: self.language.id,
                                                        keyboardSelectionBlock: KeyboardSearchViewController.defaultDownloadClosure() { result in
      switch result {
      case .cancelled:
        break
      case .error(let error):
        if let error = error {
          // Note: Errors may result from network issues.
          let errorMessage = "\(String(describing: error))"
          os_log("%{public}s", log:KeymanEngineLogger.ui, type: .error, errorMessage)
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
  
  
  private func performAction(for indexPath: IndexPath) {
    switch indexPath.section {
    case 0:
      showKeyboardInfoView(kb: (language.keyboards?[safe: indexPath.row])!)
    case 1:
      switch indexPath.row  {
        // case 0, 1:  the toggles - but a general 'click' not on the toggle itself.
      case 2:
        showLexicalModelsView()
      default:
        break
      }
    default:
      break
    }
  }
  
  func getKeyboardIndex(kb: Keyboard) -> Int? {
    let matchingFullID = FullKeyboardID(keyboardID: kb.id, languageID: language.id)
    let userData = Storage.active.userDefaults
    
    // If user defaults for keyboards list does not exist, do nothing.
    guard let globalUserKeyboards = userData.userKeyboards else {
      let message = "no keyboards in the global keyboards list!"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, message)
      SentryManager.capture(message)
      return nil
    }
    
    if let index = globalUserKeyboards.firstIndex(where: { $0.fullID == matchingFullID }) {
      guard index < globalUserKeyboards.count else {
        return nil
      }
      return index
    } else {
      let event = Sentry.Event(level: .debug)
      event.message = SentryMessage(formatted: "Keyboard index requested for uninstalled keyboard")
      event.extra = ["id": matchingFullID]
      SentrySDK.capture(event: event)
      
      let errorMessage = "this keyboard \(matchingFullID) not found among user's installed keyboards!"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
      return nil
    }
  }
  
  func showKeyboardInfoView(kb: Keyboard) {
    let matchingFullID = FullKeyboardID(keyboardID: kb.id, languageID: language.id)
    
    let userData = Storage.active.userDefaults
    
    // If user defaults for keyboards list does not exist, do nothing.
    guard let globalUserKeyboards = userData.userKeyboards else {
      let message = "No keyboards in the global keyboards list!"
      os_log("%{public}s", log: KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture("no keyboards in the global keyboards list!")
      return
    }
    
    if let index = getKeyboardIndex(kb: kb) {
      guard index < globalUserKeyboards.count else {
        return
      }
      let kbIndex:Int = index
      let thisKb = globalUserKeyboards[kbIndex]
      let mayDelete = mayDeleteKeyboard(keyboardIndex: index, keyboardCount: globalUserKeyboards.count)
      let infoView = ResourceInfoViewController(for: thisKb, mayDelete: mayDelete)
      navigationController?.pushViewController(infoView, animated: true)
    } else {
      let event = Sentry.Event(level: .debug)
      event.message = SentryMessage(formatted: "Keyboard index requested for uninstalled keyboard")
      event.extra = ["id": matchingFullID]
      SentrySDK.capture(event: event)
      
      let errorMessage = "this keyboard \(matchingFullID) not found among user's installed keyboards!"
      os_log("%{public}s", log:KeymanEngineLogger.resources, type: .error, errorMessage)
      return
    }
  }
  
  private func mayDeleteKeyboard(keyboardIndex: Int, keyboardCount: Int) -> Bool {
    if !Manager.shared.canRemoveKeyboards {
      return false
    }
    
    if !Manager.shared.canRemoveDefaultKeyboard {
      return keyboardIndex != 0
    }
    
    if keyboardIndex > 0 {
      return true
    }
    return keyboardCount > 1
  }
  
  func showLexicalModelsView() {
    //LanguageLexicalModelPickerViewController? (should show just the models for this language)
    let lmListView = LexicalModelPickerViewController(self.language)
    lmListView.language = self.language
    navigationController?.pushViewController(lmListView, animated: true)
  }
  
  /*
   // MARK: - Navigation
   
   // In a storyboard-based application, you will often want to do a little preparation before navigation
   override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
   // Get the new view controller using segue.destination.
   // Pass the selected object to the new view controller.
   }
   */
  
}
