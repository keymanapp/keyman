//
//  LanguageSettingsViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 5/29/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import UIKit

private let toolbarButtonTag = 100

class LanguageSettingsViewController: UITableViewController {
  let language: Language
  private var userKeyboards: [String: Language] = [:]
  private var settingsArray = [[String: String]]()

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
    title = "\(language.name) Settings"
    log.info("viewDidLoad: LanguageSettingsViewController title: \(title ?? "<empty>")")

    if Manager.shared.canAddNewKeyboards {
      let addButton = UIBarButtonItem(barButtonSystemItem: .add, target: self,
                                      action: #selector(self.addClicked))
      navigationItem.rightBarButtonItem = addButton
    }
  }
  
  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)

    log.info("didAppear: LanguageSettingsViewController")
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

  override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = 0 == indexPath.section ?  "KeyboardInLanguageSettingsCell" : "LanguageSettingsCell"
    
    let reusableCell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier)
    if let cell = reusableCell {
      return cell
    }
    
    let cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    if 1 == indexPath.section {
        //TODO: find the settings these are to show
        if 0 == indexPath.row {
          cell.accessoryType = .none
          let doPredictionsSwitch = UISwitch()
          let switchFrame = frameAtRightOfCell(cell: cell.frame, controlSize: doPredictionsSwitch.frame.size)
          doPredictionsSwitch.frame = switchFrame
          doPredictionsSwitch.isOn = false
          //            showBannerSwitch.addTarget(self, action: #selector(self.predictionSwitchValueChanged), for: .valueChanged)
          cell.addSubview(doPredictionsSwitch)
        } else if 1 == indexPath.row {
          cell.accessoryType = .none
          let doCorrectionsSwitch = UISwitch()
          let switchFrame = frameAtRightOfCell(cell: cell.frame, controlSize: doCorrectionsSwitch.frame.size)
          doCorrectionsSwitch.frame = switchFrame
          doCorrectionsSwitch.isOn = false
          //            showBannerSwitch.addTarget(self, action: #selector(self.correctionSwitchValueChanged), for: .valueChanged)
          cell.addSubview(doCorrectionsSwitch)
        } else { // rows 3 and 4
          cell.accessoryType = .disclosureIndicator
      }
    }
    let selectionColor = UIView()
    selectionColor.backgroundColor = UIColor(red: 95.0 / 255.0, green: 196.0 / 255.0, blue: 217.0 / 255.0, alpha: 1.0)
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
      title = "keyboards"
    case 1:
      title = "language settings"
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
          cell.textLabel?.text = "Enable corrections"
        case 1:
          cell.textLabel?.text = "Enable predictions"
        case 2:
          cell.textLabel?.text = "Model"
          cell.accessoryType = .disclosureIndicator
          if let modelCt = language.lexicalModels?.count {
            switch modelCt {
            case 0:
              cell.detailTextLabel?.text = "no models installed"
            case 1:
              cell.detailTextLabel?.text = "one model installed"
            default:
              cell.detailTextLabel?.text = "\(modelCt) models installed"
            }
          } else {
            cell.detailTextLabel?.text = "no models installed"
          }
        case 3: // future
          cell.textLabel?.text = "Manage dictionary"
          cell.accessoryType = .disclosureIndicator
          cell.isUserInteractionEnabled = false

        default:
          cell.textLabel?.text = "error"
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
  
  @objc func addClicked(_ sender: Any) {
    showAddLanguageKeyboard()
  }
  
  func showAddLanguageKeyboard() {
    let button: UIButton? = (navigationController?.toolbar?.viewWithTag(toolbarButtonTag) as? UIButton)
    button?.isEnabled = false
    let vc = LanguageSpecificViewController(Manager.shared.apiKeyboardRepository, language: language)
    vc.title = "Add new \(language.name) keyboard"
    navigationController?.pushViewController(vc, animated: true)
  }
  
  private func performAction(for indexPath: IndexPath) {
    switch indexPath.section {
    case 0:
      showKeyboardInfoView(kb: (language.keyboards?[safe: indexPath.row])!)
    case 1:
      switch indexPath.row  {
        case 2:
          showLexicalModelsView()
        default:
          break
      }
    default:
      break
    }
  }
  
  func showKeyboardInfoView(kb: Keyboard) {
    let version = kb.version
    let matchingFullID = FullKeyboardID(keyboardID: kb.id, languageID: language.id)

    let userData = Storage.active.userDefaults

    // If user defaults for keyboards list does not exist, do nothing.
    guard var globalUserKeyboards = userData.userKeyboards else {
      log.error("no keyboards in the global keyboards list!")
      return
    }

    if let index = globalUserKeyboards.index(where: { $0.fullID == matchingFullID }) {
      guard index < globalUserKeyboards.count else {
        return
      }
      let kbIndex:Int = index
      let thisKb = globalUserKeyboards[kbIndex]
      let infoView = KeyboardInfoViewController()
      infoView.title = thisKb.name
      infoView.keyboardCount = userKeyboards.count
      infoView.keyboardIndex = index
      infoView.keyboardID = thisKb.id
      infoView.languageID = language.id
      infoView.keyboardVersion = version
      infoView.isCustomKeyboard = thisKb.isCustom
      navigationController?.pushViewController(infoView, animated: true)
    } else {
      log.error("this keyboard \(matchingFullID) not found among user's installed keyboards!")
      return
    }
  }
  
  func showLexicalModelsView() {
    //LanguageLexicalModelPickerViewController? (should show just the models for this language)
    let lmListView = LexicalModelPickerViewController(self.language)
    lmListView.language = self.language
    navigationController?.pushViewController(lmListView, animated: true)
 }
  
  func showLexicalModelInfoView() {
    if let lm = language.lexicalModels?[safe: 0] {
      let version = lm.version
      let matchingFullID = FullLexicalModelID(lexicalModelID: lm.id, languageID: language.id)
      
      let userData = Storage.active.userDefaults
      
      if let globalUserLexicalModels = userData.userLexicalModels {
        if let index = globalUserLexicalModels.index(where: { $0.fullID == matchingFullID }) {
          guard index < globalUserLexicalModels.count else {
            return
          }
          let lmIndex:Int = index
          let thisLm = globalUserLexicalModels[lmIndex]
          let infoView = LexicalModelInfoViewController()
          infoView.title = thisLm.name
          infoView.lexicalModelCount = globalUserLexicalModels.count
          infoView.lexicalModelIndex = index
          infoView.lexicalModelID = thisLm.id
          infoView.languageID = language.id
          infoView.lexicalModelVersion = version ?? InstallableConstants.defaultVersion
          infoView.isCustomLexicalModel = thisLm.isCustom
          navigationController?.pushViewController(infoView, animated: true)
        } else {
          log.error("this lexical model \(matchingFullID) not found among language's installed lexical model!")
        }
      } else {
        log.error("no lexical models in the global models list!")
      }
    }
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
