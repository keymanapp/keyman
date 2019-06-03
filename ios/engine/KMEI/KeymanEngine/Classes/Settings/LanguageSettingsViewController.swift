//
//  LanguageSettingsViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 5/29/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import UIKit

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

  override func viewDidLoad() {
    super.viewDidLoad()
    title = "\(language.name) Settings"

        // Uncomment the following line to preserve selection between presentations
        // self.clearsSelectionOnViewWillAppear = false

        // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
        // self.navigationItem.rightBarButtonItem = self.editButtonItem
  }

    // MARK: - Table view data source

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 2
    }

    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
      if 0 == section {
        // so far as I know, there is always at least one keyboard or else we won't show
        let kbct = language.keyboards!.count
        return kbct
      } else {
        return 3
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
          } else { // row 3
            cell.accessoryType = .disclosureIndicator
        }
      }
      let selectionColor = UIView()
      selectionColor.backgroundColor = UIColor(red: 95.0 / 255.0, green: 196.0 / 255.0, blue: 217.0 / 255.0, alpha: 1.0)
      cell.selectedBackgroundView = selectionColor
      cell.textLabel?.font = cell.textLabel?.font?.withSize(12.0)
      return cell
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
          cell.detailTextLabel?.text = language.lexicalModels?[safe: 0]?.name

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
  
  private func performAction(for indexPath: IndexPath) {
    switch indexPath.section {
    case 0:
      showKeyboardInfoView(kb: (language.keyboards?[safe: indexPath.row])!)
    default:
      break
    }
  }
  
  func showKeyboardInfoView(kb: Keyboard) {
    let version = kb.version
    
    let infoView = KeyboardInfoViewController()
    infoView.title = kb.name
    infoView.keyboardCount = userKeyboards.count
//    infoView.keyboardIndex = index
    infoView.keyboardID = kb.id
//    infoView.languageID = kb.languageID
    infoView.keyboardVersion = version
//    infoView.isCustomKeyboard = kb.isCustom
    navigationController?.pushViewController(infoView, animated: true)
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
