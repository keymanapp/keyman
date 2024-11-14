//
//  SettingsViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 5/13/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import UIKit
import os.log

open class SettingsViewController: UITableViewController {
  private var itemsArray = [[String: String]]()
  private var userLanguages: [String: Language] = [:]

  override open func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    
    loadUserLanguages()
    os_log("viewWillAppear: SettingsViewController", log:KeymanEngineLogger.settings, type: .info)
 }
  
  override open func viewDidLoad() {
    super.viewDidLoad()
    
    title = NSLocalizedString("menu-settings-title", bundle: engineBundle, comment: "")
    let doneButton = UIBarButtonItem(title: NSLocalizedString("Done", bundle: engineBundle, comment: ""), style: .plain, target: self,
                                     action: #selector(self.doneClicked))
    navigationItem.leftBarButtonItem = doneButton

    navigationController?.toolbar?.barTintColor = Colors.statusToolbar
  }
  
  @objc func doneClicked(_ sender: Any) {
    // While the called method might should be renamed, it does the job well enough.
    // This resets KMW so that any new and/or updated resources can be properly loaded.
    Manager.shared.dismissKeyboardPicker(self)
  }
  
  open func launchSettings(launchingVC: UIViewController, sender: Any?) -> Void {
    let sb : UIStoryboard = UIStoryboard(name: "Settings", bundle: nil)
    if let vc = sb.instantiateInitialViewController() {
      launchingVC.present(vc, animated: true, completion: {
        os_log("presented settings", log:KeymanEngineLogger.settings, type: .info)
      })
    }
  }
//
//  convenience init() {
//    self.init(style: UITableViewStyle.grouped)
//  }
  
  public init(/*storage: Storage*/) {
//    self.storage = storage
    super.init(nibName: nil, bundle: nil)
    os_log("init settings", log:KeymanEngineLogger.settings, type: .default)

    itemsArray = [[String: String]]()
    itemsArray.append([
      "title": NSLocalizedString("menu-installed-languages-title", bundle: engineBundle, comment: ""),
      "subtitle": "0", //count of installed languages as string
      "reuseid" : "languages"
      ])
    
    itemsArray.append([
      "title": NSLocalizedString("menu-settings-startup-get-started", bundle: engineBundle, comment: ""),
      "subtitle": "",
      "reuseid" : "showgetstarted"
      ])

    itemsArray.append([
      "title": NSLocalizedString("menu-settings-error-report", bundle: engineBundle, comment: ""),
      "subtitle": NSLocalizedString("menu-settings-error-report-description", bundle: engineBundle, comment: ""),
      "reuseid": "enablecrashreporting"
      ])

    itemsArray.append([
      "title": NSLocalizedString("menu-settings-spacebar-text", bundle: engineBundle, comment: ""),
      "subtitle": "",
      "reuseid": "spacebartext"
      ])
    
    itemsArray.append([
      "title": NSLocalizedString("menu-settings-adjust-keyboard-height", bundle: engineBundle, comment: ""),
      "subtitle": "",
      "reuseid": "adjustkeyboardheight"
      ])
    
    if let _ = URL(string: UIApplication.openSettingsURLString) {
      itemsArray.append([
        "title": NSLocalizedString("menu-settings-system-keyboard-menu", bundle: engineBundle, comment: ""),
        "subtitle": "",
        "reuseid": "systemkeyboardsettings"
        ])
    }

    itemsArray.append([
      "title": NSLocalizedString("menu-settings-install-from-file", bundle: engineBundle, comment: ""),
      "subtitle": NSLocalizedString("menu-settings-install-from-file-description", bundle: engineBundle, comment: ""),
      "reuseid" : "installfile"
      ])

    #if DEBUG && !NO_SENTRY
            itemsArray.append([
      "title": "Force a crash",
      "subtitle": "Test Sentry error-reporting integration",
      "reuseid" : "forcederror"
      ])
    #endif

    _ = view
  }

  required public init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

    // MARK: - Table view data source

  override open func numberOfSections(in tableView: UITableView) -> Int {
        // #warning Incomplete implementation, return the number of sections
        return 1
    }

  override open func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return itemsArray.count
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

  override open func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = itemsArray[indexPath.row]["reuseid"]
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier!) {
      return cell
    }
    let cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    cell.selectionStyle = .none
    
    switch(cellIdentifier) {
      case "languages":
        break
      case "showgetstarted":
        let showAgainSwitch = UISwitch()
        showAgainSwitch.translatesAutoresizingMaskIntoConstraints = false
        
        let switchFrame = frameAtRightOfCell(cell: cell.frame, controlSize: showAgainSwitch.frame.size)
        showAgainSwitch.frame = switchFrame
        
        showAgainSwitch.isOn = showGetStarted
        showAgainSwitch.addTarget(self, action: #selector(self.showGetStartedSwitchValueChanged),
                                      for: .valueChanged)
        cell.addSubview(showAgainSwitch)
        cell.contentView.isUserInteractionEnabled = false

        showAgainSwitch.rightAnchor.constraint(equalTo: cell.layoutMarginsGuide.rightAnchor).isActive = true
        showAgainSwitch.centerYAnchor.constraint(equalTo: cell.layoutMarginsGuide.centerYAnchor).isActive = true
      case "enablecrashreporting":
        let enableReportingSwitch = UISwitch()
        enableReportingSwitch.translatesAutoresizingMaskIntoConstraints = false

        let switchFrame = frameAtRightOfCell(cell: cell.frame, controlSize: enableReportingSwitch.frame.size)
        enableReportingSwitch.frame = switchFrame

        enableReportingSwitch.isOn = SentryManager.enabled
        enableReportingSwitch.addTarget(self, action: #selector(self.reportingSwitchValueChanged),
                                      for: .valueChanged)
        cell.addSubview(enableReportingSwitch)
        cell.contentView.isUserInteractionEnabled = false

        enableReportingSwitch.rightAnchor.constraint(equalTo: cell.layoutMarginsGuide.rightAnchor).isActive = true
        enableReportingSwitch.centerYAnchor.constraint(equalTo: cell.layoutMarginsGuide.centerYAnchor).isActive = true
        
      case "systemkeyboardsettings", "installfile", "forcederror", "spacebartext", "adjustkeyboardheight":
        break
      default:
        let message = "unknown cellIdentifier(\"\(cellIdentifier ?? "EMPTY")\")"
        os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
        SentryManager.capture(message)
        cell.accessoryType = .none
    }
    
    return cell
  }

  @objc func reportingSwitchValueChanged(_ sender: Any) {
    if let toggle = sender as? UISwitch {
      // Propagate the effects
      SentryManager.enabled = toggle.isOn
    }
  }
  
  @objc func showGetStartedSwitchValueChanged(_ sender: Any) {
    let userData = Storage.active.userDefaults
    if let toggle = sender as? UISwitch {
      userData.set(toggle.isOn, forKey: Key.optShouldShowGetStarted)
      userData.synchronize()
    }
  }
  
  private var showGetStarted: Bool {
    let userData = Storage.active.userDefaults
    return userData.bool(forKey: Key.optShouldShowGetStarted)
  }

  override open func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    cell.accessoryType = .none

    // Remember, UITableViewCells may be reused, so we should always reset relevant properties.
    cell.textLabel?.text = itemsArray[indexPath.row]["title"]
    cell.detailTextLabel?.text = itemsArray[indexPath.row]["subtitle"]
    cell.textLabel?.isEnabled = true

    cell.tag = indexPath.row
    cell.isUserInteractionEnabled = true

    let cellIdentifier = itemsArray[indexPath.row]["reuseid"]

    switch (cellIdentifier) {
      case "languages", "installfile", "systemkeyboardsettings", "forcederror":
        cell.accessoryType = .disclosureIndicator
        cell.detailTextLabel?.isEnabled = true
      case "enablecrashreporting":
        cell.detailTextLabel?.isEnabled = true
        break
      case "spacebartext":
        cell.accessoryType = .disclosureIndicator
        cell.detailTextLabel?.text = NSLocalizedString("menu-settings-spacebar-hint-"+Manager.shared.spacebarText.rawValue, bundle: engineBundle, comment: "")
        cell.detailTextLabel?.isEnabled = true
        break
      case "adjustkeyboardheight":
        cell.accessoryType = .disclosureIndicator
        cell.detailTextLabel?.isEnabled = false
        break
      case "showbanner", "showgetstarted":
        cell.detailTextLabel?.isEnabled = false
      default:
        let message = "unknown cellIdentifier(\"\(cellIdentifier ?? "EMPTY")\")"
        os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
        SentryManager.capture(message)
    }
  }
  
  // In a xib-based application, navigation from a table can be handled in -tableView:didSelectRowAtIndexPath:
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
      let cellIdentifier = itemsArray[indexPath.row]["reuseid"]
      switch cellIdentifier {
        case "languages":
          showLanguages()
        case "systemkeyboardsettings":
          guard let appSettings = URL(string: UIApplication.openSettingsURLString) else {
            // It is an error if the option is displayed but unusable.  That's bad UI.
            let message = "Could not launch keyboard settings menu"
            os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
            SentryManager.capture(message)
            return
          }
          UniversalLinks.externalLinkLauncher?(appSettings)
        case "installfile":
          if let block = Manager.shared.fileBrowserLauncher {
            block(navigationController!)
          } else {
            let message = "Listener for framework signal to launch file browser is missing"
            os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
            SentryManager.capture(message)
          }
        case "forcederror":
          SentryManager.forceError()
        case "spacebartext":
          showSpacebarText()
        case "adjustkeyboardheight":
          showAdjustKeyboardHeight()
        default:
          break
      }
    default:
      break
    }
  }

    /*
    // Override to support conditional editing of the table view.
    override func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
        // Return false if you do not want the specified item to be editable.
        return true
    }
    */

    /*
    // Override to support editing the table view.
    override func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCell.EditingStyle, forRowAt indexPath: IndexPath) {
        if editingStyle == .delete {
            // Delete the row from the data source
            tableView.deleteRows(at: [indexPath], with: .fade)
        } else if editingStyle == .insert {
            // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
        }    
    }
    */

    /*
    // Override to support rearranging the table view.
    override func tableView(_ tableView: UITableView, moveRowAt fromIndexPath: IndexPath, to: IndexPath) {

    }
    */

    /*
    // Override to support conditional rearranging of the table view.
    override func tableView(_ tableView: UITableView, canMoveRowAt indexPath: IndexPath) -> Bool {
        // Return false if you do not want the item to be re-orderable.
        return true
    }
    */

    // MARK: - Navigation

    // In a storyboard-based application, you will often want to do a little preparation before navigation
  override open func prepare(for segue: UIStoryboardSegue, sender: Any?) {
    os_log("prepare for segue", log:KeymanEngineLogger.settings, type: .info)
    // Get the new view controller using segue.destination.
    // Pass the selected object to the new view controller.
  }
  
  // MARK: - language access -
  
  /** returns an array of LexicalModel created from an array of InstallableLexicalModel
   *  @param installedList: The InstallableLexicalModel are probably already installed
   *  The returned LexicalModels are not complete (usually we go the other way round)
   *    but sufficient for future API calls
   */
  public static func installed2API(_ installedList: [InstallableLexicalModel]) -> [LexicalModel] {
    var returnList = [LexicalModel]()
    for ilm in installedList {
      returnList.append(LexicalModel(id: ilm.id, name: ilm.name, license: "", version: ilm.version, languages: [], authorName: "", fileSize: 0, filename: "no filename", sourcePath: nil, authorEmail: nil, description: nil, packageFileSize: 0, packageFilename: "", packageIncludes: nil, isDefault: false, lastModified: nil, minKeymanVersion: nil))
    }
    return returnList
  }
  
  // MARK: - language access -
  private func loadUserLanguages() {
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
        os_log("%{public}s", log:KeymanEngineLogger.settings, type: .info, message)
      } else {
        // Legacy behavior:  we automatically install all MTNT language codes, even without
        // a matching keyboard for the more specific variant(s).
        let message = "lexical model language \(l) has no keyboard installed!"
        os_log("%{public}s", log:KeymanEngineLogger.settings, type: .info, message)
        SentryManager.breadcrumb(message)
      }
    }

    userLanguages = keyboardLanguages
    let formatString = NSLocalizedString("settings-languages-installed-count", bundle: engineBundle, comment: "")
    itemsArray[0]["subtitle"] = String.localizedStringWithFormat(formatString, userLanguages.count)
    tableView.reloadData()
  }
  
  public func setIsDoneButtonEnabled(_ nc: UINavigationController, _ value: Bool) {
    let doneOrCancel = value
    if doneOrCancel {
      let doneButton = UIBarButtonItem(title: NSLocalizedString("command-done", bundle: engineBundle, comment: ""), style: .plain, target: self,
                                       action: nil /* #selector(self.doneClicked) */ )
      nc.navigationItem.leftBarButtonItem = doneButton
    } else {
      let cancelButton = UIBarButtonItem(barButtonSystemItem: .cancel, target: self,
                                         action: nil /* #selector(self.cancelClicked) */ )
      nc.navigationItem.leftBarButtonItem = cancelButton
    }
  }

  func showLanguages() {
    let vc = InstalledLanguagesViewController(userLanguages)
    if let nc = navigationController {
      nc.pushViewController(vc, animated: true)
      setIsDoneButtonEnabled(nc, true)
    } else {
      let message = ("No navigation controller for showing languages???")
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(message)
    }
  }
  
  func showSpacebarText() {
    let vc = SpacebarTextViewController()
    if let nc = navigationController {
      nc.pushViewController(vc, animated: true)
      setIsDoneButtonEnabled(nc, true)
    } else {
      let message = ("No navigation controller for showing spacebarText options")
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(message)
    }
  }
  
  func showAdjustKeyboardHeight() {
    os_log("selected Adjust Keyboard Height", log:KeymanEngineLogger.settings, type: .default)
    let vc = KeyboardHeightViewController()
    if let nc = navigationController {
      nc.pushViewController(vc, animated: true)
       setIsDoneButtonEnabled(nc, true)
    } else {
      let message = ("No navigation controller for showing keyboard height view")
      os_log("%{public}s", log:KeymanEngineLogger.settings, type: .error, message)
      SentryManager.capture(message)
    }
  }
}
