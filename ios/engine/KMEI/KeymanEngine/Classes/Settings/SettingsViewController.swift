//
//  SettingsViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 5/13/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import UIKit

open class SettingsViewController: UITableViewController {

  override open func viewDidLoad() {
        super.viewDidLoad()

        // Uncomment the following line to preserve selection between presentations
        // self.clearsSelectionOnViewWillAppear = false

        // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
        // self.navigationItem.rightBarButtonItem = self.editButtonItem
    }

    // MARK: - Table view data source

  override open func numberOfSections(in tableView: UITableView) -> Int {
        // #warning Incomplete implementation, return the number of sections
        return 1
    }

  override open func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        // #warning Incomplete implementation, return the number of rows
        return 3
    }

  override open func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    var reuseID: String = "foo"
    let lastIndex = indexPath.index(before: indexPath.endIndex)
    let rowType = indexPath[lastIndex]
    switch(rowType) {
      case 0:
        reuseID = "languages"
      case 1:
        reuseID = "showbanner"
      case 2:
        reuseID = "showgetstarted"
      default:
        log.error("undefined indexPath(\rowType")
        reuseID = "foo"
    }
    if let cell = tableView.dequeueReusableCell(withIdentifier: reuseID) {
      return cell
    }
    
    let cell = UITableViewCell(style: .default, reuseIdentifier: reuseID)
    
    // Configure the cell...
    let selectionColor = UIView()
    selectionColor.backgroundColor = UIColor(red: 74.0 / 255.0, green: 186.0 / 255.0, blue: 208.0 / 255.0, alpha: 1.0)
    cell.selectedBackgroundView = selectionColor
    switch(rowType) {
      case 0:
        cell.textLabel?.text = "Installed Languages"
        cell.accessoryType = .disclosureIndicator
      case 1:
        cell.textLabel?.text = "Show Banner"
      case 2:
        cell.textLabel?.text = "Show 'Get Started' at startup'"
      default:
        log.error("undefined indexPath(\rowType")
    }

    return cell
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
      log.info("prepare for segue")
        // Get the new view controller using segue.destination.
        // Pass the selected object to the new view controller.
    }

}
