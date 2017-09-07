//
//  BookmarksViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation
import UIKit

let webBrowserBookmarksKey: String = "KMWebBrowserBookmarks"
let bookmarkTitleKey: String = "BookmarkTitle"
let bookmarkUrlKey: String = "BookMarkUrl"

class BookmarksViewController:
UIViewController, UITableViewDelegate, UITableViewDataSource, UIAlertViewDelegate {
  weak var webBrowser: WebBrowserViewController?

  @IBOutlet var tableView: UITableView!
  @IBOutlet var doneButton: UIBarButtonItem!
  @IBOutlet var navBarTopConstraint: NSLayoutConstraint!
  // TODO: Create struct for bookmarks
  var bookmarks = [[String: String]]()

  override func viewDidLoad() {
    super.viewDidLoad()
    loadBookmarks()
    tableView.tableFooterView = UIView(frame: CGRect.zero)
  }

  override func viewWillAppear(_ animated: Bool) {
    super.viewWillAppear(animated)
    navBarTopConstraint.constant = AppDelegate.statusBarHeight()
    checkIfEmpty()
  }

  override func didRotate(from fromInterfaceOrientation: UIInterfaceOrientation) {
    navBarTopConstraint.constant = AppDelegate.statusBarHeight()
  }

  @IBAction func done(_ sender: Any) {
    dismiss(animated: true, completion: nil)
  }

  @IBAction func addBookmark(_ sender: Any) {
    let alert = UIAlertView(title: "Add Bookmark", message: "", delegate: self, cancelButtonTitle: "Cancel", otherButtonTitles: "Add")
    alert.alertViewStyle = .loginAndPasswordInput
    if let textField = alert.textField(at: 0) {
      textField.placeholder = "Title"
      textField.text = webBrowser?.webView?.stringByEvaluatingJavaScript(from: "document.title")
      textField.autocapitalizationType = .sentences
      textField.font = UIFont.systemFont(ofSize: 17)
      textField.keyboardType = .default
      textField.clearButtonMode = .whileEditing
    }
    if let textField = alert.textField(at: 1) {
      textField.placeholder = "Url"
      textField.text = webBrowser?.webView?.request?.mainDocumentURL?.absoluteString
      textField.autocapitalizationType = .none
      textField.font = UIFont.systemFont(ofSize: 17)
      textField.keyboardType = UIKeyboardType.URL
      textField.clearButtonMode = .whileEditing
      textField.isSecureTextEntry = false
    }
    alert.show()
  }

  func checkIfEmpty() {
    if tableView.numberOfSections == 0 {
      let label = UILabel(frame: tableView.frame)
      label.textAlignment = .center
      label.textColor = UIColor.lightGray
      label.font = UIFont.systemFont(ofSize: 14.0)
      label.text = "No bookmarks"
      tableView.backgroundView = label
    }
  }

  func loadBookmarks() {
    let userData = AppDelegate.activeUserDefaults()
    bookmarks = userData.object(forKey: webBrowserBookmarksKey) as? [[String: String]] ?? [[String: String]]()
  }

  func saveBookmarks() {
    let userData = AppDelegate.activeUserDefaults()
    if !bookmarks.isEmpty {
      userData.set(bookmarks, forKey: webBrowserBookmarksKey)
    }
    else {
      userData.set(nil, forKey: webBrowserBookmarksKey)
    }
    userData.synchronize()
  }

  // MARK: - Table view data source

  func numberOfSections(in tableView: UITableView) -> Int {
    // Return the number of sections.
    let sections = bookmarks.count
    if sections > 0 {
      self.tableView.backgroundView = nil
    }
    return sections
  }

  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int{
    // Return the number of rows in the section.
    return 1
  }

  func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    let cellIdentifier = "Cell"
    if let cell = tableView.dequeueReusableCell(withIdentifier: cellIdentifier) {
      return cell
    }
    let cell = UITableViewCell(style: .subtitle, reuseIdentifier: cellIdentifier)
    let selectionColor = UIView()
    selectionColor.backgroundColor = UIColor(red: 95.0 / 255.0, green: 196.0 / 255.0, blue: 217.0 / 255.0, alpha: 1.0)
    cell.selectedBackgroundView = selectionColor
    cell.textLabel?.font = cell.textLabel?.font?.withSize(12.0)
    cell.detailTextLabel?.font = cell.detailTextLabel?.font?.withSize(10.0)
    return cell
  }

  // MARK: - Table view delegate
  func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    let bookmark = bookmarks[indexPath.section]
    cell.textLabel?.text = bookmark[bookmarkTitleKey]
    cell.detailTextLabel?.text = bookmark[bookmarkUrlKey]
    cell.accessoryType = UITableViewCellAccessoryType.none
  }

  func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
    return true
  }

  func tableView(_ tableView: UITableView, commit editingStyle: UITableViewCellEditingStyle, forRowAt indexPath: IndexPath) {
    if editingStyle == .delete {
      bookmarks.remove(at: indexPath.section)
      saveBookmarks()
      self.tableView.reloadData()
      checkIfEmpty()
    }
  }

  func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    tableView.cellForRow(at: indexPath)?.isSelected = false
    performAction(for: indexPath)
  }

  func tableView(_ tableView: UITableView, accessoryButtonTappedForRowWith indexPath: IndexPath) {
    performAction(for: indexPath)
  }

  func performAction(for indexPath: IndexPath) {
    let bookmark = bookmarks[indexPath.section]
    if let urlString = bookmark[bookmarkUrlKey], let url = URL(string: urlString) {
      webBrowser?.webView?.loadRequest(URLRequest(url: url))
    }
    dismiss(animated: true, completion: nil)
  }

  // MARK: - Alert view delegate
  func alertView(_ alertView: UIAlertView, clickedButtonAt buttonIndex: Int) {
    if buttonIndex == alertView.cancelButtonIndex {
      return
    }
    guard let title = alertView.textField(at: 0)?.text, !title.isEmpty else {
      let alert = UIAlertView(title: "Invalid Bookmark Title", message: "Please enter a valid title", delegate: self, cancelButtonTitle: "OK", otherButtonTitles: "")
      alert.show()
      return
    }
    guard var urlString = alertView.textField(at: 1)?.text, let url = URL(string: urlString) else {
      let alert = UIAlertView(title: "Invalid Bookmark Url", message: "Please enter a valid Url", delegate: self, cancelButtonTitle: "OK", otherButtonTitles: "")
      alert.show()
      return
    }
    if url.scheme == nil {
      let url = URL(string: "http://\(urlString)")
      urlString = url?.absoluteString ?? urlString
    }
    let bookmark = [bookmarkTitleKey : title, bookmarkUrlKey : urlString]
    bookmarks.append(bookmark)
    saveBookmarks()
    tableView.reloadData()
  }
}
