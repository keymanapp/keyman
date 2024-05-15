//
//  BookmarksViewController.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit
import KeymanEngine // contains Colors definition.

private let webBrowserBookmarksKey = "KMWebBrowserBookmarks"
private let bookmarkTitleKey = "BookmarkTitle"
private let bookmarkUrlKey = "BookMarkUrl"

class BookmarksViewController: UIViewController, UITableViewDelegate, UITableViewDataSource, UIAlertViewDelegate {
  weak var webBrowser: WebBrowserViewController?
  
  @IBOutlet var tableView: UITableView!
  @IBOutlet var doneButton: UIBarButtonItem!
  @IBOutlet var navBarTopConstraint: NSLayoutConstraint!
  @IBOutlet weak var navBar: UINavigationBar!
  // TODO: Create struct for bookmarks
  private var bookmarks = [[String: String]]()
  
  weak var inputTitleField: UITextField?
  weak var inputUrlField: UITextField?
  weak var addAction: UIAlertAction?
  
  convenience init() {
    self.init(nibName: "BookmarksViewController", bundle: nil)
  }
  
  override func viewDidLoad() {
    super.viewDidLoad()
    loadBookmarks()
    tableView.tableFooterView = UIView(frame: CGRect.zero)
    navBar.topItem?.title = NSLocalizedString("browser-bookmarks-title", comment: "")
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
    let alertController = UIAlertController(title: NSLocalizedString("browser-bookmarks-add-title", comment: ""),
                                            message: "",
                                            preferredStyle: UIAlertController.Style.alert)
    alertController.addTextField(configurationHandler: { (textField) in
      // listen for changes
      NotificationCenter.default.addObserver(self,
                                             selector: #selector(BookmarksViewController.handleTextFieldTextChangedNotification(notification:)),
                                             name: UITextField.textDidChangeNotification, object: textField)
      textField.placeholder = "Title"
      self.webBrowser?.webView?.evaluateJavaScript("document.title") { val, _ in
        if let str = val as? String {
          textField.text = str
          self.updateAddButtonEnableState()
        }
      }
      textField.autocapitalizationType = .sentences
      textField.font = UIFont.systemFont(ofSize: 17)
      textField.keyboardType = .default
      textField.clearButtonMode = .whileEditing
      self.inputTitleField = textField
    })
    alertController.addTextField(configurationHandler: { (textField) in
      // listen for changes
      NotificationCenter.default.addObserver(self,
                                             selector: #selector(BookmarksViewController.handleTextFieldTextChangedNotification(notification:)),
                                             name: UITextField.textDidChangeNotification, object: textField)
      textField.placeholder = "URL"
      textField.text = self.webBrowser?.webView?.url?.absoluteString
      textField.autocapitalizationType = .none
      textField.font = UIFont.systemFont(ofSize: 17)
      textField.keyboardType = UIKeyboardType.URL
      textField.clearButtonMode = .whileEditing
      self.inputUrlField = textField
    })
    alertController.addAction(UIAlertAction(title: NSLocalizedString("menu-cancel", comment: ""),
                                            style: UIAlertAction.Style.cancel,
                                            handler: nil))
    
    let addAlertAction = UIAlertAction(title: NSLocalizedString("menu-add", comment: ""),
                                       style: UIAlertAction.Style.default,
                                       handler: {_ in self.addBookmarkHandler()
    })
    alertController.addAction(addAlertAction)
    // save add action to toggle the enabled/disabled state when the text changes.
    addAction = addAlertAction
    updateAddButtonEnableState()
    
    self.present(alertController, animated: true, completion: nil)
  }
  
  private func checkIfEmpty() {
    if tableView.numberOfSections == 0 {
      let label = UILabel(frame: tableView.frame)
      label.textAlignment = .center
      label.textColor = UIColor.lightGray
      label.font = UIFont.systemFont(ofSize: 14.0)
      label.text = NSLocalizedString("browser-bookmarks-none", comment: "")
      tableView.backgroundView = label
    }
  }
  
  private func loadBookmarks() {
    let userData = AppDelegate.activeUserDefaults()
    bookmarks = userData.object(forKey: webBrowserBookmarksKey) as? [[String: String]] ?? [[String: String]]()
  }
  
  private func saveBookmarks() {
    let userData = AppDelegate.activeUserDefaults()
    if !bookmarks.isEmpty {
      userData.set(bookmarks, forKey: webBrowserBookmarksKey)
    } else {
      userData.removeObject(forKey: webBrowserBookmarksKey)
    }
    userData.synchronize()
  }
  
  func numberOfSections(in tableView: UITableView) -> Int {
    // Return the number of sections.
    let sections = bookmarks.count
    if sections > 0 {
      self.tableView.backgroundView = nil
    }
    return sections
  }
  
  func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
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
    selectionColor.backgroundColor = Colors.selectionSecondary
    cell.selectedBackgroundView = selectionColor
    cell.textLabel?.font = cell.textLabel?.font?.withSize(12.0)
    cell.detailTextLabel?.font = cell.detailTextLabel?.font?.withSize(10.0)
    return cell
  }
  
  func tableView(_ tableView: UITableView, willDisplay cell: UITableViewCell, forRowAt indexPath: IndexPath) {
    let bookmark = bookmarks[indexPath.section]
    cell.textLabel?.text = bookmark[bookmarkTitleKey]
    cell.detailTextLabel?.text = bookmark[bookmarkUrlKey]
    cell.accessoryType = UITableViewCell.AccessoryType.none
  }
  
  func tableView(_ tableView: UITableView, canEditRowAt indexPath: IndexPath) -> Bool {
    return true
  }
  
  func tableView(_ tableView: UITableView,
                 commit editingStyle: UITableViewCell.EditingStyle,
                 forRowAt indexPath: IndexPath) {
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
  
  private func performAction(for indexPath: IndexPath) {
    let bookmark = bookmarks[indexPath.section]
    if let urlString = bookmark[bookmarkUrlKey], let url = URL(string: urlString) {
      webBrowser?.webView?.load(URLRequest(url: url))
    }
    dismiss(animated: true, completion: nil)
  }
  
  func addBookmarkHandler() {
    guard let title = inputTitleField?.text, var urlString = inputUrlField?.text else {
      // This should be impossible.
      return
    }
    let url = URL(string: urlString)
    if url?.scheme == nil {
      let url = URL(string: "http://\(urlString)")
      urlString = url?.absoluteString ?? urlString
    }
    let bookmark = [bookmarkTitleKey: title, bookmarkUrlKey: urlString]
    bookmarks.append(bookmark)
    saveBookmarks()
    tableView.reloadData()
  }
  
  // handler
  @objc func handleTextFieldTextChangedNotification(notification: NSNotification) {
    updateAddButtonEnableState()
  }
  
  func updateAddButtonEnableState() {
    guard let title = inputTitleField?.text, let url = inputUrlField?.text else {
      addAction!.isEnabled = false
      return
    }
    
    // Enforce a minimum length of >= 1 for both the title and url text fields.
    addAction!.isEnabled = title.count > 0 && url.count > 0
  }
}
