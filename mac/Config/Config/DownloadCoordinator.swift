/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-08-21
 *
 * For coordination between WKWebview and SwiftUI views.
 * Implements WKNavigationDelegate and WKDownloadDelegate to support downloads
 * of Keyman packages and publishes several fields to allow SwiftUI views to
 * - display download progress
 * - display errors that cause the download or package validation to fail
 * - prompt with a confirm sheet including a package read me and button to install
 */

import WebKit
import Combine
import KeymanSettings
import OSLog

// safe to designate the whole Coordinator class as @MainActor with Swift 6.0
// when delegate calls come on a background thread, Swift 6 will
// intercept and switch to the main thread for calls to our code

@MainActor
public class DownloadCoordinator: NSObject, ObservableObject, WKNavigationDelegate, WKDownloadDelegate {
  @Published var isDownloading = false
  // progress is between 0.0 and 1.0
  @Published var downloadProgress: Double = 0.0
  
  @Published var showConfirmPackageSheet = false
  @Published var installHelper: PackageInstallHelper?
  
  @Published var loadFailureMessage: String?
  @Published var loadPackageFailed = false
  
  var settings: SettingsContainer?
  private var progressObserver: NSKeyValueObservation?
  private var activeDownload: WKDownload?

  // Swift regex literals, introduced in macOS 13+ and using instead of NSRegularExpression
  private static let regexInstall = /^http(s)?:\/\/keyman(-staging)?\.com(\.localhost)?\/keyboards\/install\/([^?\/]+)(\?(.+))?$/
  private static let regexRoot    = /^http(s)?:\/\/keyman(-staging)?\.com(\.localhost)?\/keyboards([\/?].*)?$/
  private static let regexGo      = /^http(s)?:\/\/keyman(-staging)?\.com(\.localhost)?\/go\/macos\/[^\/]+\/download-keyboards/

  public func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction,
                      decisionHandler: @escaping @MainActor (WKNavigationActionPolicy) -> Void) {
    
    guard let urlString = navigationAction.request.url?.absoluteString else {
      decisionHandler(.cancel)
      return
    }
    Logger.download.log("received url: \(urlString, privacy: .public)")
    
    // if the url matches the install url pattern, then cancel the request,
    // build the standard URLRequest for a package installation and send it
    if let match = try? DownloadCoordinator.regexInstall.firstMatch(in: urlString) {
      decisionHandler(.cancel)
      
      // get the package id (though it appears to be identifying a keyboard in the URL)
      let matchPackageId = String(match.4)
      if let downloadUrl = self.settings?.buildDownloadPackageUrl(for: matchPackageId) {
        Logger.download.info("package install, download url = \(downloadUrl.cleanUrlPath(), privacy: .public)")
        LogUtil.infoBreadcrumb("package install, download url = \(downloadUrl.cleanUrlPath())", category: .download)

        let newRequest = URLRequest(url: downloadUrl)
        
        DispatchQueue.main.async {
          webView.startDownload(using: newRequest) { download in
            Logger.download.info("download initiated to \(newRequest.url?.cleanUrlPath() ?? "nil", privacy: .public)")
            LogUtil.infoBreadcrumb("download initiated to \(newRequest.url?.cleanUrlPath() ?? "nil")", category: .download)
            download.delegate = self
            self.setupDownloadTracking(download)
          }
        }
      }
    }
    else if urlString.contains(DownloadCoordinator.regexRoot) ||
              urlString.contains(DownloadCoordinator.regexGo) {
      Logger.download.info("requested root or go url: load in webview")
      LogUtil.infoBreadcrumb("requested root or go url: load in webview", category: .download)

      decisionHandler(.allow)
    }
    else {
      Logger.download.info("default case, open in external browser")
      LogUtil.infoBreadcrumb("default case, open in external browser", category: .download)

      decisionHandler(.cancel)
      if let targetUrl = URL(string: urlString) {
        NSWorkspace.shared.open(targetUrl)
      }
    }
  }

  /**
   *  Setup the observer to track progress of the download.
   */
  private func setupDownloadTracking(_ download: WKDownload) {
    // record download in case we need to cancel
    self.activeDownload = download
    
    // reset progress states
    self.isDownloading = true
    self.downloadProgress = 0.0
    
    progressObserver = download.progress.observe(\.fractionCompleted, options: [.new]) { [weak self] _, change in
      guard let newValue = change.newValue else { return }
      
      Task { @MainActor [weak self] in
        self?.downloadProgress = newValue
      }
    }
  }
  
  /**
   * Called when the AddKeyboardView is closed. If there is a download in progress, it will be canceled.
   */
  public func cancelActiveDownload() {
    guard isDownloading else { return }   // only applied during downloads
    
    self.activeDownload?.cancel()
    self.activeDownload = nil
    self.progressObserver = nil
    self.isDownloading = false
    self.installHelper = nil
    self.settings?.userCanceledPackageInstallation()
  }
  
  public func download(_ download: WKDownload, decideDestinationUsing response: URLResponse, suggestedFilename: String, completionHandler: @escaping @MainActor @Sendable (URL?) -> Void) {
    Logger.download.debug("download initiated")
    
    guard let keymanSettings = self.settings else {
      Logger.download.error("tried to access settings before they were intialized")
      LogUtil.errorBreadcrumb("tried to access settings before they were intialized", category: .download)
      self.loadPackageFailed = true
      self.loadFailureMessage = InstallPackageError.internalError.localizedDescription
      completionHandler(nil)
      return
    }
    
    // notify settings that a keyboard download is beginning and get the
    // helper that is managing state for the package installation
    
    do {
      if let helper = try keymanSettings.initiateKmpFileDownload(kmpFilename: suggestedFilename) {
        Logger.download.info("download suggested filename: \(suggestedFilename, privacy: .public)")
        LogUtil.infoBreadcrumb("download suggested filename: \(suggestedFilename)", category: .download)
        self.loadFailureMessage = nil // Reset previous error
        self.loadPackageFailed = false
        
        self.installHelper = helper
        
        completionHandler(helper.temporaryKmpFileLocation)
      }
    } catch {
      Logger.download.error("could not initiate package download, error: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("could not initiate package download, error: \(error as NSError)", category: .download)
      self.loadPackageFailed = true
      self.loadFailureMessage = error.localizedDescription
      completionHandler(nil)
    }
  }
  
  public func downloadDidFinish(_ download: WKDownload) {
    self.isDownloading = false
    self.progressObserver = nil
    
    if let downloadDestination = installHelper?.temporaryKmpFileLocation {
      Logger.download.info("download of \(downloadDestination.cleanUrlPath(), privacy: .public) was successful.")
      LogUtil.infoBreadcrumb("download of \(downloadDestination.cleanUrlPath()) was successful.", category: .download)

      if let settings {
        do {
          try settings.packageDownloadComplete(kmpFileUrl: downloadDestination)
          // Trigger the SwiftUI modal sheet
          self.showConfirmPackageSheet = true
        } catch {
          self.loadPackageFailed = true
          self.loadFailureMessage = error.localizedDescription
        }
      }
    }
  }
  
  public func download(_ download: WKDownload, didFailWithError error: Error, resumeData: Data?) {
    Logger.download.error("download failed with error: \(error as NSError, privacy: .public)")
    LogUtil.errorBreadcrumb("download failed with error: \(error as NSError)", category: .download)
    self.isDownloading = false
    self.progressObserver = nil
    self.loadPackageFailed = true
    self.loadFailureMessage = error.localizedDescription
    self.installHelper = nil
    if let settings {
      settings.packageInstallationFailed()
    }
  }
  
  public func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
    // The web process crashed. Reload the webview safely.
    Logger.download.error("webkit process terminated unexpectedly: reloading content")
    LogUtil.errorBreadcrumb("webkit process terminated unexpectedly: reloading content", category: .download)
    webView.reload()
  }
}
