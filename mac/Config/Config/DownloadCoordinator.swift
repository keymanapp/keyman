/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-08-21
 *
 * For coordination between WKWebview and SwiftUI views.
 * Implements WKNavigationDelegate and WKDownloadDelegate to trigger downloads
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
  
  public func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction,
                      decisionHandler: @escaping @MainActor (WKNavigationActionPolicy) -> Void) {
    
    guard let urlString = navigationAction.request.url?.absoluteString else {
      decisionHandler(.cancel)
      return
    }
    Logger.download.info("received url: \(urlString, privacy: .public)")

    // modern Swift regex literals (Requires macOS 13+ / iOS 16+)
    let regexInstall = /^http(s)?:\/\/keyman(-staging)?\.com(\.localhost)?\/keyboards\/install\/([^?\/]+)(\?(.+))?$/
    let regexRoot    = /^http(s)?:\/\/keyman(-staging)?\.com(\.localhost)?\/keyboards([\/?].*)?$/
    let regexGo      = /^http(s)?:\/\/keyman(-staging)?\.com(\.localhost)?\/go\/macos\/[^\/]+\/download-keyboards/
    
    if navigationAction.shouldPerformDownload {
      print("webView called decisionHandler for download")
      decisionHandler(.download)
      return
    }
    
    if let match = try? regexInstall.firstMatch(in: urlString) {
      decisionHandler(.cancel)
      
      let matchPackageId = String(match.4)
      if let downloadUrl = self.settings?.buildDownloadPackageUrl(for: matchPackageId) {
        Logger.download.info("package install, download url = \(downloadUrl.absoluteString, privacy: .public)")
        
        let newRequest = URLRequest(url: downloadUrl)
        
        DispatchQueue.main.async {
          webView.startDownload(using: newRequest) { download in
            print("download initiated to \(newRequest.url?.absoluteString ?? "nil")")
            download.delegate = self
            self.setupDownloadTracking(download)
          }
        }
      }
    }
    else if urlString.contains(regexRoot) || urlString.contains(regexGo) {
      Logger.download.info("root or go url, .allow")

      decisionHandler(.allow)
    }
    else if urlString.hasPrefix("keyman:") {
      if urlString.hasPrefix("keyman:link?url=") {
        Logger.download.info("starts with 'keyman' and 'keyman:link?url' open external url -> .cancel")

        decisionHandler(.cancel)
        
        let targetUrlString = String(urlString.dropFirst("keyman:link?url=".count))
        if let targetUrl = URL(string: targetUrlString) {
          NSWorkspace.shared.open(targetUrl)
        }
      } else {
        Logger.download.info("starts with 'keyman' but not 'keyman:link?url' open external url -> .download")

        decisionHandler(.download)
      }
    }
    else {
      Logger.download.info("default case, open in external browser")

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
    print("download initiated")
    
    guard let keymanSettings = self.settings else {
      print("tried to access settings before they were intialized in updateNSView")
      self.loadPackageFailed = true
      self.loadFailureMessage = InstallPackageError.internalError.localizedDescription
      completionHandler(nil)
      return
    }
    
    // notify settings that a keyboard download is beginning and get the
    // helper that is managing state for the package installation
    
    do {
      if let helper = try keymanSettings.initiateKmpFileDownload(kmpFilename: suggestedFilename) {
        print("download suggested filename: \(suggestedFilename)")
        self.loadFailureMessage = nil // Reset previous error
        self.loadPackageFailed = false
        
        self.installHelper = helper
        
        completionHandler(helper.temporaryKmpFileLocation)
      }
    } catch {
      print("Could not initiate package download, error: \(error)")
      self.loadPackageFailed = true
      self.loadFailureMessage = error.localizedDescription
      completionHandler(nil)
    }
  }
  
  public func downloadDidFinish(_ download: WKDownload) {
    self.isDownloading = false
    self.progressObserver = nil
    
    if let downloadDestination = installHelper?.temporaryKmpFileLocation {
      print("Download of \(downloadDestination.path()) was successful.")
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
    print("Download failed with error: \(error.localizedDescription)")
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
    // The web process crashed. Reload the webview safely here.
    print("WebKit process terminated unexpectedly: reloading content...")
    webView.reload()
  }
}
