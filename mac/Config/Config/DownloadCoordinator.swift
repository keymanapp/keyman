/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-08-21
 *
 * For coordination between WKWebview and SwiftUI views
 */

import WebKit
import Combine
import KeymanSettings

// safe to designate the whole Coordinator class as @MainActor with Swift 6.0
// when delegate calls come on a background thread, Swift 6 will
// intercept and switch to the main thread for calls to our code

@MainActor
public class DownloadCoordinator: NSObject, ObservableObject, WKNavigationDelegate, WKDownloadDelegate {
  @Published var showDownloadSheet = false
  @Published var isDownloading = false
  // progress is between 0.0 and 1.0
  @Published var downloadProgress: Double = 0.0
  @Published var showConfirmPackageSheet = false
  @Published var installHelper: PackageInstallHelper?
  @Published var loadFailureMessage: String?
  @Published var loadPackageFailed = false
  
  var downloadFileUrl: URL?
  var settings: SettingsContainer?
  private var progressObserver: NSKeyValueObservation?
  
  public func webView(_ webView: WKWebView,
                      decidePolicyFor navigationAction: WKNavigationAction,
                      preferences: WKWebpagePreferences,
                      decisionHandler: @escaping @MainActor (WKNavigationActionPolicy, WKWebpagePreferences) -> Void) {
    
    print("deciding navigation based on action")
    
    if let url = navigationAction.request.url {
      print("webView navigationAction.request.url: \(url)")
    }
    
    // Trust HTML download attribute if present
    if navigationAction.shouldPerformDownload {
      print("webView called decisionHandler for download")
      decisionHandler(.download, preferences)
      return
    }
    
    // MAC-CONFIG-TODO: is this necessary or is download attribute enough to identify
    // check if URL ends with a target file extension
    if let url = navigationAction.request.url {
      if url.pathExtension.lowercased() == KeymanPaths.keymanPackageFileExtension {
        decisionHandler(.download, preferences)
        print("webView found .kmp, called decisionHandler for download")
        return
      }
    }
    
    decisionHandler(.allow, preferences)
  }
  
  /** decide whether the navigation should be allowed, canceled or result in a download */
  public func webView(_ webView: WKWebView,
                      decidePolicyFor navigationResponse: WKNavigationResponse,
                      decisionHandler: @escaping @MainActor (WKNavigationResponsePolicy) -> Void) {
    print("deciding navigation based on response")
    
    if navigationResponse.canShowMIMEType {
      decisionHandler(.allow)
    } else {
      guard let keymanSettings = self.settings else {
        print("webView decidePolicyFor:decisionHandler: no settings")
        decisionHandler(.cancel)
        return
      }
      
      // if a download is already in progress then stop another from starting
      if keymanSettings.isDownloadInProgress() {
        print("download already in progress, download canceled")
        decisionHandler(.cancel)
      } else {
        decisionHandler(.download)
      }
    }
  }
  
  public func webView(_ webView: WKWebView, navigationAction: WKNavigationAction, didBecome download: WKDownload) {
    print("📍 didBecome called via navigationAction")
    download.delegate = self // Assign delegate for file saving
    
    setupDownloadTracking(download)
  }
  
  public func webView(_ webView: WKWebView,
                      navigationResponse: WKNavigationResponse,
                      didBecome download: WKDownload) {
    print("📍 didBecome called via navigationResponse")
    download.delegate = self
    
    setupDownloadTracking(download)
  }
  
  // Common setup function to attach the delegate and the KVO progress observer
  private func setupDownloadTracking(_ download: WKDownload) {
    download.delegate = self
    
    // reset progress states
    self.isDownloading = true
    self.downloadProgress = 0.0
    
    progressObserver = download.progress.observe(\.fractionCompleted, options: [.new]) { [weak self] _, change in
      guard let newValue = change.newValue else { return }
      
      Task { @MainActor [weak self] in
        self?.downloadProgress = newValue
        print("Download Progress: \(Int(newValue * 100))%")
      }
    }
  }
  
  public func download(_ download: WKDownload, decideDestinationUsing response: URLResponse, suggestedFilename: String, completionHandler: @escaping @MainActor @Sendable (URL?) -> Void) {
    print("download initiated")
    
    guard let keymanSettings = self.settings else {
      print("tried to access settings before they were intialized in updateNSView")
      completionHandler(nil)
      return
    }
    
    // notify settings that a keyboard download is beginning and get the
    // helper that is managing state for the package installation
    
    do {
      if let helper = try keymanSettings.initiateKmpFileDownload(kmpFilename: suggestedFilename) {
        
        self.isDownloading = true
        self.downloadProgress = 0.0
        self.loadFailureMessage = nil // Reset previous error
        self.loadPackageFailed = false
        
        self.installHelper = helper
        let targetUrl = helper.temporaryKmpFileLocation
        self.downloadFileUrl = targetUrl
        
        completionHandler(targetUrl)
      }
    } catch {
      print("could not initiate KMP package download, error: \(error)")
      completionHandler(nil)
    }
  }
  
  public func downloadDidFinish(_ download: WKDownload) {
    self.isDownloading = false
    self.showDownloadSheet = true
    self.progressObserver = nil
    
    if let downloadFileUrl {
      print("Download of \(downloadFileUrl.path()) was successful.")
      if let settings {
        do {
          try settings.packageDownloadComplete(kmpFileUrl: downloadFileUrl)
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
