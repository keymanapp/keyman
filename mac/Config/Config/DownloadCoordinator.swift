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

public class DownloadCoordinator: NSObject, ObservableObject, WKNavigationDelegate, WKDownloadDelegate {
  @Published var showInstallSheet = false
  @Published var installHelper: PackageInstallHelper?
  var downloadFileUrl: URL? = nil
  var settings: SettingsContainer?
  
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
    print("webView navigationAction:didBecome called")
    download.delegate = self // Assign delegate for file saving
  }
  
  public func webView(_ webView: WKWebView,
               navigationResponse: WKNavigationResponse,
               didBecome download: WKDownload) {
    print("webView navigationResponse:didBecome called")
    download.delegate = self
  }

  public func download(_ download: WKDownload, decideDestinationUsing response: URLResponse, suggestedFilename: String, completionHandler: @escaping @MainActor @Sendable (URL?) -> Void) {
    print("download initiated")
    
    guard let keymanSettings = self.settings else {
      print("tried to access settings before they were intialized in updateNSView")
      completionHandler(nil)
      return
    }
    
    // notify settings that a keyboard download is beginning and get the URL to
    // the temporary folder where it should be downloaded
    
    do {
      if let helper = try keymanSettings.initiateKmpFileDownload(kmpFilename: suggestedFilename) {
        self.installHelper = helper
        downloadFileUrl = helper.temporaryKmpFileLocation
        completionHandler(downloadFileUrl)
      }
    } catch {
      print("could not initiate KMP package download")
      completionHandler(nil)
    }
    
//    downloadFileUrl = keymanSettings.preparePackageDownload(kmpFileName: suggestedFilename)
  }
  
  public func downloadDidFinish(_ download: WKDownload) {
    DispatchQueue.main.async {
        // Trigger the SwiftUI modal sheet
        self.showInstallSheet = true
    }

    if let downloadFileUrl {
      print("Download of \(downloadFileUrl.path()) was successful.")
      if let settings {
        do {
          try settings.packageDownloadComplete(kmpFileUrl: downloadFileUrl)
        } catch {
          // MAC-CONFIG-TODO: communicate failed install to user
        }
      }
    }
  }

  public func download(_ download: WKDownload, didFailWithError error: Error, resumeData: Data?) {
    // MAC-CONFIG-TODO: communicate failed install to user
    print("Download failed with error: \(error.localizedDescription)")
  }

  public func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
      // The web process crashed. Reload the webview safely here.
      print("WebKit process terminated unexpectedly: reloading content...")
      webView.reload()
  }
}
