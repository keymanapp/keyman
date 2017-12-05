//
//  KeymanWebViewController.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-31.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit
import WebKit

// MARK: - UIViewController
class KeymanWebViewController: UIViewController {
  weak var delegate: KeymanWebDelegate?
  var webView: WKWebView!

  var frame: CGRect? {
    didSet(frame) {
      if let view = view, let frame = frame {
        view.frame = frame
      }
    }
  }

  override func loadView() {
    let config = WKWebViewConfiguration()
    let prefs = WKPreferences()
    prefs.javaScriptEnabled = true
    config.preferences = prefs
    config.suppressesIncrementalRendering = false

    let userContentController = WKUserContentController()
    userContentController.add(self, name: "keyman")
    config.userContentController = userContentController

    webView = WKWebView(frame: frame ?? .zero, configuration: config)
    webView.isOpaque = false
    webView.backgroundColor = UIColor.clear
    webView.navigationDelegate = self
    webView.scrollView.isScrollEnabled = false

    view = webView
  }

  override func viewWillAppear(_ animated: Bool) {
    if let frame = frame {
      view.frame = frame
    }
    super.viewWillAppear(animated)
  }
}

// MARK: - JavaScript functions
extension KeymanWebViewController {
  func languageMenuPosition(_ completion: @escaping (CGRect) -> Void) {
    webView.evaluateJavaScript("langMenuPos();") { result, _ in
      guard let result = result as? String, !result.isEmpty else {
        return
      }
      let components = result.components(separatedBy: ",")
      let x = CGFloat(Float(components[0])!)
      let y = CGFloat(Float(components[1])!)
      let w = CGFloat(Float(components[2])!)
      let h = CGFloat(Float(components[3])!)
      let isPad = UIDevice.current.userInterfaceIdiom == .pad
      let adjY: CGFloat = isPad ? -0.5 : -1.0
      let frame = CGRect(x: x - w / 2.0, y: y - adjY, width: w, height: h)
      completion(frame)
    }
  }

  // FIXME: text is unused in the JS
  func executePopupKey(id: String, text: String) {
    webView.evaluateJavaScript("executePopupKey('\(id)','\(text)');", completionHandler: nil)
  }

  func setOskWidth(_ width: Int) {
    webView.evaluateJavaScript("setOskWidth(\(width));", completionHandler: nil)
  }

  func setOskHeight(_ height: Int) {
    webView.evaluateJavaScript("setOskHeight(\(height));", completionHandler: nil)
  }

  func setPopupVisible(_ visible: Bool) {
    webView.evaluateJavaScript("popupVisible(\(visible));", completionHandler: nil)
  }

  func setCursorRange(_ range: NSRange) {
    if range.location != NSNotFound {
      webView.evaluateJavaScript("setCursorRange(\(range.location),\(range.length));", completionHandler: nil)
    }
  }

  func setText(_ text: String?) {
    var text = text ?? ""
    text = text.replacingOccurrences(of: "\\", with: "\\\\")
    text = text.replacingOccurrences(of: "'", with: "\\'")
    text = text.replacingOccurrences(of: "\n", with: "\\n")
    webView.evaluateJavaScript("setKeymanVal('\(text)');", completionHandler: nil)
  }

  func setDeviceType(_ idiom: UIUserInterfaceIdiom) {
    let type: String
    switch idiom {
    case .phone:
      type = "AppleMobile"
    case .pad:
      type = "AppleTablet"
    default:
      Manager.shared.kmLog("Unexpected interface idiom: \(idiom)", checkDebugPrinting: false)
      return
    }
    webView.evaluateJavaScript("setDeviceType('\(type)');", completionHandler: nil)
  }

  func setKeyboard(id: String,
                   name: String,
                   languageID: String,
                   languageName: String,
                   version: String,
                   font: String,
                   oskFont: String) {
    let escapedID = id.replacingOccurrences(of: "'", with: "\\'")
    let escapedName = name.replacingOccurrences(of: "'", with: "\\'")
    let escapedLanguageID = languageID.replacingOccurrences(of: "'", with: "\\'")
    let escapedLanguageName = languageName.replacingOccurrences(of: "'", with: "\\'")
    let escapedVersion = version.replacingOccurrences(of: "'", with: "\\'")

    let jsString = """
    setKeymanLanguage('\(escapedName)','\(escapedID)','\(escapedLanguageName)','\(escapedLanguageID)',\
    '\(escapedVersion)',\(font),\(oskFont));
    """
    webView.evaluateJavaScript(jsString, completionHandler: nil)
  }
}

// MARK: - WKScriptMessageHandler
extension KeymanWebViewController: WKScriptMessageHandler {
  func userContentController(_ userContentController: WKUserContentController,
                             didReceive message: WKScriptMessage) {
    guard let fragment = message.body as? String, !fragment.isEmpty else {
      return
    }

    if fragment.hasPrefix("#insertText-") {
      let dnRange = fragment.range(of: "+dn=")!
      let sRange = fragment.range(of: "+s=")!

      let dn = Int(fragment[dnRange.upperBound..<sRange.lowerBound])!
      let s = fragment[sRange.upperBound...]

      // KMW uses dn == -1 to perform special processing of deadkeys.
      // This is handled outside of Swift so we don't delete any characters.
      let numCharsToDelete = max(0, dn)
      let newText = String(s).stringFromUTF16CodeUnits() ?? ""
      insertText(self, numCharsToDelete: numCharsToDelete, newText: newText)
      delegate?.insertText(self, numCharsToDelete: numCharsToDelete, newText: newText)
    } else if fragment.hasPrefix("#showKeyPreview-") {
      let xKey = fragment.range(of: "+x=")!
      let yKey = fragment.range(of: "+y=")!
      let wKey = fragment.range(of: "+w=")!
      let hKey = fragment.range(of: "+h=")!
      let tKey = fragment.range(of: "+t=")!

      let x = CGFloat(Float(fragment[xKey.upperBound..<yKey.lowerBound])!)
      let y = CGFloat(Float(fragment[yKey.upperBound..<wKey.lowerBound])!)
      let w = CGFloat(Float(fragment[wKey.upperBound..<hKey.lowerBound])!)
      let h = CGFloat(Float(fragment[hKey.upperBound..<tKey.lowerBound])!)
      let t = String(fragment[tKey.upperBound...])

      let frame = keyFrame(x: x, y: y, w: w, h: h)
      let preview = t.stringFromUTF16CodeUnits() ?? ""
      showKeyPreview(self, keyFrame: frame, preview: preview)
      delegate?.showKeyPreview(self, keyFrame: frame, preview: preview)
    } else if fragment.hasPrefix("#dismissKeyPreview-") {
      dismissKeyPreview(self)
      delegate?.dismissKeyPreview(self)
    } else if fragment.hasPrefix("#showMore-") {
      let baseFrameKey = fragment.range(of: "+baseFrame=")!
      let keysKey = fragment.range(of: "+keys=")!
      let fontKey = fragment.range(of: "+font=")
      let baseFrame = fragment[baseFrameKey.upperBound..<keysKey.lowerBound]
      let keys = fragment[keysKey.upperBound..<(fontKey?.lowerBound ?? fragment.endIndex)]
      let useSpecialFont = fontKey != nil

      let frameComponents = baseFrame.components(separatedBy: ",")
      let x = CGFloat(Float(frameComponents[0])!)
      let y = CGFloat(Float(frameComponents[1])!)
      let w = CGFloat(Float(frameComponents[2])!)
      let h = CGFloat(Float(frameComponents[3])!)
      let frame = keyFrame(x: x, y: y, w: w, h: h)

      let keyArray = keys.components(separatedBy: ";")
      var subkeyIDs: [String] = []
      var subkeyTexts: [String] = []

      for key in keyArray {
        let values = key.components(separatedBy: ":")
        switch values.count {
        case 1:
          subkeyIDs.append(values[0])
          subkeyTexts.append("")
        case 2:
          subkeyIDs.append(values[0])
          subkeyTexts.append(values[1].stringFromUTF16CodeUnits()!)
        default:
          Manager.shared.kmLog("Unexpected subkey key: \(key)", checkDebugPrinting: false)
        }
      }

      showSubkeys(self,
                  keyFrame: frame,
                  subkeyIDs: subkeyIDs,
                  subkeyTexts: subkeyTexts,
                  useSpecialFont: useSpecialFont)
      delegate?.showSubkeys(self,
                            keyFrame: frame,
                            subkeyIDs: subkeyIDs,
                            subkeyTexts: subkeyTexts,
                            useSpecialFont: useSpecialFont)
    } else if fragment.hasPrefix("#menuKeyDown-") {
      menuKeyDown(self)
      delegate?.menuKeyDown(self)
    } else if fragment.hasPrefix("#menuKeyUp-") {
      menuKeyUp(self)
      delegate?.menuKeyUp(self)
    } else if fragment.hasPrefix("#hideKeyboard-") {
      hideKeyboard(self)
      delegate?.hideKeyboard(self)
    } else if fragment.hasPrefix("ios-log:#iOS#") {
      let message = fragment.dropFirst(13)
      Manager.shared.kmLog("KMW Log: \(message)", checkDebugPrinting: true)
    } else {
      Manager.shared.kmLog("Unexpected KMW event: \(fragment)", checkDebugPrinting: false)
    }
  }

  private func keyFrame(x: CGFloat, y: CGFloat, w: CGFloat, h: CGFloat) -> CGRect {
    let isPad = UIDevice.current.userInterfaceIdiom == .pad
    let adjY: CGFloat = isPad ? -0.5 : -1.0
    return CGRect(x: x - w / 2.0, y: y - adjY, width: w, height: h)
  }
}

// MARK: - WKNavigationDelegate
extension KeymanWebViewController: WKNavigationDelegate {
  func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
    guard let url = webView.url else {
      return
    }
    guard url.lastPathComponent == Resources.kmwFileName && (url.fragment?.isEmpty ?? true) else {
      return
    }
    keyboardLoaded(self)
    delegate?.keyboardLoaded(self)
  }
}

// MARK: - KeymanWebDelegate
extension KeymanWebViewController: KeymanWebDelegate {

}
