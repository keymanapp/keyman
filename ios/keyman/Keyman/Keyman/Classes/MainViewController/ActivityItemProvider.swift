//
//  ActivityItemProvider.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-04.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

private let htmlMailFormat = """
  <html><head><style type=\"text/css\">pre {font-family:\"%@\";font-size:%@;}</style>
  </head><body><pre>%@</pre>%@</body></html>
  """
private let mailFooterTextForPad =
"<br><br>Sent from&nbsp;<a href=\"https://keyman.com/ipad\">Keyman for iPad</a>"
private let mailFooterTextForPhone =
"<br><br>Sent from&nbsp;<a href=\"https://keyman.com/iphone\">Keyman for iPhone</a>"
private let fbText = "Can't read this? Help at https://keyman.com/fonts"

// Prepares the share text
class ActivityItemProvider: UIActivityItemProvider {
  private let text: String
  private let font: UIFont
  
  init(text: String, font: UIFont?) {
    self.text = text
    self.font = font ?? UIFont.systemFont(ofSize: UIFont.systemFontSize)
    super.init(placeholderItem: text)
  }
  
  override var item: Any {
    switch activityType {
    case UIActivity.ActivityType.mail?:
      return htmlMail(withText: text, font: font)
    case UIActivity.ActivityType.postToFacebook?:
      return "\(text)\n\n\(fbText)"
    case UIActivity.ActivityType.postToTwitter?:
      return text.prefix(140)
    default:
      return text
    }
  }
  
  private func htmlMail(withText text: String, font: UIFont) -> String {
    let mailText = text.replacingOccurrences(of: "&", with: "&amp;")
      .replacingOccurrences(of: "<", with: "&lt;")
      .replacingOccurrences(of: ">", with: "&gt;")
    let familyName = font.familyName
    let fontSize = String(Int(font.pointSize))
    let footerText: String
    if UIDevice.current.userInterfaceIdiom == .pad {
      footerText = mailFooterTextForPad
    } else {
      footerText = mailFooterTextForPhone
    }
    // html mail format: family-name, font-size, text, footer-text
    return String(format: htmlMailFormat, familyName, fontSize, mailText, footerText)
  }
}
