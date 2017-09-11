//
//  ActivityItemProvider.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-04.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

let htmlMailFormat =
  "<html><head><style type=\"text/css\">pre {font-family:\"%@\";font-size:%@;}</style>" +
  "</head><body><pre>%@</pre>%@</body></html>"
let mailFooterTextForPad = "<br><br>Sent from&nbsp<a href=\"http://keyman.com/ipad\">Keyman for iPad</a>"
let mailFooterTextForPhone = "<br><br>Sent from&nbsp<a href=\"http://keyman.com/iphone\">Keyman for iPhone</a>"
let fbText = "Can't read this? Help at http://keyman.com/fonts"

// Prepares the share text
class ActivityItemProvider: UIActivityItemProvider {
  let text: String
  let font: UIFont

  init(text: String, font: UIFont?) {
    self.text = text
    self.font = font ?? UIFont.systemFont(ofSize: UIFont.systemFontSize)
    super.init(placeholderItem: text)
  }

  override var item: Any {
    switch activityType {
    case UIActivityType.mail?:
      return htmlMail(withText: text, font: font)
    case UIActivityType.postToFacebook?:
      return "\(text)\n\n\(fbText)"
    case UIActivityType.postToTwitter?:
        if text.characters.count > 140 {
          return text.substring(to: text.index(text.startIndex, offsetBy: 140))
        }
        return text
    default:
      return text
    }
  }

  func htmlMail(withText text: String, font: UIFont) -> String {
    let mailText = text.replacingOccurrences(of: "<", with: "&lt;").replacingOccurrences(of: ">", with: "&gt;")
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
