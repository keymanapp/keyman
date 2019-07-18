//
//  LanguageSpecificViewController.swift
//  KeymanEngine
//
//  Created by Randy Boring on 7/15/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import UIKit

class LanguageSpecificViewController: LanguageViewController {
  var language: Language!
  
  init(_ keyboardRepository: KeyboardRepository, language: Language) {
    super.init(keyboardRepository)
    self.language = language
  }
  
  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  override func postLanguageLoad(languageDict: [String: Language]) {
    super .postLanguageLoad(languageDict: languageDict)
    let langIdx = idxOfLanguage(languageID: language.id)
    self.showLanguageDetailView(title: self.title!, languageIndex: langIdx)
  }

}
