//
//  Notifications.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-30.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

public typealias KeyboardDownloadStartedNotification = [InstallableKeyboard]
public typealias KeyboardDownloadCompletedNotification = [InstallableKeyboard]
public struct KeyboardDownloadFailedNotification {
  public let keyboards: [InstallableKeyboard]
  public let error: Error
}
public typealias LexicalModelDownloadStartedNotification = [InstallableLexicalModel]
public typealias LexicalModelDownloadCompletedNotification = [InstallableLexicalModel]
public struct LexicalModelDownloadFailedNotification {
  public let lmOrLanguageID: String
  public let error: Error
}
public typealias BatchUpdateStartedNotification = [LanguageResource]

public struct BatchUpdateCompletedNotification {
  public let successes: [[LanguageResource]]
  public let failures: [[LanguageResource]]
  public let errors: [Error]
}

public typealias KeyboardLoadedNotification = InstallableKeyboard
public typealias KeyboardChangedNotification = InstallableKeyboard
public typealias KeyboardRemovedNotification = InstallableKeyboard

public typealias KeyboardPickerDismissedNotification = Void

public typealias LexicalModelLoadedNotification = InstallableLexicalModel
public typealias LexicalModelChangedNotification = InstallableLexicalModel
public typealias LexicalModelRemovedNotification = InstallableLexicalModel

public typealias LexicalModelPickerDismissedNotification = Void

public struct Notifications {
  public static let keyboardDownloadStarted =
    NotificationName<KeyboardDownloadStartedNotification>("KeymanKeyboardDownloadStarted")
  public static let keyboardDownloadCompleted =
    NotificationName<KeyboardDownloadCompletedNotification>("KeymanKeyboardDownloadCompleted")
  public static let keyboardDownloadFailed =
    NotificationName<KeyboardDownloadFailedNotification>("KeymanKeyboardDownloadFailed")

  public static let keyboardLoaded =
    NotificationName<KeyboardLoadedNotification>("KeymanKeyboardLoaded")
  public static let keyboardChanged =
    NotificationName<KeyboardChangedNotification>("KeymanKeyboardChanged")
  public static let keyboardRemoved =
    NotificationName<KeyboardRemovedNotification>("KeymanKeyboardRemoved")

  public static let keyboardPickerDismissed =
    NotificationName<KeyboardPickerDismissedNotification>("KeymanKeyboardPickerDismissed")

  public static let lexicalModelDownloadStarted =
    NotificationName<LexicalModelDownloadStartedNotification>("KeymanLexicalModelDownloadStarted")
  public static let lexicalModelDownloadCompleted =
    NotificationName<LexicalModelDownloadCompletedNotification>("KeymanLexicalModelDownloadCompleted")
  public static let lexicalModelDownloadFailed =
    NotificationName<LexicalModelDownloadFailedNotification>("KeymanLexicalModelDownloadFailed")
    
  public static let lexicalModelLoaded =
    NotificationName<LexicalModelLoadedNotification>("KeymanLexicalModelLoaded")
  public static let lexicalModelChanged =
    NotificationName<LexicalModelChangedNotification>("KeymanLexicalModelChanged")
  public static let lexicalModelRemoved =
    NotificationName<LexicalModelRemovedNotification>("KeymanLexicalModelRemoved")
    
  public static let lexicalModelPickerDismissed =
    NotificationName<LexicalModelPickerDismissedNotification>("KeymanLexicalModelPickerDismissed")
  
  public static let batchUpdateStarted =
    NotificationName<BatchUpdateStartedNotification>("KeymanBatchUpdateStarted")
  public static let batchUpdateCompleted =
    NotificationName<BatchUpdateCompletedNotification>("KeymanBatchUpdateCompleted")
}
