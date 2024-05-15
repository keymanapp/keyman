//
//  Notifications.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-30.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

@available(*, deprecated, message: "Download notifications are now package-based.  Use `PackageDownloadStartedNotification` instead.")
public typealias KeyboardDownloadStartedNotification = [InstallableKeyboard]
@available(*, deprecated, message: "Download notifications are now package-based.  Use `PackageDownloadCompletedNotification` instead.")
public typealias KeyboardDownloadCompletedNotification = [InstallableKeyboard]
@available(*, deprecated, message: "Download notifications are now package-based.  Use `PackageDownloadFailedNotification` instead.")
public struct KeyboardDownloadFailedNotification {
  public let keyboards: [InstallableKeyboard]
  public let error: Error
}

@available(*, deprecated, message: "Download notifications are now package-based.  Use `PackageDownloadStartedNotification` instead.")
public typealias LexicalModelDownloadStartedNotification = [InstallableLexicalModel]
@available(*, deprecated, message: "Download notifications are now package-based.  Use `PackageDownloadCompletedNotification` instead.")
public typealias LexicalModelDownloadCompletedNotification = [InstallableLexicalModel]
@available(*, deprecated, message: "Download notifications are now package-based.  Use `PackageDownloadFailedNotification` instead.")
public struct LexicalModelDownloadFailedNotification {
  public let lmOrLanguageID: String
  public let error: Error
}

public typealias PackageDownloadStartedNotification = KeymanPackage.Key
public typealias PackageDownloadCompletedNotification = KeymanPackage
public struct PackageDownloadFailedNotification {
  public let packageKey: KeymanPackage.Key?
  public let error: Error
}

public typealias BatchUpdateStartedNotification = [AnyLanguageResource]

public struct BatchUpdateCompletedNotification {
  public let successes: [KeymanPackage.Key]
  public let failures: [(KeymanPackage.Key, Error)]
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
  public static let packageDownloadStarted = NotificationName<PackageDownloadStartedNotification>("KeymanPackageDownloadStarted")
  public static let packageDownloadCompleted = NotificationName<PackageDownloadCompletedNotification>("KeymanPackageDownloadCompleted")
  public static let packageDownloadFailed = NotificationName<PackageDownloadFailedNotification>("KeymanPackageDownloadFailed")
  
  @available(swift, deprecated: 0.1, obsoleted: 0.1, message: "Download notifications are now package-based.  Use `packageDownloadStarted` instead.", renamed: "packageDownloadStarted")
  public static let keyboardDownloadStarted =
  NotificationName<KeyboardDownloadStartedNotification>("KeymanKeyboardDownloadStarted")
  @available(swift, deprecated: 0.1, obsoleted: 0.1, message: "Download notifications are now package-based.  Use `packageDownloadCompleted` instead.", renamed: "packageDownloadCompleted")
  public static let keyboardDownloadCompleted =
  NotificationName<KeyboardDownloadCompletedNotification>("KeymanKeyboardDownloadCompleted")
  @available(swift, deprecated: 0.1, obsoleted: 0.1, message: "Download notifications are now package-based.  Use `packageDownloadFailed` instead.", renamed: "packageDownloadFailed")
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
  
  @available(swift, deprecated: 0.1, obsoleted: 0.1, message: "Download notifications are now package-based.  Use `packageDownloadStarted` instead.", renamed: "packageDownloadStarted")
  public static let lexicalModelDownloadStarted =
  NotificationName<LexicalModelDownloadStartedNotification>("KeymanLexicalModelDownloadStarted")
  @available(swift, deprecated: 0.1, obsoleted: 0.1, message: "Download notifications are now package-based.  Use `packageDownloadCompleted` instead.", renamed: "packageDownloadCompleted")
  public static let lexicalModelDownloadCompleted =
  NotificationName<LexicalModelDownloadCompletedNotification>("KeymanLexicalModelDownloadCompleted")
  @available(swift, deprecated: 0.1, obsoleted: 0.1, message: "Download notifications are now package-based.  Use `packageDownloadFailed` instead.", renamed: "packageDownloadFailed")
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
