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

public typealias KeyboardLoadedNotification = InstallableKeyboard
public typealias KeyboardChangedNotification = InstallableKeyboard
public typealias KeyboardRemovedNotification = InstallableKeyboard

public typealias KeyboardPickerDismissedNotification = Void

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
}
