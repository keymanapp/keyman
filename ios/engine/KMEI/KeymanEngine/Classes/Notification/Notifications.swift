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
public typealias KeyboardChangedNotification = InstallableKeyboard

public struct Notifications {
  public static let keyboardDownloadStarted =
    NotificationName<KeyboardDownloadStartedNotification>("KeymanKeyboardDownloadStarted")
  public static let keyboardDownloadCompleted =
    NotificationName<KeyboardDownloadCompletedNotification>("KeymanKeyboardDownloadCompleted")
  public static let keyboardDownloadFailed =
    NotificationName<KeyboardDownloadFailedNotification>("KeymanKeyboardDownloadFailed")
  public static let keyboardChanged =
    NotificationName<KeyboardChangedNotification>("KeymanKeyboardChanged")
}
