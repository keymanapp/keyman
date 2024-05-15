//
//  NotificationObserver.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-30.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// NotificationCenter observer which automatically deregisters itself when deinitializing.
public class NotificationObserver {
  private let observer: NSObjectProtocol
  private let center: NotificationCenter
  
  public init(observer: NSObjectProtocol, center: NotificationCenter) {
    self.observer = observer
    self.center = center
  }
  
  deinit {
    center.removeObserver(observer)
  }
}
