//
//  ConfigApp.swift
//  Config
//
//  Created by Shawn - SIL on 2/26/26.
//

import SwiftUI
import KeymanSettings

@main
struct ConfigApp: App {
//  @StateObject var configuration = Configuration()
  @StateObject var settings = SettingsContainer()
  var body: some Scene {
    WindowGroup {
      ConfigView()
        .environmentObject(settings)
   }
  }
}
