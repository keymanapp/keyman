//
//  ConfigApp.swift
//  Config
//
//  Created by Shawn - SIL on 2/26/26.
//

import SwiftUI

@main
struct ConfigApp: App {
  @StateObject var configuration = Configuration()
  var body: some Scene {
    WindowGroup {
      ConfigView()
        .environmentObject(configuration)
   }
  }
}
