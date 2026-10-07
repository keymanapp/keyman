/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-21
 *
 * View used for directing the user to restart their mac.
 */

import SwiftUI
import OSLog

struct RestartMacView: View {
  @EnvironmentObject var installation: InstallationContainer
  let namespace: Namespace.ID
  
  var body: some View {
    VStack {
      Text("Restart Mac")
        .font(.title)
        .bold()
        .frame(maxWidth: .infinity, alignment: .center)
        .matchedGeometryEffect(id: "title", in: namespace)
      
      Spacer()
      
      Image(systemName: "restart.circle.fill")
        .font(.system(size: 100))
        .padding(16)
      Text("Restart your Mac to complete the installation.")
        .multilineTextAlignment(.leading)
        .padding(.bottom, 8)
      
      Spacer()
      
      GradientDivider(namespace: namespace)
        .padding(.bottom, 8)
      HStack {
        Text("Complete installation")
          .font(.title2)
          .frame(maxWidth: .infinity, alignment: .leading)
        Button("Restart...", role: nil) { restartMac() }
      }
    }
    .onAppear {
      installation.executeCurrentInstallationTask()
    }
  }
  
  /**
   * execute AppleScript to tell system to restart with standard 60-second countdown timer
   */
  func restartMac() {
    let scriptSource = "tell application \"loginwindow\" to «event aevtrrst»"
    
    guard let appleScript = NSAppleScript(source: scriptSource) else {
      Logger.app.error("failed to initialize AppleScript source")
        return
    }
    
    var errorInfo: NSDictionary?
    appleScript.executeAndReturnError(&errorInfo)
    
    if let error = errorInfo {
        Logger.app.error("AppleScript execution error: \(error)")
    }
  }
}
