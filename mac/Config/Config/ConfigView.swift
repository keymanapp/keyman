/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * Placeholder class
 */

import SwiftUI
import KeymanSettings

struct ConfigView: View {
  @EnvironmentObject var settings: KeymanSettings
  
  var body: some View {
    VStack {
      Image(systemName: "keyboard")
        .imageScale(.large)
        .foregroundColor(.secondary)
      Text(settings.settingName)
      Button("update settings") {settings.settingName = "Settings changed!"}
    }
    .padding()
  }
}

#Preview {
  let configuration = Configuration()
  ConfigView()
    .environmentObject(configuration)
}
