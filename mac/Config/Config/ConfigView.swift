/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * Placeholder class
 */

import SwiftUI
import KeymanData

struct ConfigView: View {
  @EnvironmentObject var configuration: Configuration
  
  var body: some View {
    VStack {
      Image(systemName: "keyboard")
        .imageScale(.large)
        .foregroundColor(.secondary)
      Text(configuration.keymanData.value)
    }
    .padding()
  }
}

#Preview {
  let configuration = Configuration()
  ConfigView()
    .environmentObject(configuration)
}
