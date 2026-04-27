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
  @EnvironmentObject var settings: SettingsContainer
  // selectedItem cannot be used with .enumerated
  //@State private var selectedItem: KeymanPackage?
  
  var body: some View {
    VStack {
      HStack {
        Image(systemName: "keyboard")
          .imageScale(.large)
          .foregroundColor(.accentColor)
        Text("keyboard count = \(settings.installedPackages.count)")
        Button("debug") {
          settings.debug()
        }
        Button("log defaults") {
          settings.logSettings()
        }
        Button("clear defaults") {
          settings.clearSettings()
        }
        Spacer()
      }
      .padding()
      
      ScrollView {
        VStack(alignment: .leading, spacing: 6) {
          ForEach(Array(settings.installedPackages.enumerated()), id: \.offset) { index, package in
            VStack {
              HStack(alignment: .center, spacing: 10) {
                Text(package.packageName)
                  .font(.headline)
                Text(package.packageVersion)
                  .font(.subheadline)
                // Example of Icon-Only Button
                Spacer()
                Button(action: {
                  settings.removePackage(at: index)
                }) {
                  Label("remove", systemImage: "trash.fill")
                }
                .labelStyle(.iconOnly)
                .buttonStyle(.borderless)
              }
              KeyboardListView(packageId: package.id, keyboards: package.keyboards)
            }
          }
        }
        .padding(.trailing, 25) // allow space for scroll bar
      }
    }
    .padding()
  }
}

#Preview {
  let settings = SettingsContainer()
  ConfigView()
    .environmentObject(settings)
}
