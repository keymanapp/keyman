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
        Text("keyboard count = \(settings.installedKeyboardPackages.count)")
        Button("debug") {
          settings.debug()
        }
        Spacer()
      }
      .padding()
      
//      ScrollView(showsIndicators: false) {
//        List {
//            // Use $ to get binding to the published array
//            ForEach($settings.installedKeyboardPackages) { $package in
//                Section(header: Text("Section")) {
//                    // Use $ to get binding to the nested array
//                  ForEach($package.keyboards) { $keyboard in
//                    Toggle(keyboard.keyboardId, isOn: $keyboard.enabled) // Binding here
//                    }
//                }
//            }
//        }
//        .padding()
//      }
      ScrollView {
        VStack(alignment: .leading, spacing: 6) {
          ForEach(Array(settings.installedKeyboardPackages.enumerated()), id: \.offset) { index, package in
            VStack {
              HStack(alignment: .center, spacing: 10) {
                Text(package.packageName)
                  .font(.headline)
                Text(package.packageVersion)
                  .font(.subheadline)
                // Example of Icon-Only Button
                Spacer()
                Button(action: {
                  settings.installedKeyboardPackages.remove(at: index)
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
