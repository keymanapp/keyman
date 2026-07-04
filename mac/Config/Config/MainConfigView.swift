/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 * TODO: Finish writing file summary
 */

import SwiftUI
import KeymanSettings

struct MainConfigView: View {
  @EnvironmentObject var settings: SettingsContainer
  @State private var isShowingSheet = false
  @State private var packageToDeleteIndex: Int?
  
  var body: some View {
    
    Button {
      isShowingSheet = true
    } label: {
      Label("Add Keyboard", systemImage: "plus")
    }
    .buttonStyle(.bordered)
    .clipShape(Capsule())
    .padding()
    // Binds the visibility state to the sheet builder
    .sheet(isPresented: $isShowingSheet) {
      InstallKeyboardView()
        .frame(width: 960, height: 390)
      // FEAT/MAC/CONFIG-WINDOW TODO: Make width and height percentages
    }
    
    List(Array(zip(settings.installedPackages.indices, settings.installedPackages)), id: \.1.id) { index, package in
      ForEach(package.keyboards) { keyboard in
        DisclosureGroup {
          Text("Keyboard Info Goes Here")
        } label: {
          HStack {
            
            Text(keyboard.name)
              .font(.title2)
            
            Spacer()
            
            // view keyboard info button
            PackageButtonView(action: { print("View info") }, label: "View info", systemImage: "info.circle", helpText: "View info")
            
            // delete keyboard button
            PackageButtonView(action: { packageToDeleteIndex = index }, label: "Delete keyboard", systemImage: "trash", helpText: "Delete keyboard")
            // FEAT/MAC/CONFIG-WINDOW TODO: Extract and clean up alert modifier
            .alert("Are you sure you want to delete the keyboard '\(package.packageName)'?",
                   isPresented: Binding(
                    get: { packageToDeleteIndex == index },
                    set: { if !$0 { packageToDeleteIndex = nil } }
                   )) {
                // cancel button
                Button("Cancel", role: .cancel) {
                  packageToDeleteIndex = nil
                }
                // delete button
                Button("Delete", role: .destructive) {
                  settings.removePackage(at: index)
                  packageToDeleteIndex = nil
                }
                   } message: {
                     Text("You can't undo this action.")
                   }
            
          }
        }
      }
    }
    .padding()
  }
}

#Preview {
  var settings = SettingsContainer()
  MainConfigView()
    .environmentObject(settings)
}
