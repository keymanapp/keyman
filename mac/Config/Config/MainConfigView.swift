/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 * FEAT/MAC/CONFIG-WINDOW TODO: Finish writing file summary
 */

import SwiftUI
import KeymanSettings

struct MainConfigView: View {
  @EnvironmentObject var settings: SettingsContainer
  @State private var isShowingSheet = false
  @State private var isShowingAlert = false
  @State private var selectedPackage: KeymanPackage?
    
  private func showAlert(for package: KeymanPackage) -> Void {
    isShowingAlert = true
    selectedPackage = package
  }
  
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
    
    List(Array(zip(settings.singleKeyboardPackages.indices, settings.singleKeyboardPackages)), id: \.1.id) { index, package in
      ForEach(package.keyboards) { keyboard in
        DisclosureGroup {
          KeyboardInfoView(for: package)
        } label: {
          HStack {
            
            Text(keyboard.name)
              .font(.title2)

            Spacer()

            // see keyboard help button
            IconButtonView(action: { print("See help") }, label: "See help", systemImage: "questionmark.circle", helpText: "See help")

            // delete keyboard button
            IconButtonView(action: { showAlert(for: package) }, label: "Delete keyboard", systemImage: "trash", helpText: "Delete keyboard")
          }
        }
      }
    }
    // FEAT?/MAC/CONFIG-WINDOW TODO: Make the alert title display the keyboard name
    .alert("Are you sure you want to delete the keyboard \"\(selectedPackage?.packageName ?? "")\"?",
             isPresented: $isShowingAlert,
             presenting: selectedPackage) { package in
        
        Button("Cancel", role: .cancel) { }
        
        Button("Delete", role: .destructive) {
          // FEAT/MAC/CONFIG-WINDOW TODO: Make removeInstalledPackage based on id
          //settings.removeInstalledPackage(at: index)
        }
      } message: { package in
        Text("You can't undo this action.")
      }
    .padding()
  }
}

#Preview {
  var settings = SettingsContainer()
  MainConfigView()
    .environmentObject(settings)
}
