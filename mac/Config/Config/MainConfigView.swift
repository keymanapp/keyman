/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 * FEAT/MAC/CONFIG-WINDOW TODO: Finish writing file summary
 * FEAT/MAC/CONFIG-WINDOW TODO: Set minimim width and height for window
 */

import SwiftUI
import KeymanSettings

struct MainConfigView: View {
  @EnvironmentObject var settings: SettingsContainer
  @State private var isShowingSheet = false
  @State private var isShowingDeleteAlert = false
  @State private var selectedPackage: KeymanPackage?
   
  /**
   * Sets isShowingDeleteAlert to true and assigns the state variable selectedPackage the KeymanPackage argument
   */
  private func showDeleteAlert(for package: KeymanPackage) -> Void {
    isShowingDeleteAlert = true
    selectedPackage = package
  }
  
  var body: some View {
    Button {
      isShowingSheet = true
    } label: {
      Label("Add Keyboard", systemImage: "plus")
        .font(.title2)
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
    
    List(settings.singleKeyboardPackages, id: \.id) { package in
      ForEach(package.keyboards) { keyboard in
        DisclosureGroup {
          KeyboardInfoView(for: package)
        } label: {
          HStack {
            
            Text(keyboard.name)
              .font(.title)

            Spacer()

            // see keyboard help button
            IconButtonView(action: { print("See help") }, label: "See help", systemImage: "questionmark.circle", helpText: "See help", font: .title2)

            // delete keyboard button
            IconButtonView(action: { showDeleteAlert(for: package) }, label: "Delete keyboard", systemImage: "trash", helpText: "Delete keyboard", font: .title2)
          }
        }
      }
    }
    .alert("Are you sure you want to delete the keyboard \"\(selectedPackage?.packageName ?? "")\"?",
             isPresented: $isShowingDeleteAlert,
             presenting: selectedPackage) { package in
        
        Button("Cancel", role: .cancel) { }
        
        Button("Delete", role: .destructive) {
          settings.removeInstalledPackage(with: package.id)
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
