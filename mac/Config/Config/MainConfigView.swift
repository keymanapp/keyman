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
  @State private var selectedPackage: KeymanPackage? = nil
  @State private var expandedPackageID: UUID? = nil
   
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
        DisclosureGroup(isExpanded: Binding(
          get: { self.expandedPackageID == package.id },
          // handles when the chevron arrow is clicked by the user
          // doesn't require if statement because the disclosure group has a state to toggle which is passed into the setter
          // $0 = true when disclosure group is open and $0 = false when disclosure group is closed
          set: { self.expandedPackageID = $0 ? package.id : nil }
        )) {
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
          .contentShape(Rectangle())
          // handles when the HStack is clicked by the user
          .onTapGesture {
            withAnimation {
              // requires if statement because the HStack doesn't have a state to toggle and so the package id property must be manually checked
              if self.expandedPackageID == package.id {
                self.expandedPackageID = nil
              } else {
                self.expandedPackageID = package.id
              }
            }
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
