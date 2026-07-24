/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 * FEAT/MAC/CONFIG-WINDOW TODO: Finish writing file summary
 * FEAT/MAC/CONFIG-WINDOW TODO: Set width and height for window
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
  private func showDeleteAlert(for package: KeymanPackage) {
    isShowingDeleteAlert = true
    selectedPackage = package
  }
  
  var body: some View {
    VStack(spacing: 0) {
      Button {
        isShowingSheet = true
      } label: {
        Label("Add Keyboard", systemImage: "plus")
          .font(.title2)
      }
      .buttonStyle(.bordered)
      .clipShape(Capsule())
      .padding([.top, .leading, .trailing])
      // binds the visibility state to the sheet builder
      .sheet(isPresented: $isShowingSheet) {
        InstallKeyboardView()
          .frame(width: 960, height: 390)
        // FEAT/MAC/CONFIG-WINDOW TODO: Make width and height percentages
      }
      
      List(settings.singleKeyboardPackages, id: \.id) { package in
        ForEach(package.keyboards) { keyboard in
          DisclosureGroup(isExpanded: Binding(
            get: { self.expandedPackageID == package.id },
            // setter handles when the chevron arrow is clicked by the user
            // $0 = true when disclosure group is open and $0 = false when disclosure group is closed
            set: { self.expandedPackageID = $0 ? package.id : nil }
          )) {
            KeyboardInfoView(package: package)
          } label: {
            HStack {
              
              Text(keyboard.name)
                .font(.title)
              
              Spacer()
              
              // see keyboard help button
              IconButtonView(action: { print("Show help") }, systemImage: "questionmark.circle", font: .title2, helpText: "Show help")
              
              // delete keyboard button
              IconButtonView(action: { showDeleteAlert(for: package) }, systemImage: "trash", font: .title2, helpText: "Delete keyboard")
            }
            .contentShape(Rectangle())
            // handles when the HStack is clicked by the user
            .onTapGesture {
              withAnimation {
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
      // binds the visibilty state to the alert builder
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
      .padding([.leading, .trailing, .bottom])
    }
  }
}

#Preview {
  let settings = SettingsContainer()
  MainConfigView()
    .environmentObject(settings)
}
