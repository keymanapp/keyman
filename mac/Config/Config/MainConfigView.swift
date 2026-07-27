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
import Combine
import KeymanSettings

struct MainConfigView: View {
  @EnvironmentObject var settings: SettingsContainer
  // visibilty state for the add package sheet
  @State private var isShowingSheet = false
  // visibilty state for the delete package alert
  @State private var isShowingDeleteAlert = false
  // used to identify the selected KeymanPackage for the delete package alert
  @State private var selectedPackage: KeymanPackage? = nil
  // used to identify the expanded KeymanPackage id
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
      // the add keyboard button
      LabelButtonView(
        action: { isShowingSheet = true },
        label: "Add Keyboard",
        systemImage: "plus",
        font: .title2
      )
        .clipShape(.capsule)
        .padding([.top, .leading, .trailing])
      // binds the visibility state to the sheet builder
        .sheet(isPresented: $isShowingSheet) {
          InstallKeyboardView()
            .frame(width: 960, height: 390)
          // FEAT/MAC/CONFIG-WINDOW TODO: Make width and height percentages
        }
      
      List(settings.singleKeyboardPackages, id: \.id) { package in
        ForEach(package.keyboards) { keyboard in
          DisclosureGroup(isExpanded: isExpanded(package: package)) {
            // the keyboard info view is shown inside each disclosure group
            KeyboardInfoView(package: package)
          } label: {
            // the HStack is shown as the label for each disclosure group
            HStack {
              
              Text(keyboard.name)
                .font(.title)
              
              // the Spacer pushes the other views inside the HStack to the opposite edge
              Spacer()
              
              // the toggle button for the keyboard
              Toggle("enabled", isOn: isEnabled(packageId: package.id, keyboardKey: keyboard.keyboardKey))
                .labelsHidden()
                .toggleStyle(.switch)
              
              // see keyboard help button
              IconButtonView(
                action: { print("Show help") },
                systemImage: "questionmark.circle",
                font: .title2,
                helpText: "Show help"
              )
              
              // delete keyboard button
              IconButtonView(
                action: { showDeleteAlert(for: package) },
                systemImage: "trash",
                font: .title2,
                helpText: "Delete keyboard"
              )
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
        // cancel button
        Button("Cancel", role: .cancel) { }
        // delete button
        Button("Delete", role: .destructive) {
          settings.removeInstalledPackage(with: package.id)
        }
      } message: { package in
        Text("You can't undo this action.")
      }
      .padding([.leading, .trailing, .bottom])
    }
  }
  
  // the view for buttons with a label
  public struct LabelButtonView: View {
    let action: () -> Void
    let label: String
    let systemImage: String
    let font: Font
    
    public var body: some View {
      Button(action: action) {
        Label(label, systemImage: systemImage)
          .font(font)
          .buttonStyle(.bordered)
      }
    }
  }
  
  // the helper method to generate the custom binding for whether a package's disclosure group is expanded or not
  func isExpanded(package: KeymanPackage) -> Binding<Bool> {
    Binding(
      // the getter renders the position of the disclosure group
      get: { self.expandedPackageID == package.id },
      // setter handles when the chevron arrow is clicked by the user
      // $0 = true when disclosure group is open and $0 = false when disclosure group is closed
      set: { self.expandedPackageID = $0 ? package.id : nil }
    )
  }
  
  // the helper method to generate the custom binding for whether a keyboard is enabled or not
  func isEnabled(packageId: UUID, keyboardKey: String) -> Binding<Bool> {
    Binding(
      // the getter renders the state of the toggle button based on the enabled property of the keyboard
      get: { settings.isKeyboardEnabled(packageId: packageId, keyboardKey: keyboardKey) },
      // the setter handles when the toggle button is clicked by the user
      // $0 = true when the toggle button is on and $0 = false when the toggle button is off
      set: {
        settings.setKeyboardEnabled(packageId: packageId, keyboardKey: keyboardKey, enabled: $0)
        settings.objectWillChange.send()
      }
    )
  }
  
}

#Preview {
  let settings = SettingsContainer()
  MainConfigView()
    .environmentObject(settings)
}
