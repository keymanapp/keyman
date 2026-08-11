/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-07-27
 *
 * View for a package row
 */

import SwiftUI
import Combine
import KeymanSettings

public struct PackageRowView: View {
  
  @EnvironmentObject var settings: SettingsContainer
  // visibilty state for the delete package alert
  @State private var isShowingDeleteAlert = false
  // used to identify the selected KeymanPackage for the delete package alert
  @State private var selectedPackage: KeymanPackage? = nil
  
  // settings.singleKeyboardPackages or settings.multiKeyboardPackages
  let packages: [KeymanPackage]
  // a boolean for weather or not a package contains multiple keyboards
  let isSingleKeyboardPackage: Bool
  // binded to the shared state variable in the parent view
  @Binding var expandedPackageID: UUID?
  // closure passed from the parent view
  let showHelpTab: (URL) -> Void
  
  init(packages: [KeymanPackage], isSingleKeyboardPackage: Bool, expandedPackageID: Binding<UUID?>, showHelpTab: @escaping (URL) -> Void) {
    self.packages = packages
    self.isSingleKeyboardPackage = isSingleKeyboardPackage
    self._expandedPackageID = expandedPackageID
    self.showHelpTab = showHelpTab
  }
  
  /**
   * Sets isShowingDeleteAlert to true and assigns the state variable selectedPackage the KeymanPackage argument
   */
  public func showDeleteAlert(for package: KeymanPackage) {
    isShowingDeleteAlert = true
    selectedPackage = package
  }
  
  public var body: some View {
    ForEach(packages, id: \.id) { package in
      ForEach(isSingleKeyboardPackage ? package.keyboards : package.keyboards.onlyFirst) { keyboard in
        DisclosureGroup(isExpanded: isExpanded(package: package)) {
          // the package info view is shown inside each disclosure group
          if expandedPackageID == package.id {
            PackageInfoView(package: package, showAlertFunction: { package in
              showDeleteAlert(for: package)
            })
              .transition(.move(edge: .top))
          }
        } label: {
          // a VStack is shown as the label for each disclosure group
          VStack (alignment: .leading, spacing: 0) {
            HStack {
              // if the package contains one keyboard, show the keyboard name, otherwise show the package name
              Text(isSingleKeyboardPackage ? keyboard.name: package.packageName)
                .font(.title)
              
              // see keyboard help button
              if let url = package.helpFileUrl {
                IconButtonView(
                  action: { showHelpTab(url) },
                  systemImage: "questionmark.circle",
                  font: .title2,
                  helpText: "Show keyboard help"
                )
              }
              
              // the Spacer pushes the contents of the HStack to the either edge
              Spacer()
              
              // if the package contains one keyboard shows the toggle button for the keyboard
              if isSingleKeyboardPackage {
                Toggle("enabled", isOn: isEnabled(packageId: package.id, keyboardKey: keyboard.keyboardKey))
                  .controlSize(.mini)
                  .labelsHidden()
                  .toggleStyle(.switch)
                  .gridColumnAlignment(.leading)
              }
              
              
            }
            
            // if the package contains multiple keyboards shows an HStack with the keyboard name and toggle button for each keyboard in the package
            if !isSingleKeyboardPackage {
              ForEach (package.keyboards) { keyboard in
                HStack {
                  Text(keyboard.name)
                    .font(.title2)
                    .foregroundStyle(.primary)
                    .gridColumnAlignment(.leading)
                  
                  // the Spacer pushes the other views inside the HStack to the opposite edge
                  Spacer()
                  
                  // the toggle button for the keyboard
                  Toggle("enabled", isOn: isEnabled(packageId: package.id, keyboardKey: keyboard.keyboardKey))
                    .controlSize(.mini)
                    .labelsHidden()
                    .toggleStyle(.switch)
                    .gridColumnAlignment(.leading)
                }
              }
            }
          }
          .contentShape(Rectangle())
          // handles when the HStack is clicked by the user
          .onTapGesture {
            withAnimation () {
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
  }
  
  // the helper method to generate the custom binding for whether a package's disclosure group is expanded or not
  func isExpanded(package: KeymanPackage) -> Binding<Bool> {
    Binding(
      // the getter renders the position of the disclosure group
      get: { expandedPackageID == package.id },
      // setter handles when the chevron arrow is clicked by the user
      // $0 = true when disclosure group is open and $0 = false when disclosure group is closed
      set: { isExpanded in
        withAnimation {
        expandedPackageID = isExpanded ? package.id : nil }
      }
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

extension Collection {
  // returns the first element of an array in an array or returns an empty array
  var onlyFirst: [Element] {
    guard let first = self.first else { return [] }
    return [first]
  }
}
