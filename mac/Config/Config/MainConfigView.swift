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
  // visibilty state for the add package sheet
  @State private var isShowingSheet = false
  // used to identify the expanded KeymanPackage id
  // both single and multi package views share the same state variable so only single disclosure group is expanded at once
  @State private var expandedPackageID: UUID? = nil
  
  var body: some View {
    VStack {
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
      ScrollView {
        VStack {
          // the view  for single keyboard packages
          PackageRowView(packages: settings.singleKeyboardPackages, isSingleKeyboardPackage: true, expandedPackageID: $expandedPackageID)
          
          // the view  for multi keyboard packages
          PackageRowView(packages: settings.multiKeyboardPackages, isSingleKeyboardPackage: false, expandedPackageID: $expandedPackageID)
        }
        .padding()
        .background(.quaternary)
        .clipShape(RoundedRectangle(cornerRadius: 12, style: .continuous))
        
        // the Spacer pushes the contents of the VStack to the top of the VStack
        Spacer()
      }
    }
    .padding([.leading, .trailing, .bottom])
  }
}

#Preview {
  var settings = SettingsContainer()
  MainConfigView()
    .environmentObject(settings)
}
