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
  
  var body: some View {
    VStack{
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
      
      List {
        Section {
          // the view  for single keyboard packages
          PackageRowView(packages: settings.singleKeyboardPackages, isSingleKeyboardPackage: true)
        }
        
        Section {
          // the view  for multi keyboard packages
          PackageRowView(packages: settings.multiKeyboardPackages, isSingleKeyboardPackage: false)
        }
      }
    }
    .padding([.leading, .trailing, .bottom])
  }
}

#Preview {
  let settings = SettingsContainer()
  MainConfigView()
    .environmentObject(settings)
}
