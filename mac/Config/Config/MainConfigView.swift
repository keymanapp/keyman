/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 * FEAT/MAC/CONFIG-WINDOW TODO: Set default width and height for window
 */

import SwiftUI
import KeymanSettings

struct MainConfigView: View {
  
  @EnvironmentObject var settings: SettingsContainer
  // visibilty state for the add package sheet
  @State private var isShowingSheet = false
  // used to identify the expanded KeymanPackage id
  // both single and multi package views share the same state variable so only a single disclosure group is expanded at once
  @State private var expandedPackageID: UUID? = nil
  @State private var selectedTab = 0
  @State private var packageSelectedForHelpUrl: URL? = nil
  
  /**
   * Assigns packageSelectedForHelpUrl the url argument and changes the selected tab to the help tab
   */
  public func showHelpTab(for url: URL) {
    packageSelectedForHelpUrl = url
    selectedTab = 1
  }
  
  var body: some View {
    TabView (selection: $selectedTab) {
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
        
        Form {
          // the view  for single keyboard packages
          PackageRowView(packages: settings.singleKeyboardPackages, isSingleKeyboardPackage: true, expandedPackageID: $expandedPackageID, showHelpTab: { url in
          showHelpTab(for: url)})
          
          // the view  for multi keyboard packages
          PackageRowView(packages: settings.multiKeyboardPackages, isSingleKeyboardPackage: false, expandedPackageID: $expandedPackageID, showHelpTab: { url in
            showHelpTab(for: url) })
        }
        .formStyle(.grouped)
        
        // the Spacer pushes the contents of the VStack to the top of the VStack
        Spacer()
      }
      .padding([.leading, .trailing, .bottom])
      .tabItem { Text("Keyboards") }
      .tag(0)
      
      if let url = packageSelectedForHelpUrl {
        HelpView(helpFileURL: url)
          .padding()
          .tabItem { Text("Help") }
          .tag(1)
      } else {
        Text("Help not available.")
          .font(.title)
          .tabItem { Text("Help") }
          .tag(1)
      }
    }
  }
}

#Preview {
  let settings = SettingsContainer()
  MainConfigView()
    .environmentObject(settings)
}
