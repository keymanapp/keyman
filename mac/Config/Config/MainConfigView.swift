/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 * MAC-CONFIG-TODO: Set default width and height for window
 */

import SwiftUI
import KeymanSettings

struct MainConfigView: View {
  
  @EnvironmentObject var settings: SettingsContainer
  // visibilty state for the add package sheet
  @State private var isShowingAddKeyboardSheet = false
  // used to identify the expanded KeymanPackage id
  // both single and multi package views share the same state variable so only a single disclosure group is expanded at once
  @State private var expandedPackageID: UUID? = nil
  @State private var selectedTab = 0
  @State private var packageSelectedForHelpUrl: URL? = nil
  
  // for drag and drop package installation
  @State private var packageInstallHelper: PackageInstallHelper? = nil
  @State private var isShowingDropKmpAlert = false
  @State private var alertMessage = ""
  @State private var isHovering = false

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
          action: { isShowingAddKeyboardSheet = true },
          label: "Add Keyboard",
          systemImage: "plus",
          font: .title2
        )
        .clipShape(.capsule)
        .padding([.top, .leading, .trailing])
        // binds the visibility state to the sheet builder
        .sheet(isPresented: $isShowingAddKeyboardSheet) {
          InstallKeyboardView()
            .frame(width: 960, height: 390)
          // MAC-CONFIG-TODO: Make width and height percentages
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
        // highlight border with accent color when hovering over view
        .overlay(RoundedRectangle(cornerRadius: 10).stroke(Color.accentColor, lineWidth: 2).opacity(isHovering ? 1 : 0))
        .animation(.easeInOut(duration: 0.2), value: isHovering)
        // accepts URL drops
        .dropDestination(for: URL.self) { urls, _ in
          // reject drop if it is more than one file
          guard let droppedFileUrl = urls.first, urls.count == 1 else {
            let error = DropKmpError.tooManyFiles
            self.alertMessage = error.localizedDescription
            self.isShowingDropKmpAlert = true
            return false // the drop failed
          }
          do {
            packageInstallHelper = try settings.initiateKmpFileInstallation(at: droppedFileUrl)
            return true // the drop was successful
          } catch {
            self.alertMessage = error.localizedDescription
            self.isShowingDropKmpAlert = true
            return false
          }
        } isTargeted: { hovering in
          isHovering = hovering
        }
        // alert triggers automatically when $isShowingDropKmpAlert is true
        .alert("Package Installation Failed", isPresented: $isShowingDropKmpAlert) {
            Button("OK", role: .cancel) { }
        } message: {
            Text(alertMessage)
        }
        .sheet(item: $packageInstallHelper) { helper in
          PackageInstallView(installHelper: helper) { accepted in
            if accepted {
              print("Processing validated package: \(helper.packageName ?? "unknown package")")
              do {
                try settings.installPackage()
              } catch {
                print("failed to install package: \(helper.packageName ?? "unknown package") with error: \(error.localizedDescription)")
              }
            } else {
              settings.userCanceledPackageInstallation()
            }
            packageInstallHelper = nil
          }
        }

        // the Spacer pushes the contents of the VStack to the top of the VStack
        Spacer()
      }
      .padding([.leading, .trailing, .bottom])
      .tabItem { Text("Keyboards") }
      .tag(0)
      
      if let url = packageSelectedForHelpUrl {
        PackageContentWebView(packageFileUrl: url)
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
