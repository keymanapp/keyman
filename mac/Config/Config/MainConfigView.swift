/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 */

import SwiftUI
import KeymanSettings
import OSLog

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
  
  // item being targeted for deletion
  @State private var idToDelete: UUID? = nil

  private var packageNameToDelete: String {
    guard let uuid = idToDelete else { return "this item" }
    guard let package = settings.findInstalledPackage(with: uuid) else { return "this item" }
    return package.packageName
  }

  @Environment(\.colorScheme) var colorScheme
  var canvasColor: Color {
      colorScheme == .dark ? Color(white: 0.12) : Color(white: 0.94)
  }
  var cardColor: Color {
      colorScheme == .dark ? Color(white: 0.20) : Color(.white)
  }

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
          AddKeyboardView()
            // disable escape key for closing view to avoid issues with canceling downloads
            .interactiveDismissDisabled(true)
            .frame(minWidth: 800, minHeight: 600)
       }
        
        List {
          // the view  for single keyboard packages
          PackageRowView(packages: settings.singleKeyboardPackages, isSingleKeyboardPackage: true, expandedPackageID: $expandedPackageID,
                         idToDelete: $idToDelete, showHelpTab: { url in
          showHelpTab(for: url)})
          
          // the view  for multi keyboard packages
          PackageRowView(packages: settings.multiKeyboardPackages, isSingleKeyboardPackage: false, expandedPackageID: $expandedPackageID,
                         idToDelete: $idToDelete, showHelpTab: { url in
            showHelpTab(for: url) })
        }
        .listStyle(.inset)
        
        // confirmation dialog for deleting a package
        
        .confirmationDialog(
          "Are you sure you want to delete the Keyman package '\(packageNameToDelete)'?",
          isPresented: Binding(
            get: { idToDelete != nil },
            set: { if !$0 { idToDelete = nil } }
          ),
          titleVisibility: .visible
        ) {
          Button("Delete", role: .destructive) {
            if let uuid = idToDelete {
              Logger.app.info("deleting package.id: \(uuid)")

              // use multiple expanded states?
              //expandedStates.removeValue(forKey: uuid)
              
              withAnimation(.easeInOut(duration: 0.3)) {
                expandedPackageID = nil
                settings.removeInstalledPackage(with: uuid)
              }
            }
            idToDelete = nil // dismiss safely
          }
          
          Button("Cancel", role: .cancel) {
            idToDelete = nil
          }
        }
        
        // drag and drop
        
        // highlight border with accent color when hovering over view
        .overlay(RoundedRectangle(cornerRadius: 10).stroke(Color.accentColor, lineWidth: 2).opacity(isHovering ? 1 : 0))
        .animation(.easeInOut(duration: 0.2), value: isHovering)
        // accepts URL drops
        .dropDestination(for: URL.self) { urls, _ in
          // reject drop if it is more than one file
          guard let droppedFileUrl = urls.first, urls.count < 2 else {
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
        
        // alert to indicate failed package installation
        
        .alert("Package Installation Failed", isPresented: $isShowingDropKmpAlert) {
            Button("OK", role: .cancel) { }
        } message: {
            Text(alertMessage)
        }
        
        // package installation confirmation, displays readme contents for package
        
        .sheet(item: $packageInstallHelper) { helper in
          PackageConfirmationView(installHelper: helper) { accepted in
            
            // close PackageConfirmationView sheet before updating list
            packageInstallHelper = nil

            if accepted {
              Logger.app.info("installing validated package: \(helper.packageName ?? "unknown package", privacy: .public)")
              do {
                try settings.installPackage()
              } catch {
                self.alertMessage = error.localizedDescription
                self.isShowingDropKmpAlert = true
                Logger.app.error("failed to install package: \(helper.packageName ?? "unknown package", privacy: .public), error: \(error as NSError, privacy: .public)")
              }
            } else {
              settings.userCanceledPackageInstallation()
            }
          }
          // disable escape key for closing view to avoid issues with canceling downloads
          .interactiveDismissDisabled(true)
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
