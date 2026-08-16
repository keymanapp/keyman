/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * View for debugging Keyman configuration
 */

import SwiftUI
import KeymanSettings

struct ConfigDebugView: View {
  @EnvironmentObject var settings: SettingsContainer
  @State private var isShowingSheet = false
  
  // for drag and drop package installation
  @State private var dropError: DropKmpError?
  @State private var isShowingDropKmpAlert = false
  @State private var alertMessage = ""
  @State private var isHovering = false

  var body: some View {
    VStack {
      HStack {
        Image(systemName: "keyboard")
          .imageScale(.large)
          .foregroundColor(.accentColor)
        Text("multiple keyboard package count = \(settings.multiKeyboardPackages.count)")
        Text("single keyboard package count = \(settings.singleKeyboardPackages.count)")
        Button("log defaults") {
          settings.logUserDefaults()
        }
        Button("clear defaults") {
          settings.clearUserDefaults()
        }
        Button("install keyboard") {
          isShowingSheet = true
        }
        Spacer()
      }
      .padding()
      .frame(width: 700, height: 100)
      // Binds the visibility state to the sheet builder
      .sheet(isPresented: $isShowingSheet) {
        InstallKeyboardView()
          .presentationDetents([.medium, .large])
          .frame(width: 700, height: 500)
      }

      VStack {
        Text("Drag a single .kmp archive here")
          .font(.system(.body, design: .monospaced))
          .multilineTextAlignment(.center)
          .padding()
          .frame(width: 350, height: 180)
          .background(Color(NSColor.controlBackgroundColor))
          .cornerRadius(10)
          .overlay(
            RoundedRectangle(cornerRadius: 10)
              .stroke(isHovering ? Color.accentColor : Color.gray, lineWidth: 2)
          )
        // Accept URL drops
          .dropDestination(for: URL.self) { urls, _ in
            // reject drop if it is more than one file
            guard let droppedFileUrl = urls.first, urls.count == 1 else {
              let error = DropKmpError.tooManyFiles
              self.alertMessage = error.localizedDescription
              self.isShowingDropKmpAlert = true
              return false // the drop failed
            }
            do {
              try settings.processDroppedKmpFile(at: droppedFileUrl)
              return true // the drop was successful
            } catch {
              self.alertMessage = error.localizedDescription
              self.isShowingDropKmpAlert = true
              return false
              
            }
          } isTargeted: { hovering in
            isHovering = hovering
          }
      }
      .padding()
      // alert triggers automatically when $dropError becomes non-nil
      .alert("Package Installation Failed", isPresented: $isShowingDropKmpAlert) {
          Button("OK", role: .cancel) { }
      } message: {
          Text(alertMessage)
      }

      ScrollView {
        VStack(alignment: .leading, spacing: 6) {
          ForEach(Array(settings.singleKeyboardPackages.enumerated()), id: \.offset) { index, package in
            VStack {
              HStack(alignment: .center, spacing: 10) {
                Text(package.packageName)
                  .font(.headline)
                Text(package.packageVersion)
                  .font(.subheadline)
                // Example of Icon-Only Button
                Spacer()
                if let nsImage = package.graphicImage {
                  Image(nsImage: nsImage)
                    .resizable() // Allows resizing
                    .scaledToFit() // Maintains original aspect ratio
                    .frame(maxWidth: 140, maxHeight: 250) // Controls the bounds
                }
                Button(action: {
                  settings.removeInstalledPackage(with: package.id)
                }) {
                  Label("remove", systemImage: "trash.fill")
                }
                .labelStyle(.iconOnly)
                .buttonStyle(.borderless)
              }
              KeyboardListDebugView(packageId: package.id, keyboards: package.keyboards)
            }
          }
        }
        .padding(.trailing, 25) // allow space for scroll bar
      }
    }
    .padding()
  }
}

#Preview {
  let settings = SettingsContainer()
  ConfigDebugView()
    .environmentObject(settings)
}
