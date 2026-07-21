/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-26
 *
 * Main view used for configuring Keyman
 */

import SwiftUI
import KeymanSettings

struct ConfigView: View {
  @EnvironmentObject var settings: SettingsContainer
  @State private var isShowingSheet = false

  var body: some View {
    VStack {
      HStack {
        Image(systemName: "keyboard")
          .imageScale(.large)
          .foregroundColor(.accentColor)
        Text("multiple keyboard package count = \(settings.multiKeyboardPackages.count)")
        Text("single keyboard package count = \(settings.singleKeyboardPackages.count)")
        Button("debug") {
          settings.debug()
        }
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

      
      ScrollView {
        VStack(alignment: .leading, spacing: 6) {
          ForEach(Array(settings.singleKeyboardPackages.enumerated()), id: \.offset) { index, package in
            VStack {
              HStack(alignment: .center, spacing: 10) {
                VStack(spacing: 16) {
                    Text("Scan to visit website:")
                        .font(.headline)
                    
                  if let qrImage = package.generateSharePackageQRCode(size: 200) {
                        Image(nsImage: qrImage)
                            .interpolation(.none) // important: keeps the QR edges sharp
                            .resizable()
                            .frame(width: 200, height: 200)
                            .background(Color.white) // ensures good scanning contrast
                    } else {
                        Text("Failed to generate QR Code")
                            .foregroundColor(.red)
                    }
                }
                .padding()
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
                  settings.removeSingleKeyboardPackage(at: index)
                }) {
                  Label("remove", systemImage: "trash.fill")
                }
                .labelStyle(.iconOnly)
                .buttonStyle(.borderless)
              }
              KeyboardListView(packageId: package.id, keyboards: package.keyboards)
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
  ConfigView()
    .environmentObject(settings)
}
