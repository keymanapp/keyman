import SwiftUI
import KeymanSettings

struct InstallKeyboardView: View {
  @EnvironmentObject var settings: SettingsContainer
  @Environment(\.dismiss) private var dismiss
  
  var body: some View {
    VStack {
      KeyboardSearchView()
        .environmentObject(settings)
        .padding()
    }
    .toolbar {
      // Placement determines where on the bar it sits
      ToolbarItem(placement: .cancellationAction) {
        Button("Close") {
          dismiss()
        }
      }
    }
  }
}
