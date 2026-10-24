/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-02
 *
 * View used to display a simple continue or close button.
 */

import SwiftUI
import Combine
import OSLog

enum ButtonAction {
  case advance
  case dismiss
  case dismissAndOpenConfigView
}

struct NavigationButton: View {
  @Environment(\.dismiss) private var dismiss
  @Environment(\.openWindow) private var openWindow
  @EnvironmentObject var installation: InstallationContainer
  
  var action: ButtonAction = .advance
  var onContinue: () -> Void = {}
  
  var body: some View {
    Button {
      switch action {
      case .advance:
        withAnimation(.smooth) {
          onContinue()
        }
      case .dismiss:
        dismiss()
      case .dismissAndOpenConfigView:
        Logger.app.debug("dismiss and open config window")
        dismiss()
        openWindow(id: "config")
      }
    } label: {
      switch action {
      case .advance:
        Text("Continue")
          .padding(.horizontal, 16)
          .padding(.vertical, 4)
      case .dismiss, .dismissAndOpenConfigView:
        Text("Close")
          .padding(.horizontal, 16)
          .padding(.vertical, 4)
      }
    }
    .clipShape(Capsule())
  }
}
