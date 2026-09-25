/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-02
 *
 * View used to display a simple continue or close button.
 */

import SwiftUI
import Combine

enum ButtonAction {
  case advance
  case dismiss
}

struct NavigationButton: View {
  @Environment(\.dismiss) private var dismiss
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
      }
    } label: {
      switch action {
      case .advance:
        Text("Continue")
          .padding(.horizontal, 16)
          .padding(.vertical, 4)
      case .dismiss:
        Text("Close")
          .padding(.horizontal, 16)
          .padding(.vertical, 4)
      }
    }
    .clipShape(Capsule())
  }
}
