/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-04-02
 *
 * Subview to display list of keyboards for a package
 */

import SwiftUI
import KeymanSettings
import Combine

struct KeyboardListView: View {
  @EnvironmentObject var settings: SettingsContainer
  @State var packageId: UUID
  @State var keyboards: [Keyboard]

  var body: some View {
    VStack(alignment: .leading, spacing: 6) {
      ForEach($keyboards) { $keyboard in
        HStack {
          Toggle("", isOn: Binding(
            get: { settings.isKeyboardEnabled(packageId: packageId, keyboardId: keyboard.keyboardId) },
            set: { newValue in settings.setKeyboardEnabled(packageId: packageId, keyboardId: keyboard.keyboardId, enabled: newValue)
              settings.objectWillChange.send() }
          ))
          Text(keyboard.keyboardId)
            .padding(.leading, 5)
          Spacer()
        }
      }
    }
  }
}
