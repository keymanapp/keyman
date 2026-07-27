/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-07-03
 *
 * The view used for image-only buttons
 */

import SwiftUI

public struct IconButtonView: View {
  let action: () -> Void
  let systemImage: String
  let font: Font
  let helpText: String
  
  public var body: some View {

    Button {
      action()
    } label: {
      Image(systemName: systemImage)
        .font(font)
    }
    .buttonStyle(.bordered)
    .clipShape(.circle)
    .accessibilityLabel(helpText)
    .help(helpText)
    
  }
}
