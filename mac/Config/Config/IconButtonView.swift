/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-07-03
 *
 * The view used for image-only buttons
 * The view relies on the automatically sythesized memberwise initializer
 *
 * Parameters:
 * - action: The closure to perform when the button is clicked
 * - systemImage: The String used for the SF Symbol to display
 * - font: The Font used for the image
 * - helpText: The String used for assistive technologies (e.g. VoiceOver) and shown as a tooltip
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
