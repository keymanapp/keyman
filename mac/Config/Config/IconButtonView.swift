/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-07-03
 *
 * View for icon buttons
 * FEAT/MAC/CONFIG-WINDOW TODO: Finish writing file summary
 */

import SwiftUI

public struct IconButtonView: View {
  let action: () -> Void
  let label: String
  let systemImage: String
  let helpText: String
  let font: Font
  
  public var body: some View {
    
    Button {
      action()
    } label: {
      Label(label, systemImage: systemImage)
        .labelStyle(.iconOnly)
        .font(font)
    }
    .buttonStyle(.bordered)
    .clipShape(.circle)
    .contentShape(Capsule())
    // FEAT/MAC/CONFIG-WINDOW TODO: Decide proper help text for usages of PackageButtonView...
    .help(helpText)
    
  }
}
