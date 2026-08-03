/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-07-03
 *
 * Extracted views used to compose smaller views in MainConfigView
 * TODO: Finish writing file summary
 */

import SwiftUI

public struct PackageButtonView: View {
  let action: () -> Void
  let label: String
  let systemImage: String
  let helpText: String
  
  public var body: some View {
    
    Button {
      action()
    } label: {
      Label(label, systemImage: systemImage)
        .labelStyle(.iconOnly)
        .font(.title2)
    }
    .buttonStyle(.bordered)
    .clipShape(.circle)
    .contentShape(Capsule())
    // feat/mac/config-window TODO: Decide proper help text for usages of PackageButtonView...
    .help(helpText)
    
  }
}
