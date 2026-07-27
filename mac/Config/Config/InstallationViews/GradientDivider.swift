/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-01
 *
 * View used for creating a thin divider with a Keyman color gradient.
 */

import SwiftUI

struct GradientDivider: View {
  let namespace: Namespace.ID
  var id: String = "divider"
  
  var body: some View {
    Rectangle()
      .fill(LinearGradient(
        colors: [
          Color(.keymanBlue),
          Color(.keymanOrange),
          Color(.keymanRed)
        ],
        startPoint: .leading,
        endPoint: .trailing
      ))
      .frame(height: 1)
      .opacity(0.5)
      .matchedGeometryEffect(id: id, in: namespace)
  }
}
