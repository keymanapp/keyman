/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-02
 *
 * View used to display the large Keyman logo.
 */

import SwiftUI

struct KeymanLogo: View {
  let namespace: Namespace.ID
  
  var body: some View {
    Image("KeymanLogo-BIG")
      .interpolation(.high)
      .resizable()
      .scaledToFit()
      .frame(height: 60)
      .frame(maxWidth: .infinity, alignment: .center)
      .matchedGeometryEffect(id: "title", in: namespace, anchor: .top)
  }
}
