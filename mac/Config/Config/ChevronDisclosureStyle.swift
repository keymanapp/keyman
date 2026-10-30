/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-10-26
 *
 * A style defined to present disclosure groups consistently across different versions of macOS
 */

import SwiftUI

struct ChevronDisclosureStyle: DisclosureGroupStyle {
  func makeBody(configuration: Configuration) -> some View {
    VStack(alignment: .leading, spacing: 0) {
      HStack(alignment: .firstTextBaseline) {
        Image(systemName: "chevron.right")
          .rotationEffect(.degrees(configuration.isExpanded ? 90 : 0))
          .frame(width: 16, height: 16)
        configuration.label
      }
      .contentShape(Rectangle())          // whole header, including the Spacer, gets clicks
      .onTapGesture {
        withAnimation(.easeInOut(duration: 0.25)) {
          configuration.isExpanded.toggle()
        }
      }
      
      if configuration.isExpanded {
        configuration.content
          .padding(.top, 8)
          .transition(.opacity)
      }
    }
    .clipped()
  }
}
