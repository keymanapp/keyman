/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-23
 *
 * View shown when About Keyman Configuration is selected from the menu.
 */

import SwiftUI
import KeymanSettings

struct AboutPanelView: View {
  
  var body: some View {
    VStack(spacing: 20) {
      HStack(alignment: .top, spacing: 28) {
        Image(nsImage: NSApp.applicationIconImage)
          .resizable()
          .aspectRatio(contentMode: .fit)
          .frame(width: 104, height: 104)
        
        VStack(alignment: .leading, spacing: 6) {
          Text("Keyman Configuration")
            .font(.system(size: 32, weight: .semibold))
          Text("Version \(ConfigAppUtil.configAppVersion())")
            .font(.system(size: 13))
            .foregroundStyle(.secondary)
          
          Spacer(minLength: 14)
          
          Text("Copyright © SIL Global")
            .font(.system(size: 11))
            .foregroundStyle(.secondary)
        }
        
        Spacer(minLength: 0)
      }
      
      HStack {
        Spacer()
        Button("License Agreement") {
          if let url = Bundle.main.url(forResource: "keyman-for-mac-os-license", withExtension: "html") {
            NSWorkspace.shared.open(url)
          }
        }
      }
    }
    .padding(.top, 32)
    .padding(.horizontal, 42)
    .padding(.bottom, 20)
    .frame(width: 570, height: 200)
  }
}
#Preview {
  AboutPanelView()
}
