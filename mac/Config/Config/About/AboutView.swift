//
//  AboutView.swift
//  Config
//
//  Created by Eli Schantz on 7/23/26.
import SwiftUI
import KeymanSettings

struct AboutView: View {
  @Environment(\.dismiss) private var dismiss
  let namespace: Namespace.ID
  var body: some View {
    HStack(alignment: .top) {
      Image(nsImage: NSApp.applicationIconImage)
        .interpolation(.high)
        .resizable()
        .frame(width: 120, height: 120)
        .padding(.horizontal, 35)
        .padding(.top, 15)
      
      VStack() {
        
        Text("Keyman")
          .font(.system(size: 40))
          .frame(maxWidth: .infinity, alignment: .leading)
        
        Text("Version \(ConfigAppUtil.configAppVersion())")
          .foregroundStyle(.secondary)
          .frame(maxWidth: .infinity, alignment: .leading)
        
        GradientDivider(namespace: namespace)
        
        Text("© SIL Global")
          .foregroundStyle(.secondary)
          .frame(maxWidth: .infinity, alignment: .leading)
        
        Spacer()
        
        HStack (spacing: 12){
          
          Spacer()
          
          Button("License Agreement"){
            if let url = Bundle.main.url(forResource: "keyman-for-mac-os-license", withExtension: "html") {
              NSWorkspace.shared.open(url)
            }
            
          }
          
          Button("Close"){
            dismiss()
          }
        }
      }
      .padding(.trailing, 20)
      .padding(.bottom, 20)
    }
    .frame(width: 535, height: 185)
  }
}


#Preview {
  AboutViewPreview()
}

private struct AboutViewPreview: View {
  @Namespace private var namespace
  
  var body: some View {
    AboutView(namespace: namespace)
  }
}
