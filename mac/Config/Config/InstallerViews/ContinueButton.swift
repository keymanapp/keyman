//
//  ContinueButton.swift
//  Config
//
//  Created by Eli Schantz on 7/2/26.
//

import SwiftUI

struct ContinueButton: View {
  @Binding var currentPage: NewInstallView.InstallPage
  var nextPage : NewInstallView.InstallPage
  
    var body: some View {
      Button {
          withAnimation(.smooth(duration: 0.5)) {
              currentPage = nextPage
          }
      } label: {
          Text("Continue")
              .padding(.horizontal, 16)
              .padding(.vertical, 4)
      }
      .clipShape(Capsule())
    }
}
