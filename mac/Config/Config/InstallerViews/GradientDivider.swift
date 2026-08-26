//
//  GradientDivider.swift
//  Config
//
//  Created by Eli Schantz on 7/1/26.
//
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
