//
//  KeymanLogo.swift
//  Config
//
//  Created by Eli Schantz on 7/2/26.
//

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
