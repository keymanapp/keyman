//
//  ContinueButton.swift
//  Config
//
//  Created by Eli Schantz on 7/2/26.
//

import SwiftUI
import Combine

struct ContinueButton: View {
    var body: some View {
        Button {
          NotificationCenter.default.post(name: Notification.Name("advancePage"), object: nil)
          
        } label: {
            Text("Continue")
                .padding(.horizontal, 16)
                .padding(.vertical, 4)
        }
        .clipShape(Capsule())
    }
}

extension Notification.Name {
    static let advancePage = Notification.Name("advancePage")
}
