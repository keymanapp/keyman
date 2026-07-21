/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-07-20
 *
 *
 * FEAT/MAC/CONFIG-WINDOW TODO: Finish writing file summary
 */

import SwiftUI
import KeymanSettings

public struct KeyboardInfoView: View {
  let package: KeymanPackage
  
  public init(for package: KeymanPackage) {
    self.package = package
  }
  
  public var body: some View {
    HStack {
      // the custom package image
      if let packageImage = package.graphicImage {
        Image(nsImage: packageImage)
          .resizable()
          .frame(width: 140, height: 250)
          .border(Color.red, width: 1)
      }
      // the package properties presented as text
      VStack (alignment: .leading, spacing: 10) {
        Text("Keyboard Version: \(package.packageVersion)")
        Text("Fonts:")
        ForEach(package.fonts, id: \.self) { font in
          Text(font)
        }
        Text("Author: \(package.author ?? "")")
        Text("Copyright: \(package.copyright ?? "")")
        // FEAT/MAC/CONFIG-WINDOW TODO: Change text to link with Author's Website
        Text("Author's Website")
      }
      .border(Color.blue, width: 1)
      // the package QR Code
      // FEAT?MAC?CONFIG-WINDOW TODO: Make size variable
      if let qrCode = package.generateSharePackageQRCode(size: 200) {
        Image(nsImage: qrCode)
          .interpolation(.none) // important: ensures the edges of the QR Code remain sharp
          .resizable()
          .frame(width: 200, height: 200)
          .background(Color.white) // ensures good contrast for scanning
          .border(Color.green, width: 1)
      }
    }
  }
}
