/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Gabriel Schantz on 2026-07-20
 *
 * The view used for keyboard info
 *
 * FEAT/MAC/CONFIG-WINDOW TODO: Finish writing file summary
 */

import SwiftUI
import KeymanSettings

public struct PackageInfoView: View {
  let package: KeymanPackage
  
  /**
   * Copies the text argument to the system clipboard
   */
  private func copyTextToClipboard (text: String) -> Void {
    let pasteboard = NSPasteboard.general
    pasteboard.clearContents()
    pasteboard.setString(text, forType: .string)
  }
  
  public var body: some View {
    
    HStack (alignment: .top) {
      
      // the custom package image
      if let packageImage = package.graphicImage {
        Image(nsImage: packageImage)
          .resizable()
          .frame(maxWidth: 84, maxHeight: 150)
      }
      
      VStack {
        // the text-based package properties presented in a grid
        Grid(horizontalSpacing: 10, verticalSpacing: 5) {
          // the package version
          GridRow {
            Text("Package Version:").bold()
              .gridColumnAlignment(.trailing) // all elements underneath inherit the .trailing alignment
            Text(package.packageVersion)
              .gridColumnAlignment(.leading) // all elements underneath inherit the .leading alignment
          }
          
          // the fonts
          GridRow {
            Text("Fonts:").bold()
            HStack {
              ForEach(package.fonts, id: \.self) { font in
                Text(font)
              }
            }
          }
          
          // the copyright
          GridRow {
            Text("Copyright:").bold()
            Text(package.copyright ?? "")
          }
          
          // the author
          GridRow {
            Text("Author:").bold()
            Text(package.author ?? "")
          }
          
          // the website
          GridRow {
            Text("Website:").bold()
            if let websiteUrl = package.websiteUrl {
              Link(destination: websiteUrl) {
                Text(websiteUrl.absoluteString)
                  .underline()
                  .multilineTextAlignment(.leading)
              }
            }
          }
        }
        .padding(5)
      }
        
      Spacer()
      
      // the package QR Code and link to share the package online
      VStack {
        let size: CGFloat = 106
        if let qrCode = package.generateSharePackageQRCode(size: size) {

          // the package QR Code
          Image(nsImage: qrCode)
            .interpolation(.none) // important: ensures the edges of the QR Code remain sharp
            .resizable()
            .frame(width: size, height: size)
            .background(Color.white) // ensures good contrast for scanning
        }
        
        if let sharePackageUrl = package.sharePackageUrl {
          HStack {
            
            // the link to share the package online
            Link(destination: sharePackageUrl) {
              Text("Share Keyboard")
                .underline()
            }
            
            // the button to copy the link to share the package online
            IconButtonView(action: { copyTextToClipboard(text: sharePackageUrl.absoluteString) }, systemImage: "doc.on.doc", font: .body , helpText: "Copy link")
          }
        }
      }
      .padding(5)
      .border(Color.black, width: 1)
    }
    .frame(height: 150)
  }
}
