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
  
  /**
   * Copies the text argument to the system clipboard
   */
  private func copyTextToClipboard (text: String) -> Void {
    let pasteboard = NSPasteboard.general
    pasteboard.clearContents()
    pasteboard.setString(text, forType: .string)
  }
  
  public var body: some View {
    
    HStack {
      
      // the custom package image
      if let packageImage = package.graphicImage {
        Image(nsImage: packageImage)
          .resizable()
          .frame(width: 87.92, height: 157) // Corresponds to max height of package properties presented as text and maintains width:height ratio of 140:250
      }
      
      // the package properties presented as text
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
          HStack{
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
      .frame(minWidth: 350, minHeight: 125)
      .padding()
      
      Spacer()
      
      // the package QR Code and link to share the package online
      // FEAT?MAC?CONFIG-WINDOW TODO: Make size variable
      VStack {
        
        if let qrCode = package.generateSharePackageQRCode(size: 113) {
          Image(nsImage: qrCode)
            .interpolation(.none) // important: ensures the edges of the QR Code remain sharp
            .resizable()
            .frame(width: 113, height: 113)
            .background(Color.white) // ensures good contrast for scanning
        }
        
        if let sharePackageUrl = package.sharePackageUrl {
          
          HStack {
            
            Link(destination: sharePackageUrl) {
              Text("Share Keyboard")
                .underline()
            }
            IconButtonView(action: { copyTextToClipboard(text: sharePackageUrl.absoluteString) }, systemImage: "doc.on.doc", font: .body , helpText: "Copy link")
          }
        }
      }
      .padding(5)
      .border(Color.black, width: 1)
      //.padding([.top, .bottom])
    }
  }
}
