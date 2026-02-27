/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-02-24
 *
 * Owns the data that owns all the Keyman configuration data
 * which it loads form the repository and communicates with
 * the Keyman input method when the configuration is changed
 *
 */
import SwiftUI
import Combine
import KeymanData

public class Configuration : ObservableObject {
  //@Published public var keyboardPackages: [KeymanPackage]
  @Published public var keymanData: KeymanData

  fileprivate let notificationCenter:DistributedNotificationCenter
  //fileprivate let dataRepo: DataRepository
  
  public init() {
    self.keymanData = KeymanData()
    
    //self.dataRepo = DataRepository()
    
    notificationCenter = DistributedNotificationCenter.default()
//    self.keyboardPackages = []
    
//    if let keyboardsUrl = self.pathUtil.keymanKeyboardsDirectory {
//
//      if FileManager.default.fileExists(atPath: keyboardsUrl.path) {
//        ConfigLogger.shared.testLogger.debug("directory exists: \(keyboardsUrl.absoluteString)")
//      } else {
//        ConfigLogger.shared.testLogger.debug("non-existent directory: \(keyboardsUrl.absoluteString)")
//      }
//    }
    
    // load keyboards from disk
    //let packageSourceArray = self.dataRepo.readKeyboardPackageSource()
    
    // create a KeymanPackage object for each PackageSource and insert it in the array
//    for source in packageSourceArray {
//      let package = KeymanPackage(packageSource: source)
//      self.keyboardPackages.append(package)
//    }
//    
//    ConfigLogger.shared.testLogger.debug("posting to notification center from KeyFig")
//    notificationCenter.postNotificationName(NSNotification.Name("com.keyman.removedkeyboard"), object: nil, userInfo: ["data": "khmer angkor"], deliverImmediately: false)
//
  }
}
