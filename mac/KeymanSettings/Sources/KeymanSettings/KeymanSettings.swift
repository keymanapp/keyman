import Combine

public class KeymanSettings : ObservableObject {

  @Published public var settingName: String
  @Published public var keyboardPackages: [KeymanPackage]
  
  fileprivate let dataRepository: PackageRepository
  
  public init() {
    settingName = "uninitialized"
    self.dataRepository = PackageRepository()
    
    self.keyboardPackages = []
    
    //    if let keyboardsUrl = self.pathUtil.keymanKeyboardsDirectory {
    //
    //      if FileManager.default.fileExists(atPath: keyboardsUrl.path) {
    //        print("directory exists: \(keyboardsUrl.absoluteString)")
    //      } else {
    //        print("non-existent directory: \(keyboardsUrl.absoluteString)")
    //      }
    //    }
    
    // load keyboards from disk
    if (self.dataRepository.keyman19SharedDataDirectoryExists()) {
      let packageSourceArray = self.dataRepository.readKeymanPackagesForKeyman19()
      
      // create a KeymanPackage object for each PackageSource object and insert it in the array
      for source in packageSourceArray {
        let package = KeymanPackage(packageSource: source)
        self.keyboardPackages.append(package)
      }
    } else {
      self.dataRepository.createKeyman19SharedDataDirectories()
    }
  }
}
