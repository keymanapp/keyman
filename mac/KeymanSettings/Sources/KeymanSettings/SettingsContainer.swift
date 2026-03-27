import Combine

public class SettingsContainer : ObservableObject {

  @Published public var settingName: String
  @Published public var keyboardPackages: [KeymanPackage]
  @Published public var activeKeyboards: Set<String>
  @Published public var currentKeyboard: String

  fileprivate let dataRepository: PackageRepository
  fileprivate let settingsRepository: SettingsRepository

  public init() {
    settingName = "uninitialized"
    self.dataRepository = PackageRepository()
    self.settingsRepository = SettingsRepository(suiteName: KeymanPaths.groupId)
    
    self.activeKeyboards = self.settingsRepository.readActiveKeyboards()
    self.currentKeyboard = self.settingsRepository.readSelectedKeyboard()
    
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
      // TODO: remove test code
      self.dataRepository.writeSomethingToContainer()

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
