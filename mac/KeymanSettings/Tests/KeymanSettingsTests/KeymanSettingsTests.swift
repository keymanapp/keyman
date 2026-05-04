import Testing
import Foundation
@testable import KeymanSettings


@Suite("Settings Container") struct SettingsContainersTests {
  
  fileprivate init() async throws {
    print("init")
  }
  
  @Test("Check settings creation ") func testSettingsCreation() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, hittiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    
    #expect(settingsContainer.installedPackages.isEmpty)
    
    settingsContainer.loadPackages()
    
    let package = try #require(settingsContainer.findPackage(packageId: packageRepo.testPackageId))

    #expect(defaultsRepo.readEnabledKeyboards().contains(moabiteKeyboardKey))
  }
  
  /**
   * include uninstalled keyboard in the list of enabled keyboards and verify that it is removed after `validateSettings()` is called
   */
  @Test("Check settings validation ") func testSettingsValidation() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, uninstalledKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)

    settingsContainer.loadPackages()
    settingsContainer.validateSettings()
    let enabledKeyboards = defaultsRepo.readEnabledKeyboards()
    
    #expect(!enabledKeyboards.contains(uninstalledKeyboardKey))
    #expect(enabledKeyboards.contains(moabiteKeyboardKey))
  }
}

/**
 * see that keyboard state changes from disabled to enabled when settings are applied
 */
@Test("Check settings applied ") func testSettingsApplied() async throws {
  let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, hittiteKeyboardKey]))
  let packageRepo = PackageRepoStub()
  let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)

  settingsContainer.loadPackages()
  let package = try #require(settingsContainer.findPackage(packageId: packageRepo.testPackageId))

  #expect(!package.isKeyboardEnabled(keyboardKey: hittiteKeyboardKey))

  settingsContainer.applySettingsToInstalledPackages()

  #expect(package.isKeyboardEnabled(keyboardKey: hittiteKeyboardKey))
}


@Suite("Check Keyman paths") struct KeymanPathsTests {
  
  fileprivate init() async throws {
    print("init")
  }
  
  @Test("Check Keyman 17 documents directory") func testKeyman17DocumentsDirectory() async throws {
    let documentsDirectory = try #require(KeymanPaths().keyman17DocumentsDirectory)
    #expect(documentsDirectory.absoluteString.hasSuffix("Documents/"))
  }
  
  @Test("Check Keyman 17 keyboards directory") func testKeyman17KeyboardsDirectory() async throws {
    #expect(true)
    let keyboardsDirectory = try #require(KeymanPaths().keyman17KeyboardsDirectory)
    #expect(keyboardsDirectory.absoluteString.hasSuffix("Documents/Keyman-Keyboards/"))
  }
  
  @Test("Check Keyman 18 support directory") func testKeyman18SupportDirectory() async throws {
    #expect(true)
    let supportDirectory = try #require(KeymanPaths().keyman18SupportDirectory)
    #expect(supportDirectory.absoluteString.hasSuffix("Library/Application%20Support/"))
  }
  
  @Test("Check Keyman 18 data directory") func testKeyman18SupportKeymanDirectory() async throws {
    #expect(true)
    let supportDirectory = try #require(KeymanPaths().keyman18DataDirectory)
    #expect(supportDirectory.absoluteString.hasSuffix("Library/Application%20Support/keyman.inputmethod.Keyman/"))
  }
  
  @Test("Check Keyman 18 keyboards directory") func testKeyman18SupportKeyboardsDirectory() async throws {
    #expect(true)
    let keyboardsDirectory = try #require(KeymanPaths().keyman18KeyboardsDirectory)
    #expect(keyboardsDirectory.absoluteString.hasSuffix("Library/Application%20Support/keyman.inputmethod.Keyman/Keyman-Keyboards/"))
  }
  
  @Test("Check Keyman 19 container data directory") func testKeyman19ContainerDataDirectory() async throws {
    #expect(true)
    let containerDirectory = try #require(KeymanPaths().keyman19ContainerDirectory)
    #expect(containerDirectory.absoluteString.hasSuffix("Group%20Containers/group.com.keyman/"))
  }
  
  @Test("Check Keyman 19 keyboards directory") func testKeyman19KeyboardsDirectory() async throws {
    #expect(true)
    let keyboardsDirectory = try #require(KeymanPaths().keyman19KeyboardsDirectory)
    #expect(keyboardsDirectory.absoluteString.hasSuffix("Group%20Containers/group.com.keyman/Library/Application%20Support/Keyman-Packages/"))
  }
}

// defaults read test.suite.name

@Suite("Settings check") struct KeymanSettingsRepositoryTests {
  var settingsRepo: DefaultsRepository
  
  fileprivate init() async throws {
    print("init Settings")
    do {
      try self.settingsRepo = DefaultsRepository(suiteName: "test.suite.name")
      print("Found group container")
    } catch UserDefaultsError.unknownSuite {
      fatalError("Group container not found.")
    } catch {
      fatalError("Unable to access settings in group container.")
    }
    
    self.settingsRepo.writeSelectedKeyboard(keyboardName: "keyboardtest")
  }
  
  @Test("Write selected keyboard") func writeSelectedKeyboard() async throws {
    settingsRepo.writeSelectedKeyboard(keyboardName: "testing")
    #expect(settingsRepo.readSelectedKeyboard() == "testing")
  }
  
  @Test("Read selected keyboard") func readSelectedKeyboard() async throws {
    #expect(settingsRepo.readSelectedKeyboard() == "keyboardtest")
  }
}

@Suite("Load KMP data") struct KeymanPackageRepositoryTests {
  var testKmpUrl: URL? = nil
  
  fileprivate init() async throws {
    print("init")
  }
  
  func getAmharicKmpUrl() -> URL? {
    return Bundle.module.url(forResource: "amharic.kmp", withExtension: "json")
  }
  
  @Test("Read package") func loadPackageSource() async throws {
    let packageRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let _ =  try #require(packageRepo.readKeyboardPackage(kmpUrl))
  }
  
  @Test("Read package name") func readPackageName() async throws {
    let packageRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(packageRepo.readKeyboardPackage(kmpUrl))
    #expect(source.packageName == "GFF Amharic Keyboard")
  }
  
  @Test("Read package version") func readPackageVersion() async throws {
    let packageRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(packageRepo.readKeyboardPackage(kmpUrl))
    #expect(source.packageVersion == "3.1.2")
  }
  
  @Test("Read copyright") func readCopyright() async throws {
    let packageRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(packageRepo.readKeyboardPackage(kmpUrl))
    #expect(source.copyright == "© Geʾez Frontier Foundation, SIL International")
  }
  
  @Test("Read keyboard count") func readKeyboardCount() async throws {
    let packageRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(packageRepo.readKeyboardPackage(kmpUrl))
    #expect(source.keyboards?.count == 1)
  }
  
  @Test("Read keyboard name") func readKeyboardName() async throws {
    let packageRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(packageRepo.readKeyboardPackage(kmpUrl))
    let keyboard = try #require(source.keyboards?.first)
    #expect(keyboard.name == "አማርኛ (Amharic)")
  }
}
