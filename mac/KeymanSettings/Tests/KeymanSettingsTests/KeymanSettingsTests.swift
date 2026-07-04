/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-03-06
 *
 * Tests for KeymanSettings package
 *
 */

import Testing
import Foundation
@testable import KeymanSettings


@Suite("Settings Container") struct SettingsContainersTests {
  
  fileprivate init() async throws {
    print("init")
  }
  
  @Test("Check settings creation") func testSettingsCreation() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, hittiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    
    #expect(settingsContainer.installedPackages.isEmpty)
    
    settingsContainer.loadPackages()
    
    #expect(settingsContainer.findPackage(packageId: packageRepo.testPackageId) != nil)
    #expect(defaultsRepo.readEnabledKeyboards().contains(moabiteKeyboardKey))
  }
  
  @Test("Check finding packages by UUID") func testFindPackage() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, hittiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    settingsContainer.loadPackages()
    
    // verify that we can find that test package and cannot find the null package
    #expect(settingsContainer.findPackage(packageId: packageRepo.testPackageId) != nil)
    #expect(settingsContainer.findPackage(packageId: packageRepo.nullPackageId) == nil)
  }
  
  @Test("Check remove package") func testRemovePackage() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, hittiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    settingsContainer.loadPackages()
    
    // verify that we cannot find the test package after removing it
    #expect(settingsContainer.findPackage(packageId: packageRepo.testPackageId) != nil)
    settingsContainer.removePackage(at: 0)
    #expect(settingsContainer.findPackage(packageId: packageRepo.testPackageId) == nil)
  }
  
  /**
   * test whether an uninstalled keyboard is removed from the list of enabled keyboards when calling `validateSettings()`
   */
  @Test("Check settings validation ") func testSettingsValidation() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, uninstalledKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    
    settingsContainer.loadPackages()
    settingsContainer.validateUserDefaults()
    let enabledKeyboards = defaultsRepo.readEnabledKeyboards()
    
    #expect(!enabledKeyboards.contains(uninstalledKeyboardKey))
    #expect(enabledKeyboards.contains(moabiteKeyboardKey))
  }
  
  /**
   * test whether keyboard state changes from disabled to enabled when settings are applied
   */
  @Test("Check settings applied ") func testSettingsApplied() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, hittiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    
    settingsContainer.loadPackages()
    let package = try #require(settingsContainer.findPackage(packageId: packageRepo.testPackageId))
    
    // the hittite keyboard is initialized as disabled
    #expect(!package.isKeyboardEnabled(keyboardKey: hittiteKeyboardKey))
    
    // it should be enabled after user defaults are applied
    settingsContainer.applyUserDefaultsToInstalledPackages()
    
    #expect(package.isKeyboardEnabled(keyboardKey: hittiteKeyboardKey))
  }
  
  @Test("Check get installed keyboards") func testGetInstalledKeyboards() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey, hittiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    settingsContainer.loadPackages()
    
    // verify that two keyboards are installed
    #expect(settingsContainer.getInstalledKeyboardKeys().count == 2)
  }
  
  @Test("Check get enabled keyboards") func testGetEnabledKeyboards() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    settingsContainer.loadPackages()
    
    // verify that one keyboard is enabled
    #expect(settingsContainer.getEnabledKeyboardKeys().count == 1)
  }
  
  @Test("Check keyboard enabled") func testIsKeyboardEnabled() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    settingsContainer.loadPackages()
    
    // verify that moabite keyboard is enabled
    #expect(settingsContainer.isKeyboardEnabled(packageId: packageRepo.testPackageId, keyboardKey: moabiteKeyboardKey))
  }
  
  @Test("Check enable keyboard") func testEnableKeyboard() async throws {
    let defaultsRepo = DefaultsRepoStub(enabledKeyboards: Set([moabiteKeyboardKey]))
    let packageRepo = PackageRepoStub()
    let settingsContainer = SettingsContainer(defaultsRepo: defaultsRepo, packageRepo: packageRepo)
    settingsContainer.loadPackages()
    
    // verify that the hittite keyboard changes from disabled to enabled
    #expect(!settingsContainer.isKeyboardEnabled(packageId: packageRepo.testPackageId, keyboardKey: hittiteKeyboardKey))
    settingsContainer.setKeyboardEnabled(packageId: packageRepo.testPackageId, keyboardKey: hittiteKeyboardKey, enabled: true)
    #expect(settingsContainer.isKeyboardEnabled(packageId: packageRepo.testPackageId, keyboardKey: hittiteKeyboardKey))
  }
}



@Suite("Check Keyman paths") struct KeymanPathsTests {
  
  fileprivate init() async throws {
    print("init")
  }
  
  @Test("Check Keyman 17 documents directory") func testKeyman17DocumentsDirectory() async throws {
    let documentsDirectory = try #require(KeymanPaths().keyman17DocumentsDirectory)
    #expect(documentsDirectory.absoluteString.hasSuffix("Documents/"))
  }
  
  @Test("Check Keyman 17 packages directory") func testKeyman17KeyboardsDirectory() async throws {
    #expect(true)
    let keyboardsDirectory = try #require(KeymanPaths().keyman17PackagesDirectory)
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
  
  @Test("Check Keyman 18 packages directory") func testKeyman18SupportKeyboardsDirectory() async throws {
    #expect(true)
    let keyboardsDirectory = try #require(KeymanPaths().keyman18PackagesDirectory)
    #expect(keyboardsDirectory.absoluteString.hasSuffix("Library/Application%20Support/keyman.inputmethod.Keyman/Keyman-Keyboards/"))
  }
  
  @Test("Check Keyman 19 container data directory") func testKeyman19ContainerDataDirectory() async throws {
    #expect(true)
    let containerDirectory = try #require(KeymanPaths().keyman19ContainerDirectory)
    #expect(containerDirectory.absoluteString.hasSuffix("Group%20Containers/group.com.keyman/"))
  }
  
  @Test("Check Keyman 19 packages directory") func testKeyman19KeyboardsDirectory() async throws {
    #expect(true)
    let keyboardsDirectory = try #require(KeymanPaths().keyman19PackagesDirectory)
    #expect(keyboardsDirectory.absoluteString.hasSuffix("Group%20Containers/group.com.keyman/Library/Application%20Support/Keyman-Packages/"))
  }
}

// defaults read test.suite.name

@Suite("UserDefaults check") struct KeymanSettingsRepositoryTests {
  var settingsRepo: DefaultsRepo
  let hittiteKeyboardKey = "/sil_extinct/hittite.kmx"
  let moabiteKeyboardKey = "/sil_extinct/moabite.kmx"
  
  fileprivate init() async throws {
    print("init Settings")
    do {
      try self.settingsRepo = DefaultsRepository(suiteName: "test.suite.name")
    } catch UserDefaultsError.unknownSuite {
      fatalError("Cannot access UserDefaults suite.")
    } catch {
      fatalError("Cannot access UserDefaults suite.")
    }
  }
  
  @Test("Write and read selected keyboard") func writeSelectedKeyboard() async throws {
    settingsRepo.writeSelectedKeyboard(keyboardName: moabiteKeyboardKey)
    #expect(settingsRepo.readSelectedKeyboard() == moabiteKeyboardKey)
  }
  
  @Test("Write and read enabled keyboards") func writeEnabledKeyboards() async throws {
    settingsRepo.writeEnabledKeyboards(enabledKeyboardsArray: [hittiteKeyboardKey, moabiteKeyboardKey])
    #expect(settingsRepo.readEnabledKeyboards().count == 2)
    #expect(settingsRepo.readEnabledKeyboards().contains(hittiteKeyboardKey))
    #expect(settingsRepo.readEnabledKeyboards().contains(moabiteKeyboardKey))
  }
}

@Suite("Read Keyman package") struct KeymanPackageRepositoryTests {
  let packageRepo = PackageRepository()
  let kmpUrl: URL
  let source: PackageSource?
  let fakePackageUrl: URL = URL(fileURLWithPath: "/fake/test/destination/amharic-fake")

  fileprivate init() async throws {
    self.kmpUrl = try #require(Bundle.module.url(forResource: "amharic.kmp", withExtension: "json"))
    source =  packageRepo.readPackage(packageDirectoryUrl: fakePackageUrl, kmpFileUrl: self.kmpUrl)
  }
  
  @Test("Read package name") func readPackageName() async throws {
    let packageSource = try #require(source)
    #expect(packageSource.packageName == "GFF Amharic Keyboard")
  }
  
  @Test("Read package version") func readPackageVersion() async throws {
    let packageSource = try #require(source)
    #expect(packageSource.packageVersion == "3.1.2")
  }
  
  @Test("Read copyright") func readCopyright() async throws {
    let packageSource = try #require(source)
    #expect(packageSource.copyright == "© Geʾez Frontier Foundation, SIL International")
  }
  
  @Test("Read keyboard count") func readKeyboardCount() async throws {
    let packageSource = try #require(source)
    #expect(packageSource.keyboards?.count == 1)
  }
  
  @Test("Read keyboard name") func readKeyboardName() async throws {
    let packageSource = try #require(source)
    let keyboard = try #require(packageSource.keyboards?.first)
    #expect(keyboard.name == "አማርኛ (Amharic)")
  }
  
  /**
   * Ensure that Keyboard objects are correctly created from Keyboardsource
   */
  @Suite("Check keyboard state") struct KeyboardStateTests {
    let packageRepo = PackageRepository()
    let kmpUrl: URL
    let packageSource: PackageSource?
    let fakePackageUrl: URL = URL(fileURLWithPath: "/fake/test/destination/amharic-fake")
    
    fileprivate init() async throws {
      self.kmpUrl = try #require(Bundle.module.url(forResource: "amharic.kmp", withExtension: "json"))
      self.packageSource =  packageRepo.readPackage(packageDirectoryUrl: fakePackageUrl, kmpFileUrl: self.kmpUrl)
    }

    @Test("Check keyboard is disabled") func checkKeyboardDisabled() async throws {
      let package = try #require(self.packageSource)
      let directoryUrl = try #require(package.directoryUrl)
      let keyboards = try #require(package.keyboards)
      let keyboardSource = try #require(keyboards.first)
      let keyboard = Keyboard(keyboardSource: keyboardSource, directoryUrl: directoryUrl)
      
      #expect(keyboard.enabled)
    }

    @Test("Check keyboard key") func checkKeyboardKey() async throws {
      let package = try #require(self.packageSource)
      let directoryUrl = try #require(package.directoryUrl)
      let keyboards = try #require(package.keyboards)
      let keyboardSource = try #require(keyboards.first)
      let keyboard = Keyboard(keyboardSource: keyboardSource, directoryUrl: directoryUrl)
      
      #expect(keyboard.keyboardKey == "/amharic-fake/gff_amharic.kmx")
    }
 }
}
