/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-04-29
 *
 * Contains stubs for the repository classes to support unit tests
 * so that they do not access the file system or UserDefaults database.
 */

import Foundation
import KeymanSettings

/**
 * conforms to DefaultsRepo but takes a set of enabled keyboard keys in the initializer and
 * keeps everything in memory rather than accessing UserDefaults.
 */

public let extinctPackageName = "sil_extinct"
public let moabiteKeyboardKey = "/\(extinctPackageName)/moabite_basic.kmx"
public let hittiteKeyboardKey = "/\(extinctPackageName)/hittite_basic.kmx"
public let uninstalledKeyboardKey = "/\(extinctPackageName)/girgashite_basic.kmx"

class DefaultsRepoStub: DefaultsRepo {
  private var selectedKeyboard: String
  private var enabledKeyboards: Set<String>
  
  init(enabledKeyboards: Set<String>) {
    self.enabledKeyboards = enabledKeyboards
    
    // selectedKeyboard not relevant yet in config app
    // but may need validation later
    selectedKeyboard = moabiteKeyboardKey
  }
  
  func readEnabledKeyboards() -> Set<String> {
    return enabledKeyboards
  }
  
  func writeEnabledKeyboards(enabledKeyboardsArray: [String]) {
    self.enabledKeyboards = Set(enabledKeyboardsArray)
  }
  
  func readSelectedKeyboard() -> String {
    return self.selectedKeyboard
  }
  
  func writeSelectedKeyboard(keyboardName: String) {
    self.selectedKeyboard = keyboardName
  }
  
  
  func logDefaults() {
    print("UserDefaults:")
    print("\("KMSelectedKeyboardsKey"): \(self.readSelectedKeyboard())")
    print("\("KMEnabledKeyboardsKey"): \(self.readEnabledKeyboards())")
  }
  
  func clearDefaults() {
    selectedKeyboard = ""
    enabledKeyboards = []
  }
  
  func installationStateExists() -> Bool {
    return false
  }
  
  func readInstallationState() -> Dictionary<String, Any>? {
    return nil
  }
  
  func writeInstallationState(_ dictionary: Dictionary<String, Any>) {
  }
  
  func deleteInstallationState() {
  }
}

/**
 * conforms to PackageRepo but creates a package with two keyboards and
 * stores them in memory with no access to disk
 */
class PackageRepoStub: PackageRepo {
  func unzipKmpFile(at kmpFileUrl: URL, to packageDestinationUrl: URL) throws {
  }
  
  func getDownloadUrl(for kmpFilename: String) -> URL {
    return URL(fileURLWithPath: "")
  }
  
  func getUnzipDestinationUrl(for packageName: String) -> URL {
    return URL(fileURLWithPath: "")
  }
  
  func getInstallationUrlForPackageName(packageName: String) -> URL {
    return URL(fileURLWithPath: "")
  }
  
  public let nullPackageId = UUID(uuid: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  public var testPackageId = UUID(uuid: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  
  func keyman19SharedDataDirectoryExists() -> Bool {
    true
  }
  
  func createKeyman19SharedDataDirectoriesIfNeeded() throws {
    // do nothing
  }
  
  /**
   * creates a package containing two keyboards, one enabled and one disabled
   */
  func loadAllPackages() -> [KeymanPackage] {
    let packagesDirectoryUrl = URL(filePath: "/Users/linguist-sil/Library/Group%20Containers/group.com.keyman/Library/Application%20Support/Keyman-Packages/")!
    let testPackageDirectoryName = extinctPackageName
    let testPackageUrl = packagesDirectoryUrl.appendingPathComponent(testPackageDirectoryName)
    let moabiteKeyboardId = "moabite_basic"
    let moabiteKeyboard = Keyboard(name: "moabite basic", keyboardId: moabiteKeyboardId, keyboardDirectoryUrl: testPackageUrl, enabled: true)
    let hittiteKeyboardId = "hittite_basic"
    let hittiteKeyboard = Keyboard(name: "hittite basic", keyboardId: hittiteKeyboardId, keyboardDirectoryUrl: testPackageUrl, enabled: false)
    
    let testPackage = KeymanPackage(sourceDirectoryUrl: testPackageUrl,
                                    keyboards: [moabiteKeyboard, hittiteKeyboard], packageName: "Extinct Languages", packageVersion: "1.1.03", jsonFileUrl: testPackageUrl.appendingPathComponent("kmp.json"))
    testPackageId = testPackage.id
    
    return [testPackage]
  }
  
  func deletePackage(package: KeymanPackage) {
  }
  
  func getDownloadUrlForPackageName(packageName: String) -> URL? {
    return nil
  }
  
  func loadSinglePackage(packageUrl: URL) throws -> KeymanSettings.KeymanPackage {
    fatalError("loadSinglePackage is not implemented")
  }
}
