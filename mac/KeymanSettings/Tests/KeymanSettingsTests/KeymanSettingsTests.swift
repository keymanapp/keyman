import Testing
import Foundation
@testable import KeymanSettings

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
    #expect(keyboardsDirectory.absoluteString.hasSuffix("Group%20Containers/group.com.keyman/Library/Application%20Support/Keyman-Keyboards/"))
  }
}


@Suite("Load KMP data") struct KeymanDataRepositoryTests {
  var testKmpUrl: URL? = nil
  
  fileprivate init() async throws {
    print("init")
  }
  
  func getAmharicKmpUrl() -> URL? {
    return Bundle.module.url(forResource: "amharic.kmp", withExtension: "json")
  }
  
  @Test("Find amharic .kmp file") func locateKmpTestData() async throws {
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    #expect(kmpUrl.isFileURL)
  }
  
  @Test("Read package") func loadPackageSource() async throws {
    let dataRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let _ =  try #require(dataRepo.readKeyboardPackage(kmpUrl))
  }
  
  @Test("Read package name") func readPackageName() async throws {
    let dataRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(dataRepo.readKeyboardPackage(kmpUrl))
    #expect(source.packageName == "GFF Amharic Keyboard")
  }
  
  @Test("Read package version") func readPackageVersion() async throws {
    let dataRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(dataRepo.readKeyboardPackage(kmpUrl))
    #expect(source.packageVersion == "3.1.2")
  }
  
  @Test("Read copyright") func readCopyright() async throws {
    let dataRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(dataRepo.readKeyboardPackage(kmpUrl))
    #expect(source.copyright == "© Geʾez Frontier Foundation, SIL International")
  }
  
  @Test("Read keyboard count") func readKeyboardCount() async throws {
    let dataRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(dataRepo.readKeyboardPackage(kmpUrl))
    #expect(source.keyboards?.count == 1)
  }
  
  @Test("Read keyboard name") func readKeyboardName() async throws {
    let dataRepo = PackageRepository()
    
    let kmpUrl = try #require(self.getAmharicKmpUrl())
    let source =  try #require(dataRepo.readKeyboardPackage(kmpUrl))
    let keyboard = try #require(source.keyboards?.first)
    #expect(keyboard.name == "አማርኛ (Amharic)")
  }
}
