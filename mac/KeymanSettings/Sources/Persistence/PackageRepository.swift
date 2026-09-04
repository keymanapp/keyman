/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * PackageRepository is responsible for reading, writing and removing
 * Keyman data on disk.
 *
 */

import Foundation
import OSLog

public enum LoadPackageError: LocalizedError {
  case invalidUrl
  case unzipError
  case containsNoFiles
  case containsNoKeyboards
  case kmpJsonFileUnreadable
  case kmpJsonFileNotFound
  case missingKeyboardName
  case missingKeyboardId
  case missingKeyboardVersion
  case missingKmxFile
  case insufficientKeymanVersion(packageName: String, requiredKeymanVersion: String, actualKeymanVersion: String)
  
  private var packageBundle: LocalizedStringResource.BundleDescription {
    .atURL(Bundle.module.bundleURL)
  }
  public var errorDescription: String? {
    switch self {
    case .invalidUrl:
      let resource = LocalizedStringResource(
        "invalid.url",
        defaultValue: "The URL is not valid.",
        bundle: packageBundle)
      return String(localized: resource)
    case .unzipError:
      let resource = LocalizedStringResource(
        "could.not.unzip",
        defaultValue: "The keyboard package could not be unzipped.",
        bundle: packageBundle)
      return String(localized: resource)
    case .containsNoFiles:
      let resource = LocalizedStringResource(
        "contains.no.files",
        defaultValue: "The keyboard package contains no files.",
        bundle: packageBundle)
      return String(localized: resource)
    case .containsNoKeyboards:
      let resource = LocalizedStringResource(
        "contains.no.keyboards",
        defaultValue: "The keyboard package contains no keyboards.",
        bundle: packageBundle)
      return String(localized: resource)
    case .kmpJsonFileUnreadable:
      let resource = LocalizedStringResource(
        "kmp.json.unreadable",
        defaultValue: "The package's kmp.json file could not be parsed.",
        bundle: packageBundle)
      return String(localized: resource)
    case .kmpJsonFileNotFound:
      let resource = LocalizedStringResource(
        "kmp.json.not.found",
        defaultValue: "The package's kmp.json file was not found.",
        bundle: packageBundle)
      return String(localized: resource)
    case .missingKeyboardName:
      let resource = LocalizedStringResource(
        "keyboard.name.missing",
        defaultValue: "A keyboard in the package has no name.",
        bundle: packageBundle)
      return String(localized: resource)
    case .missingKeyboardId:
      let resource = LocalizedStringResource(
        "keyboard.id.missing",
        defaultValue: "A keyboard in the package has no ID.",
        bundle: packageBundle)
      return String(localized: resource)
    case .missingKeyboardVersion:
      let resource = LocalizedStringResource(
        "keyboard.missing.version",
        defaultValue: "A keyboard in the package has no version.",
        bundle: packageBundle)
      return String(localized: resource)
    case .missingKmxFile:
      let resource = LocalizedStringResource(
        "keyboard.missing.kmx.file",
        defaultValue: "A keyboard in the package has no corresponding .KMX file.",
        bundle: packageBundle)
      return String(localized: resource)
    case .insufficientKeymanVersion(let packageName, let requiredKeymanVersion, let actualKeymanVersion):
      let resource = LocalizedStringResource(
        "insufficient.keyman.version",
        defaultValue: "The keyboard package '\(packageName)' requires Keyman version \(requiredKeymanVersion) but your version is \(actualKeymanVersion).",
        bundle: packageBundle)
      return String(localized: resource)
    }
  }
}

public class PackageRepository: PackageRepo {
  fileprivate let packageJsonFilename = "kmp.json"
  fileprivate let packageInfFilename = "kmp.inf"
  fileprivate let pathUtil: KeymanPaths
  
  public init() throws {
    self.pathUtil = try KeymanPaths()
    
    try self.createKeyman19SharedDataDirectoriesIfNeeded()
  }
  
  /**
   * Load the Keyman packages from disk and wrap each package as a `KeymanPackage` object
   * If the `KeymanPackage` passes validation, then add it to the `installedPackages` array.
   *
   */
  public func loadAllPackages() -> [KeymanPackage] {
    var installedPackages: [KeymanPackage] = []
    let packageSourceMap = self.readKeymanPackagesForKeyman19()
    
    for (url, source) in packageSourceMap {
      let package = KeymanPackage(packageUrl: url, packageSource: source)
      do {
        try package.validate()
        installedPackages.append(package)
      } catch {
        Logger.data.error("validation failed for \(url.lastPathComponent, privacy: .public) with error: \(error as NSError, privacy: .public)")
        LogUtil.errorBreadcrumb("validation failed for \(url.lastPathComponent) with error: \(error as NSError)", category: .data)
      }
    }
    
    return installedPackages
  }
  
  /**
   * Load the single package from disk and wrap it as a `KeymanPackage` object
   * If the `KeymanPackage` passes validation, then add it to the `installedPackages` array.
   *
   */
  public func loadSinglePackage(packageUrl: URL) throws -> KeymanPackage {
    Logger.data.info("loadSinglePackage from url: \(packageUrl.cleanUrlPath(), privacy: .public)")
    LogUtil.infoBreadcrumb("loadSinglePackage from url: \(packageUrl.cleanUrlPath())", category: .data)
    
    guard let source =  try readPackageFromDirectory(packageDirectoryUrl: packageUrl) else { throw LoadPackageError.invalidUrl }
    
    let package = KeymanPackage(packageUrl: packageUrl, packageSource: source)
    try package.validate()
    return package
  }
  
  /**
   * delete the package from disk
   */
  public func deletePackage(package: KeymanPackage) {
    Logger.data.info("deleting package: \(package.sourceDirectoryUrl.cleanUrlPath(), privacy: .public)")
    LogUtil.infoBreadcrumb("deleting package: \(package.sourceDirectoryUrl.cleanUrlPath())", category: .data)
    do {
      try FileManager.default.removeItem(at: package.sourceDirectoryUrl)
      Logger.data.info("deleted package: \(package.sourceDirectoryUrl.cleanUrlPath(), privacy: .public)")
      LogUtil.infoBreadcrumb("deleted package: \(package.sourceDirectoryUrl.cleanUrlPath())", category: .data)
    } catch {
      Logger.data.error("could not delete directory: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("could not delete directory: \(error as NSError)", category: .data)
    }
  }
  
  /**
   * Creates the directory tree where packages are stored under the standard 'Group Containers' directory
   * Also creates the temp directory used for keyboard installation
   */
  public func createKeyman19SharedDataDirectoriesIfNeeded() throws {
    let packageDirectory = pathUtil.keyman19PackagesDirectory
    let packageTempDirectory = pathUtil.keyman19TempDirectory
    
    // create the keyman-packages directory if it doesn't already exist
    if !FileManager.default.fileExists(atPath: packageDirectory.path(percentEncoded: false)) {
      try FileManager.default.createDirectory(at: packageDirectory, withIntermediateDirectories: true, attributes: nil)
      Logger.data.info("Created directory: \(packageDirectory.cleanUrlPath(), privacy: .public)")
      LogUtil.infoBreadcrumb("Created directory: \(packageDirectory.cleanUrlPath())", category: .data)
    } else {
      Logger.data.info("Directory already exists: \(packageDirectory.cleanUrlPath(), privacy: .public)")
      LogUtil.infoBreadcrumb("Directory already exists: \(packageDirectory.cleanUrlPath())", category: .data)
    }
    
    // create the temp directory if it doesn't already exist
    if !FileManager.default.fileExists(atPath: packageTempDirectory.path(percentEncoded: false)) {
      try FileManager.default.createDirectory(at: packageTempDirectory, withIntermediateDirectories: true, attributes: nil)
      Logger.data.info("Created directory: \(packageTempDirectory.cleanUrlPath(), privacy: .public)")
      LogUtil.infoBreadcrumb("Created directory: \(packageTempDirectory.cleanUrlPath())", category: .data)
    } else {
      Logger.data.info("Directory already exists: \(packageTempDirectory.cleanUrlPath(), privacy: .public)")
      LogUtil.infoBreadcrumb("Directory already exists: \(packageTempDirectory.cleanUrlPath())", category: .data)
    }
  }
  
  /**
   * Delete all the files in the temp directory
   */
  public func cleanupTempDirectory() {
    let fileManager = FileManager.default
    
    do {
      let fileURLs = try fileManager.contentsOfDirectory(
        at: self.pathUtil.keyman19TempDirectory,
        includingPropertiesForKeys: nil,
        options: .skipsHiddenFiles
      )
      
      for fileURL in fileURLs {
        try fileManager.removeItem(at: fileURL)
      }
      
      Logger.data.info("successfully cleared temp directory")
      LogUtil.infoBreadcrumb("successfully cleared temp directory", category: .data)
    } catch {
      Logger.data.error("error clearing temp directory: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("error clearing temp directory: \(error as NSError)", category: .data)
    }
  }
  
  /**
   * get the url to where the specified kmp file should be downloaded
   */
  public func getDownloadUrl(for kmpFilename: String) -> URL {
    return self.pathUtil.keyman19TempDirectory.appendingPathComponent(kmpFilename)
  }
  
  /**
   * get the url to where the specified package should initially be unzipped
   */
  public func getUnzipDestinationUrl(for packageName: String) -> URL {
    return self.pathUtil.keyman19TempDirectory.appendingPathComponent(packageName)
  }
  
  /**
   * build the URL where the specified package will be installed
   */
  public func buildInstallationUrlForPackageName(directoryName: String) -> URL {
    return self.pathUtil.keyman19PackagesDirectory.appendingPathComponent(directoryName)
  }
  
  /**
   * install keyboard at specified URL
   */
  public func unzipKmpFile(at kmpFileUrl: URL, to packageDestinationUrl: URL) throws {
    do {
      try FileManager.default.unzipItem(at: kmpFileUrl, to: packageDestinationUrl)
      Logger.data.info("successfully unzipped the file")
      LogUtil.infoBreadcrumb("successfully unzipped the file", category: .data)
    } catch {
      Logger.data.error("extraction failed: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("extraction failed: \(error as NSError)", category: .data)
      throw LoadPackageError.unzipError
    }
  }
  
  /**
   * Check to see whether the shared Keyman data directory exists under 'Library/Group Containers/'
   */
  public func keyman19SharedDataDirectoryExists() -> Bool {
    return self.directoryExistsAtPath(directoryUrl: self.pathUtil.keyman19PackagesDirectory)
  }
  
  /**
   * returns true if a directory exists at the specified URL
   */
  func directoryExistsAtPath(directoryUrl: URL) -> Bool {
    var isDirectory: ObjCBool = false
    let exists = FileManager.default.fileExists(atPath: directoryUrl.path(percentEncoded: false), isDirectory: &isDirectory)
    return exists && isDirectory.boolValue
  }
  
  /**
   * read packages at Keyman 19 location, inside Group Containers directory
   */
  func readKeymanPackagesForKeyman19() -> [URL: PackageSource] {
    return readPackageSource(packageDirectoryUrl: self.pathUtil.keyman19PackagesDirectory)
  }
  
  /**
   * loop through all the sub-directories in the packages directory and try to read them as packages
   */
  func readPackageSource(packageDirectoryUrl: URL) -> [URL: PackageSource] {
    var packageMap: [URL: PackageSource] = [:]
    
    do {
      // Get the URLs for all items in the directory that are not hidden
      let directoryContents = try FileManager.default.contentsOfDirectory(
        at: packageDirectoryUrl,
        includingPropertiesForKeys: nil,
        options: [.skipsHiddenFiles]
      )
      
      for itemUrl in directoryContents {
        // if the item is a directory, then attempt to read it as a keyboard package
        if (itemUrl.hasDirectoryPath) {
          do {
            if let packageSource =  try readPackageFromDirectory(packageDirectoryUrl: itemUrl) {
              packageMap[itemUrl] = packageSource
            }
          } catch let error as LoadPackageError {
            Logger.data.error("package at \(itemUrl.cleanUrlPath(), privacy: .public) could not be loaded: \(error as NSError, privacy: .public)")
            LogUtil.errorBreadcrumb("package at \(itemUrl.cleanUrlPath()) could not be loaded: \(error as NSError)", category: .data)
          }
        }
      }
    } catch {
      Logger.data.error("failed to read directory: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("failed to read directory: \(error as NSError)", category: .data)
    }
    
    Logger.data.info("readPackageSource: \(packageMap.count) packages read")
    LogUtil.infoBreadcrumb("readPackageSource: \(packageMap.count) packages read", category: .data)
    return packageMap
  }
  
  /**
   * check the specified directory for the kmp.json file and read it if it exists
   */
  func readPackageFromDirectory(packageDirectoryUrl: URL) throws -> PackageSource? {
    Logger.data.info("readPackageFromDirectory from url: \(packageDirectoryUrl.cleanUrlPath(), privacy: .public)")
    LogUtil.infoBreadcrumb("readPackageFromDirectory from url: \(packageDirectoryUrl.cleanUrlPath())", category: .data)
    var packageSource: PackageSource? = nil
    let kmpJsonFileUrl = packageDirectoryUrl.appendingPathComponent(packageJsonFilename)
    let kmpInfFileUrl = packageDirectoryUrl.appendingPathComponent(packageInfFilename)
    
    if FileManager.default.fileExists(atPath: kmpJsonFileUrl.path(percentEncoded: false)) {
      // if an error occurs, it will be propagated to caller
      if let source = try readPackageFromJson(kmpJsonFileUrl: kmpJsonFileUrl) {
        packageSource = source
      }
    } else {
      // if no kmp.json file, look for kmp.info instead
      if FileManager.default.fileExists(atPath: kmpInfFileUrl.path(percentEncoded: false)) {
        Logger.data.info("fallback to kmp.inf file at: \(kmpInfFileUrl.cleanUrlPath(), privacy: .public)")
        LogUtil.infoBreadcrumb("fallback to kmp.inf file at: \(kmpInfFileUrl.cleanUrlPath())", category: .data)
        
        if let source = try readPackageFromInf(kmpInfFileUrl: kmpInfFileUrl) {
          packageSource = source
        }
      } else {
        // no kmp.json and no kmp.inf, so throw error
        // the error designates only kmp.json missing as kmp.inf is a fallback
        throw LoadPackageError.kmpJsonFileNotFound
      }
    }
    
    return packageSource
  }
  
  /**
   * read and parse the kmp.json file at the specified URL
   */
  func readPackageFromJson(kmpJsonFileUrl: URL) throws -> PackageSource? {
    var packageSource: PackageSource?
    do {
      let jsonData = try Data(contentsOf: kmpJsonFileUrl, options: .mappedIfSafe)
      packageSource = try JSONDecoder().decode(PackageSource.self, from: jsonData)
      
    } catch let error as LoadPackageError {
      // if we encounter a LoadPackageError, propagate it
      throw error
    } catch {
      // otherwise convert the error to a LoadPackageError error
      Logger.data.error("readPackage error: \(error as NSError, privacy: .public)")
      LogUtil.errorBreadcrumb("readPackage error: \(error as NSError)", category: .data)
      throw LoadPackageError.kmpJsonFileUnreadable
    }
    return packageSource
  }
  
  /**
   * read and parse the kmp.json file at the specified URL
   */
  func readPackageFromInf(kmpInfFileUrl: URL) throws -> PackageSource? {
    var packageSource: PackageSource? = nil
    
    do {
      let packageReader = KMPackageReader(url: kmpInfFileUrl)
      if let packageInfo = try packageReader.loadPackageInfoFromInfFile() {
        print("successfully read package: \(String(describing: packageInfo.packageName))")
      }
    } catch {
      print("failed to read kmp.inf file, error: \(error)")
      return nil
    }
    
    return packageSource
  }
}
