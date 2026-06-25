/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-10
 *
 * PackageRepo is a protocol that exposes the ability to read, write and remove
 * Keyman packages.
 *
 */

import Foundation

public protocol PackageRepo {
  func keyman19SharedDataDirectoryExists() -> Bool
  func createKeyman19SharedDataDirectories()
  func loadAllPackages() -> [KeymanPackage]
  func deletePackage(package: KeymanPackage)
  func getDownloadUrlForPackageName(packageName: String) -> URL?
  func installPackage(packageUrl: URL) throws -> URL?
  func loadSinglePackage(packageUrl: URL) throws -> KeymanPackage
}
