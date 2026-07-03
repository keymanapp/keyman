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
  func createKeyman19SharedDataDirectoriesIfNeeded() throws
  func loadAllPackages() -> [KeymanPackage]
  func deletePackage(package: KeymanPackage)
  func unzipKmpFile(at kmpFileUrl: URL, to packageDestinationUrl: URL) throws
  func loadSinglePackage(packageUrl: URL) throws -> KeymanPackage
  func getDownloadUrl(for kmpFilename: String) -> URL
  func getUnzipDestinationUrl(for packageName: String) -> URL
  func getInstallationUrlForPackageName(packageName: String) -> URL
}
