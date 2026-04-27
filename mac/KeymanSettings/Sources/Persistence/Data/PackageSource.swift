/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-19
 *
 * Value object that describes a package as loaded from a .KMP file
 *
 */


import Foundation

public struct PackageSource: Identifiable, Decodable, Hashable, Equatable {
  public var id = UUID()
  var directoryUrl: URL?
  var jsonFileUrl: URL?
  let system: SystemInfo?
  let options: Options?
  let info: Info
  let files: [PackageFile]?
  let keyboards: [KeyboardSource]?

  // computed properties for convenience
  var packageName: String {
    return info.name.description
  }
  var packageVersion: String {
    return info.version.description
  }
  var copyright: String? {
    if let copy = info.copyright?.description {
      return copy
    } else {
      return nil
    }
  }
  var readmeFilename: String? {
    if let filename = options?.readmeFile {
      return filename
    } else {
      return nil
    }
  }
  var graphicFilename: String? {
    if let filename = options?.graphicFile {
      return filename
    } else {
      return nil
    }
  }

  enum CodingKeys: String, CodingKey {
    case system
    case options
    case info
    case files
    case keyboards
  }
  
  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    directoryUrl = nil
    jsonFileUrl = nil

    self.info = try container.decode(Info.self, forKey: .info)
    self.keyboards = try container.decodeIfPresent([KeyboardSource].self, forKey: .keyboards)
    self.system = try container.decodeIfPresent(SystemInfo.self, forKey: .system)
    self.options = try container.decodeIfPresent(Options.self, forKey: .options)
    self.files = try container.decodeIfPresent([PackageFile].self, forKey: .files)
    
    // Invalidation: must contain files or keyboards
    if files == nil && keyboards == nil {
      throw DecodingError.dataCorrupted(
        .init(
          codingPath: decoder.codingPath,
          debugDescription: "KeymanPackage must contain either 'files' or 'keyboards'."
        )
      )
    }
  }
  
  public func hash(into hasher: inout Hasher) {
      hasher.combine(id) // only combine the unique ID
  }
  
  // Custom Equatable conformance (required by Hashable)
  public static func == (lhs: PackageSource, rhs: PackageSource) -> Bool {
      return lhs.id == rhs.id // only compare unique IDs
  }
}

struct Info: Decodable {
  let name: DescribedValue
  let version: DescribedValue
  let copyright: DescribedValue?
  let author: Author?
  let website: Website?
  
  enum CodingKeys: String, CodingKey {
    case name
    case copyright
    case author
    case version
    case website
  }
}

struct DescribedValue: Decodable {
  let description: String
  
  enum CodingKeys: String, CodingKey {
    case description
  }
}

struct Author: Decodable {
  let description: String?
  let url: String?
  
  enum CodingKeys: String, CodingKey {
    case description
    case url
  }
}

struct Website: Decodable {
  let description: String?
  let url: String?
  
  enum CodingKeys: String, CodingKey {
    case description
    case url
  }
}

struct SystemInfo: Decodable {
  let keymanDeveloperVersion: String?
  let fileVersion: String?
  
  enum CodingKeys: String, CodingKey {
    case keymanDeveloperVersion
    case fileVersion
  }
}

struct Options: Decodable {
  let readmeFile: String?
  let graphicFile: String?
  
  enum CodingKeys: String, CodingKey {
    case readmeFile
    case graphicFile
  }
}

struct PackageFile: Decodable {
  let name: String?
  let description: String?
  
  enum CodingKeys: String, CodingKey {
    case name
    case description
  }
}




