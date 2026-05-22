/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-19
 *
 * Value object for loading a keyboard from kmp.json
 *
 */


public struct KeyboardSource: Identifiable, Decodable, Hashable, Equatable {
  let name: String
  public let id: String
  let version: String?
  let oskFont: String?
  let displayFont: String?
  let languages: [LanguageSource]?
  
  enum CodingKeys: String, CodingKey {
    case name
    case id
    case version
    case oskFont
    case displayFont
    case languages
  }
  
  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    
    let name = try container.decodeIfPresent(String.self, forKey: .name)
    let id = try container.decodeIfPresent(String.self, forKey: .id)
    let version = try container.decodeIfPresent(String.self, forKey: .version)

    // Invalidation: name, id and version required and non-empty
    guard let validName = name, !validName.isEmpty else {
      throw DecodingError.dataCorruptedError(
        forKey: .name,
        in: container,
        debugDescription: "Keyboard.name is required and cannot be empty."
      )
    }
    
    guard let validId = id, !validId.isEmpty else {
      throw DecodingError.dataCorruptedError(
        forKey: .id,
        in: container,
        debugDescription: "Keyboard.id is required and cannot be empty."
      )
    }
 
    guard let validVersion = version, !validVersion.isEmpty else {
      throw DecodingError.dataCorruptedError(
        forKey: .version,
        in: container,
        debugDescription: "Keyboard.version is required and cannot be empty."
      )
    }

    self.name = validName
    self.id = validId
    self.version = validVersion
    self.oskFont = try container.decodeIfPresent(String.self, forKey: .oskFont)
    self.displayFont = try container.decodeIfPresent(String.self, forKey: .displayFont)
    self.languages = try container.decodeIfPresent([LanguageSource].self, forKey: .languages)
  }
}
