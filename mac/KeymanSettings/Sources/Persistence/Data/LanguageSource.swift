/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2025-12-19
 *
 * Value object that describes a language as loaded from a .KMP file
 *
 */


public struct LanguageSource: Identifiable, Decodable, Hashable, Equatable {
  let name: String?
  public let id: String
  
  enum CodingKeys: String, CodingKey {
    case name
    case id
  }
  
  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    
    self.name = try container.decodeIfPresent(String.self, forKey: .name)
    
    let id = try container.decodeIfPresent(String.self, forKey: .id)
    
    guard let validId = id, !validId.isEmpty else {
      throw DecodingError.dataCorruptedError(
        forKey: .id,
        in: container,
        debugDescription: "Language.id is required and cannot be empty."
      )
    }
    
    self.id = validId
  }
}
