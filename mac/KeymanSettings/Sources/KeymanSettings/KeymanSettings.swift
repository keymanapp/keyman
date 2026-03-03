import Combine

public class KeymanSettings : ObservableObject {

  @Published public var settingName: String

  public init() {
    settingName = "uninitialized"
  }
}
