/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-07-21
 *
 * The @State variable currentPage is of type InstallPage.
 * These pages are used to easily connect each installation phase with a SwiftUI view in ParentInstallView.
 */

enum InstallPage: String, CaseIterable {
  case loading
  case initialInstall
  case initialRepair
  case completed
  case enableInputMethod
  case allowSecurityPermission
  case rerunInstaller
}
