/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Eli Schantz on 2026-06-29
 *
 * Main view used for configuring Keyman
 */

import SwiftUI

enum InstallPage: String, CaseIterable {
  case loading
  case initialInstall
  case initialRepair
  case completed
  case enableInputMethod
  case allowSecurityPermission
  case rerunInstaller
  case restartMac
}

struct MainInstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  /**
   * A namespace is created here and passed to child views.
   * Any subviews with the same string id and this namespace
   * will animate smoothly when changing positions or states.
   */
  @Namespace var animation
  @State public var currentPage: InstallPage = .loading
  
  /**
   * chooseCurrentPage() will update the @State var currentPage according to the current task.
   * If there is a task involved with the installationPhase, it will display the page associated with that task.
   * If there is not a task involved with the installationPhase, it will display the page associated with that phase.
   */
  
  func chooseCurrentPage() {
    if installation.installationPhase.hasTasks {
      switch installation.currentTask()?.taskType {
      case .prepareNewInstall: currentPage = .initialInstall
      case .prepareNewRepair: currentPage = .initialRepair
      case .enableInputMethod: currentPage = .enableInputMethod
      case .requestAccess: currentPage = .allowSecurityPermission
      case .confirmAccess: currentPage = .allowSecurityPermission
      case .requestRestart: currentPage = .restartMac
      case .confirmRestart: currentPage = .restartMac
      default: currentPage = .completed
      }
    } else {
      switch installation.installationPhase {
      case .evaluatingInstallation: currentPage = .loading
      case .inputMethodMissing, .inputMethodOutdated: currentPage = .rerunInstaller
      case .installationComplete:
        currentPage = .completed
        if !installation.getHasDisplayedInstallationComplete() {
          installation.setHasDisplayedInstallationComplete()
        }
      default:
        currentPage = .completed
      }
    }
   
  }
  
  var body: some View {
    
    VStack {
      // The switch statement below updates the view this VStack contains whenever currentPage changes value
      switch currentPage {
      case .loading: ProgressView()
      case .initialInstall: InitialInstallView(namespace: animation,onContinue: {
        installation.executeCurrentInstallationTask()
        chooseCurrentPage()
      })
      case .initialRepair: InitialRepairView(namespace: animation,onContinue: {
        installation.executeCurrentInstallationTask()
        chooseCurrentPage()
      })
      case .completed: CompletedInstallView(namespace: animation)
      case .enableInputMethod: EnableInputMethodView(namespace: animation, onContinue: chooseCurrentPage)
      case .allowSecurityPermission: GrantAccessibiltyPermissionView(namespace: animation, onContinue: chooseCurrentPage)
      case .rerunInstaller: RerunInstallerView(namespace: animation)
      case .restartMac: RestartComputerView(namespace: animation)
      }
    }
    // While the installer is evaluating the Keyman installation, the loading screen will be shown
    .onAppear {      
      if installation.installationPhase == .evaluatingInstallation {
        currentPage = .loading
        Task {
          while installation.installationPhase == .evaluatingInstallation {
            try? await Task.sleep(for: .milliseconds(200))
          }
          await MainActor.run {
            withAnimation(.smooth) {
              chooseCurrentPage()
            }
          }
        }
      } else {
        chooseCurrentPage()
      }
    }
    .padding()
    .frame(
      minWidth: 600, idealWidth: 600, maxWidth: 600,
      minHeight: 500, idealHeight: 500, maxHeight: 500
  )
  }
}
