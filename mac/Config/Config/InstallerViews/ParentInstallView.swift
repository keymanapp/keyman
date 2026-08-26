//
//  ParentInstallView.swift
//  Config
//
//  Created by Eli Schantz on 6/29/26.
//

import SwiftUI

struct ParentInstallView: View {
  @EnvironmentObject var installation: InstallationContainer
  @Namespace var animation
  @State public var currentPage: InstallPage = .initial
  
  func chooseCurrentPage() -> Void {
    if installation.installationPhase.hasTasks {
      switch installation.currentTask()?.taskType {
      case .prepareNewInstall: currentPage = .initial
      case .enableInputMethod:
        currentPage = .enableInputMethod
      case .requestAccess: currentPage =
          .allowSecurityPermission
      case .restartMac: currentPage = .restartComputer
      default: currentPage = .completed
      }
    } else {
      switch installation.installationPhase {
      case .evaluatingInstallation: currentPage = .loading
      case .inputMethodMissing, .inputMethodOutdated: currentPage = .rerunInstaller
      case .installationComplete: currentPage = .completed
      default: currentPage = .completed
      }
    }
  }
  
  var body: some View {
    
    ZStack {
      switch currentPage {
      case .loading: KeymanLogo(namespace: animation)
      case .initial: InitialView(namespace: animation,onContinue: chooseCurrentPage)
      case .completed: CompletedInstallView(namespace: animation)
      case .enableInputMethod: EnableInputMethodView(namespace: animation, onContinue: chooseCurrentPage)
      case .allowSecurityPermission: CheckAccessibiltyPermissionView(namespace: animation, onContinue: chooseCurrentPage)
      case .rerunInstaller: RerunInstallerView(namespace: animation)
      case .restartComputer: RestartComputerView(namespace: animation)
      }
    }
    .onAppear {
      switch installation.installationPhase {
      case .evaluatingInstallation:
        currentPage = .initial
      default:
        chooseCurrentPage()
      }
    }
    .padding()
    .frame(width: 600)
    .frame(height: 400)
  }
}

#Preview {
  let installation = InstallationContainer()
  ParentInstallView()
    .environmentObject(installation)
}
