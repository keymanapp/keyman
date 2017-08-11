object modActionsKeyboardEditor: TmodActionsKeyboardEditor
  OldCreateOrder = False
  Height = 333
  Width = 408
  object actionsKeyboardEditor: TActionList
    Images = frmKeymanDeveloper.lstImages
    OnUpdate = actionsKeyboardEditorUpdate
    Left = 60
    Top = 24
    object actKeyboardCompile: TAction
      Category = 'Keyboard'
      Caption = '&Compile Keyboard'
      ImageIndex = 37
      ShortCut = 118
      OnExecute = actKeyboardCompileExecute
    end
    object actKeyboardIncludeDebugInformation: TAction
      Category = 'Keyboard'
      Caption = 'Include &Debug Information'
      OnExecute = actKeyboardIncludeDebugInformationExecute
      OnUpdate = actKeyboardIncludeDebugInformationUpdate
    end
    object actKeyboardCreatePackage: TAction
      Category = 'Keyboard'
      Caption = '&Package for Distribution...'
      ImageIndex = 40
      OnExecute = actKeyboardCreatePackageExecute
    end
    object actKeyboardInstall: TAction
      Category = 'Keyboard'
      Caption = '&Install...'
      ImageIndex = 41
      ShortCut = 16504
      OnExecute = actKeyboardInstallExecute
    end
    object actKeyboardUninstall: TAction
      Category = 'Keyboard'
      Caption = '&Uninstall...'
      ImageIndex = 42
      OnExecute = actKeyboardUninstallExecute
    end
    object actKeyboardTest: TAction
      Category = 'Keyboard'
      Caption = '&Test Keyboard'
      OnExecute = actKeyboardTestExecute
    end
    object actKeyboardTestKeymanWeb: TAction
      Category = 'Keyboard'
      Caption = 'Test &Keyboard on web'
      OnExecute = actKeyboardTestKeymanWebExecute
    end
    object actKeyboardFontHelper: TAction
      Category = 'Keyboard'
      Caption = 'Font &Helper...'
      OnExecute = actKeyboardFontHelperExecute
    end
    object actKeyboardFonts: TAction
      Category = 'Keyboard'
      Caption = '&Fonts...'
      OnExecute = actKeyboardFontsExecute
    end
  end
  object actionsDebug: TActionList
    Images = frmKeymanDeveloper.lstImages
    Left = 60
    Top = 88
    object actDebugStartDebugger: TAction
      Category = 'Debug Control'
      Caption = '&Start Debugging'
      ImageIndex = 44
      ShortCut = 116
      OnExecute = actDebugStartDebuggerExecute
      OnUpdate = actDebugStartDebuggerUpdate
    end
    object actDebugStopDebugger: TAction
      Category = 'Debug Control'
      Caption = 'Stop Debugger'
      ImageIndex = 45
      ShortCut = 8308
      OnExecute = actDebugStopDebuggerExecute
      OnUpdate = actDebugStopDebuggerUpdate
    end
    object actDebugSetClearBreakpoint: TAction
      Category = 'Debug Control'
      Caption = 'Set/Clear &Breakpoint'
      ImageIndex = 43
      ShortCut = 120
      OnExecute = actDebugSetClearBreakpointExecute
      OnUpdate = actDebugSetClearBreakpointUpdate
    end
    object actDebugSingleStepMode: TAction
      Category = 'Debug Action'
      Caption = 'Single Step &Mode'
      ImageIndex = 46
      OnExecute = actDebugSingleStepModeExecute
      OnUpdate = actDebugSingleStepModeUpdate
    end
    object actDebugStepForward: TAction
      Category = 'Debug Action'
      Caption = 'Step &Forward'
      ImageIndex = 47
      OnExecute = actDebugStepForwardExecute
      OnUpdate = actDebugStepForwardUpdate
    end
    object actDebugRun: TAction
      Category = 'Debug Action'
      Caption = '&Run'
      ImageIndex = 48
      OnExecute = actDebugRunExecute
      OnUpdate = actDebugRunUpdate
    end
    object actDebugSelectSystemKeyboard: TAction
      Category = 'Debug Control'
      Caption = 'Select System &Keyboard...'
      ImageIndex = 49
      OnExecute = actDebugSelectSystemKeyboardExecute
      OnUpdate = actDebugSelectSystemKeyboardUpdate
    end
    object actDebugANSITestMode: TAction
      Category = 'Debug Control'
      Caption = '&ANSI Test Mode'
    end
    object actDebugViewElements: TAction
      Category = 'Debug Control'
      Caption = '&Elements'
      ImageIndex = 50
      OnExecute = actDebugViewElementsExecute
      OnUpdate = actDebugViewElementsUpdate
    end
    object actDebugViewCallStack: TAction
      Category = 'Debug Control'
      Caption = '&Call Stack'
      ImageIndex = 51
      OnExecute = actDebugViewCallStackExecute
      OnUpdate = actDebugViewCallStackUpdate
    end
    object actDebugViewDeadkeys: TAction
      Category = 'Debug Control'
      Caption = '&Deadkeys'
      ImageIndex = 52
      OnExecute = actDebugViewDeadkeysExecute
      OnUpdate = actDebugViewDeadkeysUpdate
    end
    object actDebugViewRegressionTesting: TAction
      Category = 'Debug Control'
      Caption = '&Regression Testing'
      OnExecute = actDebugViewRegressionTestingExecute
      OnUpdate = actDebugViewRegressionTestingUpdate
    end
    object actDebugTestMode: TAction
      Category = 'Debug Control'
      Caption = '&Test Mode'
      OnExecute = actDebugTestModeExecute
      OnUpdate = actDebugTestModeUpdate
    end
    object actDebugDebuggerMode: TAction
      Category = 'Debug Control'
      Caption = '&Debugger Mode'
      OnExecute = actDebugDebuggerModeExecute
      OnUpdate = actDebugDebuggerModeUpdate
    end
    object actDebugPause: TAction
      Category = 'Debug Action'
      Caption = '&Pause'
      ImageIndex = 57
      OnExecute = actDebugPauseExecute
      OnUpdate = actDebugPauseUpdate
    end
    object actDebugViewFont: TAction
      Category = 'Debug Control'
      Caption = '&Font...'
      OnExecute = actDebugViewFontExecute
      OnUpdate = actDebugViewFontUpdate
    end
    object actDebugViewDefaultFont: TAction
      Category = 'Debug Control'
      Caption = 'Use Editor &Character Font'
      OnExecute = actDebugViewDefaultFontExecute
      OnUpdate = actDebugViewDefaultFontUpdate
    end
    object actDebugViewState: TAction
      Category = 'Debug Control'
      Caption = '&State'
      OnExecute = actDebugViewStateExecute
      OnUpdate = actDebugViewStateUpdate
    end
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 256
    Top = 73
  end
end
