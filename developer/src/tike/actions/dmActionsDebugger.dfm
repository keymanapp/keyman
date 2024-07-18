object modActionsDebugger: TmodActionsDebugger
  OldCreateOrder = False
  Height = 333
  Width = 408
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
      Visible = False
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
      GroupIndex = 1
      OnExecute = actDebugTestModeExecute
      OnUpdate = actDebugTestModeUpdate
    end
    object actDebugDebuggerMode: TAction
      Category = 'Debug Control'
      Caption = '&Debugger Mode'
      GroupIndex = 2
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
    object actDebugSwitchToDebuggerWindow: TAction
      Category = 'Debug Control'
      Caption = 'Switch to debugger'
      ShortCut = 117
      OnExecute = actDebugSwitchToDebuggerWindowExecute
      OnUpdate = actDebugSwitchToDebuggerWindowUpdate
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
