object modActionsKeyboardEditor: TmodActionsKeyboardEditor
  OldCreateOrder = False
  Height = 333
  Width = 408
  object actionsKeyboardEditor: TActionList
    Images = frmKeymanDeveloper.lstImages
    Left = 60
    Top = 24
    object actKeyboardCompile: TAction
      Category = 'Keyboard'
      Caption = '&Compile Keyboard'
      ImageIndex = 37
      ShortCut = 118
      OnExecute = actKeyboardCompileExecute
      OnUpdate = actKeyboardCompileUpdate
    end
    object actKeyboardIncludeDebugInformation: TAction
      Category = 'Keyboard'
      Caption = 'Include &Debug Information'
      OnExecute = actKeyboardIncludeDebugInformationExecute
      OnUpdate = actKeyboardIncludeDebugInformationUpdate
    end
    object actKeyboardInstall: TAction
      Category = 'Keyboard'
      Caption = '&Install...'
      ImageIndex = 41
      ShortCut = 16504
      OnExecute = actKeyboardInstallExecute
      OnUpdate = actKeyboardInstallUpdate
    end
    object actKeyboardUninstall: TAction
      Category = 'Keyboard'
      Caption = '&Uninstall...'
      ImageIndex = 42
      OnExecute = actKeyboardUninstallExecute
      OnUpdate = actKeyboardUninstallUpdate
    end
    object actKeyboardTest: TAction
      Category = 'Keyboard'
      Caption = '&Test Keyboard'
      OnExecute = actKeyboardTestExecute
      OnUpdate = actKeyboardTestUpdate
    end
    object actKeyboardTestKeymanWeb: TAction
      Category = 'Keyboard'
      Caption = 'Test &Keyboard on web'
      OnExecute = actKeyboardTestKeymanWebExecute
      OnUpdate = actKeyboardTestKeymanWebUpdate
    end
    object actKeyboardFontHelper: TAction
      Category = 'Keyboard'
      Caption = 'Font &Helper...'
      OnExecute = actKeyboardFontHelperExecute
      OnUpdate = actKeyboardFontHelperUpdate
    end
    object actKeyboardFonts: TAction
      Category = 'Keyboard'
      Caption = '&Fonts...'
      OnExecute = actKeyboardFontsExecute
      OnUpdate = actKeyboardFontsUpdate
    end
  end
end
