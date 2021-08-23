inherited frmMustIncludeDebug: TfrmMustIncludeDebug
  Left = 192
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Keyman Developer Debugger'
  ClientHeight = 133
  ClientWidth = 393
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  ExplicitWidth = 399
  ExplicitHeight = 162
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 366
    Height = 39
    Caption = 
      'You must include debug information in the compiled keyboard befo' +
      're you can use the debugger or test window.  You can do this you' +
      'rself by selecting the "Include debug information" option in the' +
      ' Keyboard menu.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 52
    Width = 343
    Height = 13
    Caption = 
      'Recompile now with debug information and start the debug/test se' +
      'ssion?'
  end
  object cmdOK: TButton
    Left = 120
    Top = 100
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 200
    Top = 100
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object chkAutoRecompile: TCheckBox
    Left = 8
    Top = 72
    Width = 343
    Height = 17
    Caption = 
      '&Always recompile with debug information before debugging or tes' +
      'ting.'
    TabOrder = 0
  end
end
