inherited frmMustIncludeDebug: TfrmMustIncludeDebug
  Left = 192
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Keyman Developer Debugger'
  ClientHeight = 133
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 366
    Height = 39
    Caption = 
      'You must include debug information in the compiled keyboard befo' +
      're you can use the debugger.  You can do this yourself by select' +
      'ing the "Include debug information" option in the Keyboard menu.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 52
    Width = 298
    Height = 13
    Caption = 'Recompile now with debug information and start the debugger?'
  end
  object cmdOK: TButton
    Left = 56
    Top = 100
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = cmdOKClick
  end
  object cmdTestWithoutDebugger: TButton
    Left = 216
    Top = 100
    Width = 121
    Height = 25
    Caption = '&Test without debugger'
    ModalResult = 7
    TabOrder = 3
  end
  object cmdCancel: TButton
    Left = 136
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
    Width = 333
    Height = 17
    Caption = '&Always recompile with debug information before debugging.'
    TabOrder = 0
  end
end
