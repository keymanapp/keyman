object frmRegressionTestsWaitForIdle: TfrmRegressionTestsWaitForIdle
  Left = 192
  Top = 362
  BorderStyle = bsDialog
  Caption = 'Wait for Idle Keyboard'
  ClientHeight = 181
  ClientWidth = 317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 274
    Height = 26
    Caption = 
      'The following keys are currently in incorrect state.  Please pre' +
      'ss and release them:'
    WordWrap = True
  end
  object lblDownKeys: TLabel
    Left = 20
    Top = 44
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 8
    Top = 76
    Width = 276
    Height = 26
    Caption = 
      'The following keys are activated.  Please deactivate them by pre' +
      'ssing them once:'
    WordWrap = True
  end
  object lblActiveKeys: TLabel
    Left = 20
    Top = 112
    Width = 3
    Height = 13
  end
  object cmdCancel: TButton
    Left = 236
    Top = 148
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 0
  end
  object cmdForce: TButton
    Left = 152
    Top = 148
    Width = 73
    Height = 25
    Caption = '&Force'
    TabOrder = 1
    OnClick = cmdForceClick
  end
end
