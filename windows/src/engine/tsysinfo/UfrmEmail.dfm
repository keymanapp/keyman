object frmEmail: TfrmEmail
  Left = 243
  Top = 249
  ActiveControl = editEmail
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Send Diagnostics to Keyman Support'
  ClientHeight = 345
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblEmail: TLabel
    Left = 12
    Top = 33
    Width = 68
    Height = 13
    Caption = '&Email address:'
    FocusControl = editEmail
  end
  object lblIntro: TLabel
    Left = 12
    Top = 12
    Width = 188
    Height = 13
    Caption = 'Please enter your contact details below:'
  end
  object Label1: TLabel
    Left = 12
    Top = 124
    Width = 439
    Height = 26
    Caption =
      '&Please enter as many details as you can of any problems you are' +
      ' experiencing using Keyman, to assist us in finding a re' +
      'solution:'
    FocusControl = memoBody
    WordWrap = True
  end
  object lblCase: TLabel
    Left = 12
    Top = 80
    Width = 183
    Height = 13
    Caption = 'Please enter a brief &title for the problem'
    FocusControl = editTitle
  end
  object lblSecure: TLabel
    Left = 180
    Top = 313
    Width = 154
    Height = 13
    Caption = 'This report will be sent to (api url)'
  end
  object Image1: TImage
    Left = 472
    Top = 311
    Width = 12
    Height = 15
    AutoSize = True
    Picture.Data = {
      07544269746D6170EE000000424DEE0000000000000076000000280000000C00
      00000F000000010004000000000078000000130B0000130B0000100000001000
      00000000000000848400C6C6C600D6E7E700DEEFEF0000FFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000003000011111111111000001655555555100000161111111210
      0000165555555510000016111111121000001655555555100000166666666610
      0000311111111110000033101111101300003310333310330000331033331033
      0000331203311033000033312001033300004444111144440000}
  end
  object lblPrivacy1: TLabel
    Left = 12
    Top = 256
    Width = 106
    Height = 13
    Caption = 'Privacy statement:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPrivacy2: TLabel
    Left = 120
    Top = 256
    Width = 361
    Height = 13
    Caption =
      'No personally identifiable information is included in this repor' +
      't.  You can view '
  end
  object lblPrivacy3: TLabel
    Left = 12
    Top = 270
    Width = 464
    Height = 26
    Caption =
      'all the information sent in the report by selecting Advanced Vie' +
      'w in the Options menu while viewing the report.  Please see our ' +
      'online'
    WordWrap = True
  end
  object lblPrivacyStatement: TLabel
    Left = 170
    Top = 283
    Width = 83
    Height = 13
    Cursor = crHandPoint
    Caption = 'privacy statement'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = lblPrivacyStatementClick
  end
  object lblPrivacy4: TLabel
    Left = 256
    Top = 283
    Width = 78
    Height = 13
    Caption = 'for further details'
  end
  object lblName: TLabel
    Left = 12
    Top = 56
    Width = 54
    Height = 13
    Caption = 'Your &name:'
    FocusControl = editName
  end
  object editEmail: TEdit
    Left = 96
    Top = 30
    Width = 213
    Height = 21
    MaxLength = 128
    TabOrder = 0
  end
  object cmdSend: TButton
    Left = 12
    Top = 308
    Width = 73
    Height = 25
    Caption = '&Send'
    Default = True
    TabOrder = 4
    OnClick = cmdSendClick
  end
  object cmdCancel: TButton
    Left = 96
    Top = 308
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object memoBody: TMemo
    Left = 12
    Top = 156
    Width = 473
    Height = 89
    TabOrder = 3
  end
  object editTitle: TEdit
    Left = 12
    Top = 96
    Width = 473
    Height = 21
    TabOrder = 2
  end
  object editName: TEdit
    Left = 96
    Top = 53
    Width = 213
    Height = 21
    MaxLength = 128
    TabOrder = 1
  end
end
