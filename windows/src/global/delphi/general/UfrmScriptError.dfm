object frmScriptError: TfrmScriptError
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Script Error'
  ClientHeight = 201
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblBlurb1: TLabel
    Left = 16
    Top = 16
    Width = 455
    Height = 13
    Caption = 
      'We are very sorry but an error has been encountered in this wind' +
      'ow.  The error received was:'
  end
  object lblError: TLabel
    Left = 36
    Top = 44
    Width = 397
    Height = 45
    AutoSize = False
  end
  object lblBlurb2: TLabel
    Left = 16
    Top = 108
    Width = 321
    Height = 13
    Caption = 
      'Do you want to try continuing?  In many cases this will be just ' +
      'fine.'
  end
  object cmdContinue: TButton
    Left = 16
    Top = 161
    Width = 95
    Height = 25
    Caption = '&Try and continue'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object cmdExit: TButton
    Left = 376
    Top = 161
    Width = 95
    Height = 25
    Caption = 'E&xit Application'
    ModalResult = 3
    TabOrder = 1
  end
  object cmdCancel: TButton
    Left = 117
    Top = 161
    Width = 95
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object chkTellKeymanSupport: TCheckBox
    Left = 16
    Top = 138
    Width = 225
    Height = 17
    Caption = 'Tell Keyman Support about this error'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
