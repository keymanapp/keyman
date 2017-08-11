object frmDebugNotify: TfrmDebugNotify
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Keyman Debug Notification'
  ClientHeight = 321
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblNotes: TLabel
    Left = 12
    Top = 17
    Width = 240
    Height = 13
    Caption = 'Note &details about this issue to store in the log file'
    FocusControl = memoNotes
  end
  object lblLogFileName: TLabel
    Left = 12
    Top = 259
    Width = 67
    Height = 13
    Caption = '&Log file name:'
    FocusControl = editLogFileName
  end
  object memoNotes: TMemo
    Left = 12
    Top = 36
    Width = 529
    Height = 209
    TabOrder = 0
  end
  object editLogFileName: TEdit
    Left = 92
    Top = 256
    Width = 449
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object chkNotepad: TCheckBox
    Left = 92
    Top = 288
    Width = 133
    Height = 17
    Caption = 'Open log file in &Notepad'
    TabOrder = 2
  end
  object cmdOK: TButton
    Left = 472
    Top = 283
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = cmdOKClick
  end
  object chkClipboard: TCheckBox
    Left = 244
    Top = 288
    Width = 145
    Height = 17
    Caption = '&Copy file to clipboard'
    TabOrder = 3
  end
end
