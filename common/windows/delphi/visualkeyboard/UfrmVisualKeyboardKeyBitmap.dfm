object frmVisualKeyboardKeyBitmap: TfrmVisualKeyboardKeyBitmap
  Left = 229
  Top = 292
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Customise Key Image'
  ClientHeight = 238
  ClientWidth = 501
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
  object imgKeyImage: TImage
    Left = 88
    Top = 152
    Width = 45
    Height = 33
  end
  object lblOptions2: TLabel
    Left = 8
    Top = 8
    Width = 105
    Height = 13
    Caption = 'Custom Key Image'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblOptions3: TLabel
    Left = 8
    Top = 24
    Width = 401
    Height = 52
    AutoSize = False
    Caption = 
      'You can change the appearance of the visual keyboard keys.  The ' +
      'bitmap file must have six images in a row: normal (pressed and u' +
      'npressed), control (pressed and unpressed), selected (pressed an' +
      'd unpressed).'
    WordWrap = True
  end
  object lblOptions4: TLabel
    Left = 8
    Top = 88
    Width = 366
    Height = 26
    Caption = 
      'The image will be stored inside the visual keyboard file, so the' +
      're is no need to distribute the original image file.'
    WordWrap = True
  end
  object lblKeyImageParameters: TLabel
    Left = 6
    Top = 189
    Width = 56
    Height = 13
    Caption = '&Parameters:'
  end
  object lblOptions5: TLabel
    Left = 9
    Top = 120
    Width = 384
    Height = 26
    Caption = 
      'The parameters option defines the character position on the key.' +
      '  Please read the help file for details.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 68
    Width = 335
    Height = 13
    Caption = 
      'Keep the key face blank.  Keyman will insert the characters on t' +
      'he key.'
  end
  object cmdBrowseKeyImage: TButton
    Left = 8
    Top = 151
    Width = 73
    Height = 25
    Caption = '&Browse...'
    TabOrder = 0
    OnClick = cmdBrowseKeyImageClick
  end
  object editKeyImageParameters: TEdit
    Left = 6
    Top = 204
    Width = 443
    Height = 21
    TabOrder = 1
  end
  object cmdOK: TButton
    Left = 420
    Top = 8
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 420
    Top = 40
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object dlgBrowseBitmap: TOpenPictureDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofCreatePrompt, ofEnableSizing]
    Title = 'Select Keyboard Bitmap'
    Left = 464
    Top = 73
  end
end
