object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Delphi port of ZXing QRCode'
  ClientHeight = 282
  ClientWidth = 534
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    534
    282)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 22
    Height = 13
    Caption = 'Text'
  end
  object Label2: TLabel
    Left = 8
    Top = 69
    Width = 43
    Height = 13
    Caption = 'Encoding'
  end
  object Label3: TLabel
    Left = 184
    Top = 69
    Width = 52
    Height = 13
    Caption = 'Quiet zone'
  end
  object Label4: TLabel
    Left = 296
    Top = 13
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object PaintBox1: TPaintBox
    Left = 296
    Top = 32
    Width = 230
    Height = 242
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBox1Paint
    ExplicitWidth = 331
    ExplicitHeight = 260
  end
  object edtText: TEdit
    Left = 8
    Top = 32
    Width = 265
    Height = 21
    TabOrder = 0
    Text = 'Hello world'
    OnChange = edtTextChange
  end
  object cmbEncoding: TComboBox
    Left = 8
    Top = 88
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 1
    Text = 'Auto'
    OnChange = cmbEncodingChange
    Items.Strings = (
      'Auto'
      'Numeric'
      'Alphanumeric'
      'ISO-8859-1'
      'UTF-8 without BOM'
      'UTF-8 with BOM')
  end
  object edtQuietZone: TEdit
    Left = 184
    Top = 88
    Width = 89
    Height = 21
    TabOrder = 2
    Text = '4'
    OnChange = edtQuietZoneChange
  end
end
