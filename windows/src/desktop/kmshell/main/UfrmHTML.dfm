object frmHTML: TfrmHTML
  Left = 192
  Top = 107
  ClientHeight = 313
  ClientWidth = 477
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
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  DesignSize = (
    477
    313)
  PixelsPerInch = 96
  TextHeight = 13
  object panHTML: TPanel
    Left = 0
    Top = 0
    Width = 477
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object web: TKeymanEmbeddedWB
      Left = 0
      Top = 0
      Width = 477
      Height = 273
      Align = alClient
      TabOrder = 0
      Silent = False
      OnCommandStateChange = webCommandStateChange
      OnNewWindow3 = webNewWindow3
      DisableCtrlShortcuts = 'N'
      UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
      About = ' EmbeddedWB http://bsalsa.com/'
      DisableErrors.fpExceptions = False
      PrintOptions.HTMLHeader.Strings = (
        '<HTML></HTML>')
      PrintOptions.Orientation = poPortrait
      OnKeyDown = webKeyDown
      ExplicitTop = 1
      ControlData = {
        4C000000A52F0000631B00000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E12620C000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
  object cmdOK: TButton
    Left = 202
    Top = 280
    Width = 73
    Height = 25
    Anchors = [akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object cmdPrint: TButton
    Left = 8
    Top = 280
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Print...'
    TabOrder = 1
    OnClick = cmdPrintClick
  end
  object cmdBack: TButton
    Left = 87
    Top = 280
    Width = 30
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '<'
    TabOrder = 3
    OnClick = cmdBackClick
  end
  object cmdForward: TButton
    Left = 123
    Top = 280
    Width = 30
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '>'
    TabOrder = 4
    OnClick = cmdForwardClick
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 232
    Top = 160
  end
end
