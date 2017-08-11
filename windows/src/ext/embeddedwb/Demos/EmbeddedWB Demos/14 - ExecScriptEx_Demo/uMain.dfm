object Form1: TForm1
  Left = 236
  Top = 168
  BorderStyle = bsDialog
  Caption = 'EmbeddedWB - ExecScriptEx Demo'
  ClientHeight = 448
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 653
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 8
      Height = 13
      Caption = 'x:'
    end
    object Button1: TButton
      Left = 99
      Top = 8
      Width = 121
      Height = 24
      Caption = 'Execute Script'
      TabOrder = 0
      OnClick = Button1Click
    end
    object edPara: TEdit
      Left = 24
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 1
      Text = '5'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 313
    Height = 407
    Align = alLeft
    TabOrder = 1
    object Memo1: TMemo
      Left = 1
      Top = 23
      Width = 311
      Height = 383
      Align = alClient
      BorderStyle = bsNone
      Lines.Strings = (
        '<HTML> '
        '<HEAD> '
        '<TITLE>Test Script</TITLE> '
        '<SCRIPT> function evaluate(x) '
        '{  alert("Hello from the script evaluate(x)");  return '
        'eval(x * x);} '
        '</SCRIPT> '
        '</HEAD> '
        '<BODY> TEST Script: eval(x * x)</BODY> '
        '</HTML>')
      ReadOnly = True
      TabOrder = 0
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 311
      Height = 22
      Align = alTop
      Caption = 'Script'
      TabOrder = 1
    end
  end
  object Panel3: TPanel
    Left = 313
    Top = 41
    Width = 340
    Height = 407
    Align = alClient
    TabOrder = 2
    object EmbeddedWB1: TEmbeddedWB
      Left = 1
      Top = 23
      Width = 338
      Height = 383
      Align = alClient
      TabOrder = 0
      Silent = False
      DisableCtrlShortcuts = 'N'
      UserInterfaceOptions = [DontUse3DBorders, DontUseScrollBars, ForceFlatScrollBars, EnablesFormsAutoComplete, EnableThemes]
      About = ' EmbeddedWB http://bsalsa.com/'
      HTMLCode.Strings = (
        '<HTML> '
        '<HEAD> '
        '<TITLE>Test Script</TITLE> '
        '<SCRIPT> function evaluate(x) '
        
          '{  alert("Hello from the script evaluate(x)");  return eval(x * ' +
          'x);} '
        '</SCRIPT> '
        '</HEAD> '
        '<BODY> TEST Script: eval(x * x)</BODY> '
        '</HTML>')
      PrintOptions.Margins.Left = 19.05
      PrintOptions.Margins.Right = 19.05
      PrintOptions.Margins.Top = 19.05
      PrintOptions.Margins.Bottom = 19.05
      PrintOptions.Header = '&w&bPage &p of &P'
      PrintOptions.HTMLHeader.Strings = (
        '<HTML></HTML>')
      PrintOptions.Footer = '&u&b&d'
      PrintOptions.Orientation = poPortrait
      ControlData = {
        4C0000007D430000112A00000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 338
      Height = 22
      Align = alTop
      Caption = 'EmbeddedWB'
      TabOrder = 1
    end
  end
end
