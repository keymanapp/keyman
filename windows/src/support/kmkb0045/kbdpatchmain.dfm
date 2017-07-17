object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'System Shadow non-Unicode Keyboard Patch'
  ClientHeight = 341
  ClientWidth = 629
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 217
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Run Now'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 337
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Exit'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 613
    Height = 281
    Lines.Strings = (
      
        'This utility corrects a specific problem with legacy non-Unicode' +
        ' Keyman keyboards on Windows Vista, when configured as a '
      
        'system shadow keyboard (that is, adding the keyboard through Con' +
        'trol Panel/Regional and Language Options).'
      ''
      
        'This problem has not been documented on other versions of Window' +
        's and this utility should not be used except on '
      
        'Windows Vista, unless otherwise instructed by Tavultesoft Suppor' +
        't.'
      ''
      
        'This utility should be run after installing a Keyman keyboard.  ' +
        'It is best to run it before adding the keyboard through '
      'Control Panel/Regional and Language Options.'
      ''
      
        'Please make sure Keyman Desktop and Keyman Desktop Configuration' +
        ' are not running before starting this utility.'
      ''
      
        'You may need to restart Windows to complete this process.  If yo' +
        'u use Toolbox, you will need to reconfigure the keyboards '
      'in Toolbox after running this utility.')
    TabOrder = 2
  end
  object chkDontWrite: TCheckBox
    Left = 8
    Top = 308
    Width = 169
    Height = 17
    Caption = 'Dry Run (don'#39't save changes)'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
