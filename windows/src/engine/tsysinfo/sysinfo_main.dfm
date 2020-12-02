object frmDiagnostics: TfrmDiagnostics
  Left = 194
  Top = 111
  Caption = 'Keyman System Information'
  ClientHeight = 344
  ClientWidth = 565
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = menu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    565
    344)
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 8
    Top = 8
    Width = 549
    Height = 328
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    Visible = False
  end
  object panSimple: TPanel
    Left = 8
    Top = 8
    Width = 549
    Height = 328
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object lblTitle: TLabel
      Left = 12
      Top = 12
      Width = 295
      Height = 19
      Caption = 'System Information Report for your PC'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblSendToKeyman: TLabel
      Left = 36
      Top = 179
      Width = 433
      Height = 39
      AutoSize = False
      Caption =
        'If Keyman Support have asked for a diagnostic report from you, y' +
        'ou can click the button below to send the report to Keyman Suppo' +
        'rt.'
      WordWrap = True
    end
    object cmdSendToKeyman: TButton
      Left = 36
      Top = 228
      Width = 205
      Height = 25
      Caption = '&Send Report to Keyman Support...'
      TabOrder = 0
      OnClick = mnuSendToKeymanClick
    end
    object panNoIssuesFound: TPanel
      Left = 28
      Top = 40
      Width = 513
      Height = 133
      BevelOuter = bvNone
      TabOrder = 1
      object lblNoIssuesFound: TLabel
        Left = 8
        Top = 12
        Width = 330
        Height = 26
        Caption =
          'Keyman System Information did not detect any issues in y' +
          'our system.'
        WordWrap = True
      end
    end
    object panIssuesFound: TPanel
      Left = 28
      Top = 40
      Width = 493
      Height = 137
      BevelOuter = bvNone
      TabOrder = 2
      object lblIssuesFound: TLabel
        Left = 8
        Top = 5
        Width = 215
        Height = 13
        Caption = 'The following items may conflict with Keyman:'
      end
      object lblSendSuggestion: TLabel
        Left = 8
        Top = 104
        Width = 400
        Height = 26
        Caption =
          'You may wish to try disabling the programs listed above, if you ' +
          'recognise them, to see if this resolves the problem.'
        WordWrap = True
      end
      object lbIssues: TListBox
        Left = 8
        Top = 24
        Width = 485
        Height = 69
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object Panel1: TPanel
      Left = 28
      Top = 40
      Width = 493
      Height = 137
      BevelOuter = bvNone
      TabOrder = 3
      object Label1: TLabel
        Left = 8
        Top = 5
        Width = 379
        Height = 39
        Caption =
          'This diagnostic report contains a list of all the programs curre' +
          'ntly running on your computer, as well as information on the lan' +
          'guages you have installed and your Keyman keyboards and ' +
          'options.'
        WordWrap = True
      end
      object Label2: TLabel
        Left = 8
        Top = 60
        Width = 424
        Height = 26
        Caption =
          'Use this diagnostic report to help resolve any issues that you m' +
          'ay have experienced while using Keyman.'
        WordWrap = True
      end
      object Label3: TLabel
        Left = 8
        Top = 100
        Width = 367
        Height = 13
        Caption =
          'You can view all the details of the report by selecting Options/' +
          'Advanced View'
      end
    end
  end
  object menu: TMainMenu
    Left = 320
    Top = 4
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuFileOpen: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = mnuFileOpenClick
      end
      object mnuFileSaveAs: TMenuItem
        Caption = '&Save as...'
        ShortCut = 16467
        OnClick = mnuFileSaveAsClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuFileRefresh: TMenuItem
        Caption = '&Refresh'
        ShortCut = 116
        OnClick = mnuFileRefreshClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuSendToKeyman: TMenuItem
        Caption = '&Send Report to Keyman Support...'
        ShortCut = 16453
        Visible = False
        OnClick = mnuSendToKeymanClick
      end
      object N2: TMenuItem
        Caption = '-'
        Visible = False
      end
      object mnuExit: TMenuItem
        Caption = 'E&xit'
        OnClick = mnuExitClick
      end
    end
    object mnuOptions: TMenuItem
      Caption = '&Options'
      OnClick = mnuOptionsClick
      object mnuOptionsAdvancedView: TMenuItem
        Caption = '&Advanced View'
        OnClick = mnuOptionsAdvancedViewClick
      end
      object mnuOptionsXMLView: TMenuItem
        Caption = '&XML View'
        ShortCut = 16472
        OnClick = mnuOptionsXMLViewClick
      end
      object Reloadpage1: TMenuItem
        Caption = 'Reload page'
        ShortCut = 16466
      end
      object mnuOptionsDebugTests: TMenuItem
        Caption = 'Debug Tests'
        object mnuOptionsSentryExceptionTest: TMenuItem
          Caption = 'Sentry Exception Test'
          OnClick = mnuOptionsSentryExceptionTestClick
        end
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      object mnuHelpAbout: TMenuItem
        Caption = '&About...'
        OnClick = mnuHelpAboutClick
      end
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'tsi'
    Filter =
      'Diagnostic Report files (*.tsi, *.xml)|*.tsi;*.xml|Compressed Di' +
      'agnostic Report files (*.tsi)|*.tsi|Diagnostic Report XML files ' +
      '(*.xml)|*.xml|All files (*.*)|*.*'
    Left = 384
    Top = 4
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'xml'
    Filter =
      'Diagnostic Report File (*.tsi)|*.tsi|Diagnostic Report XML files' +
      ' (*.xml)|*.xml|All files (*.*)|*.*'
    Left = 448
    Top = 4
  end
end
