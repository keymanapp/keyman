object frmExceptionHandler: TfrmExceptionHandler
  Left = 0
  Top = 0
  ActiveControl = editEmail
  Caption = 'Exception in Keyman Developer'
  ClientHeight = 474
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  DesignSize = (
    497
    474)
  PixelsPerInch = 96
  TextHeight = 13
  object bevelTitle: TBevel
    Left = 0
    Top = 51
    Width = 497
    Height = 3
    Align = alTop
    Shape = bsBottomLine
    ExplicitTop = 49
  end
  object lblText1: TLabel
    Left = 12
    Top = 68
    Width = 473
    Height = 13
    AutoSize = False
    Caption = 
      'If you were in the middle of something, the information you were' +
      ' working on might be lost.'
  end
  object lblText2: TLabel
    Left = 12
    Top = 92
    Width = 473
    Height = 26
    AutoSize = False
    Caption = 
      'An error report has been created that you can send to Tavultesof' +
      't.  We will treat this report as confidential.'
    WordWrap = True
  end
  object lblText3: TLabel
    Left = 12
    Top = 128
    Width = 473
    Height = 26
    AutoSize = False
    Caption = 
      'Please tell us what you were doing when the problem occurred, so' +
      ' that we can identify and address the problem you experienced.'
    WordWrap = True
  end
  object lblText4: TLabel
    Left = 12
    Top = 164
    Width = 473
    Height = 26
    AutoSize = False
    Caption = 
      'If you enter your email address, you will be automatically notif' +
      'ied if an update becomes available that addresses this problem.'
    WordWrap = True
  end
  object lblPrivacy: TLabel
    Left = 344
    Top = 442
    Width = 140
    Height = 13
    Cursor = crHandPoint
    Caption = 'View our privacy policy online'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblPrivacyClick
  end
  object cmdSend: TButton
    Left = 12
    Top = 437
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Send to Keyman Support'
    Default = True
    TabOrder = 2
    OnClick = cmdSendClick
  end
  object cmdAbort: TButton
    Left = 124
    Top = 437
    Width = 105
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'E&xit Application'
    TabOrder = 3
    OnClick = cmdAbortClick
  end
  object cmdContinue: TButton
    Left = 236
    Top = 437
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Continue'
    TabOrder = 4
    Visible = False
    OnClick = cmdContinueClick
  end
  object panTitle: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 51
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 16
      Top = 12
      Width = 409
      Height = 26
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object imgIcon: TImage
      Left = 452
      Top = 8
      Width = 32
      Height = 32
    end
  end
  object pages: TPageControl
    Left = 12
    Top = 200
    Width = 473
    Height = 229
    ActivePage = tabInfo
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabInfo: TTabSheet
      Caption = 'Comments'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        465
        201)
      object lblEmail: TLabel
        Left = 4
        Top = 8
        Width = 139
        Height = 13
        Caption = 'Your email address (optional)'
      end
      object lblComments: TLabel
        Left = 4
        Top = 56
        Width = 291
        Height = 13
        Caption = 'What were you doing when the problem occurred? (optional)'
      end
      object editEmail: TEdit
        Left = 4
        Top = 24
        Width = 233
        Height = 21
        TabOrder = 0
      end
      object memoComments: TMemo
        Left = 4
        Top = 72
        Width = 453
        Height = 101
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
      end
      object chkViewTechnicalReport: TCheckBox
        Left = 4
        Top = 180
        Width = 125
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = '&View Technical Report'
        TabOrder = 2
        OnClick = chkViewTechnicalReportClick
      end
    end
    object tabDetails: TTabSheet
      Caption = 'Technical Report'
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        465
        201)
      object memoCrashText: TMemo
        Left = 4
        Top = 4
        Width = 457
        Height = 69
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'memoCrashText')
        ParentColor = True
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object memoDetails: TMemo
        Left = 4
        Top = 79
        Width = 457
        Height = 118
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'memoDetails')
        ParentColor = True
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
  end
end
