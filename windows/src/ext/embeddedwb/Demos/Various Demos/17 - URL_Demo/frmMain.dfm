object Form1: TForm1
  Left = 340
  Top = 295
  BorderStyle = bsSingle
  Caption = 'Url Demo'
  ClientHeight = 358
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 553
    Height = 33
    Align = alTop
    TabOrder = 0
    object IEAddress1: TIEAddress
      Left = 0
      Top = 8
      Width = 549
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      TabOrder = 0
    end
  end
  object PageControl2: TPageControl
    Left = 0
    Top = 33
    Width = 553
    Height = 325
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Parse Url'
      object Panel5: TPanel
        Left = 0
        Top = 39
        Width = 545
        Height = 258
        Align = alBottom
        TabOrder = 0
        object Label7: TLabel
          Left = 7
          Top = 6
          Width = 23
          Height = 13
          Caption = 'URL:'
        end
        object Label1: TLabel
          Left = 8
          Top = 33
          Width = 26
          Height = 13
          Caption = 'Host:'
        end
        object Label2: TLabel
          Left = 10
          Top = 79
          Width = 24
          Height = 13
          Caption = 'Port:'
        end
        object Label3: TLabel
          Left = 8
          Top = 125
          Width = 52
          Height = 13
          Caption = 'Document:'
        end
        object Label9: TLabel
          Left = 10
          Top = 171
          Width = 26
          Height = 13
          Caption = 'User:'
        end
        object Label5: TLabel
          Left = 272
          Top = 79
          Width = 43
          Height = 13
          Caption = 'Protocol:'
        end
        object Label6: TLabel
          Left = 272
          Top = 125
          Width = 50
          Height = 13
          Caption = 'Bookmark:'
        end
        object Label8: TLabel
          Left = 272
          Top = 171
          Width = 50
          Height = 13
          Caption = 'Password:'
        end
        object Label4: TLabel
          Left = 272
          Top = 33
          Width = 26
          Height = 13
          Caption = 'Path:'
        end
        object Label13: TLabel
          Left = 10
          Top = 215
          Width = 59
          Height = 13
          Caption = 'Parameters:'
        end
        object Label14: TLabel
          Left = 272
          Top = 217
          Width = 50
          Height = 13
          Caption = 'ExtraInfo:'
        end
        object edtURL: TEdit
          Left = 35
          Top = 6
          Width = 479
          Height = 21
          TabOrder = 0
        end
        object edtHost: TEdit
          Left = 8
          Top = 52
          Width = 258
          Height = 21
          TabOrder = 1
        end
        object edtPort: TEdit
          Left = 7
          Top = 98
          Width = 258
          Height = 21
          TabOrder = 2
        end
        object edtDocument: TEdit
          Left = 8
          Top = 144
          Width = 258
          Height = 21
          TabOrder = 3
        end
        object edtUser: TEdit
          Left = 10
          Top = 190
          Width = 256
          Height = 21
          TabOrder = 4
        end
        object edtPath: TEdit
          Left = 272
          Top = 52
          Width = 242
          Height = 21
          TabOrder = 5
        end
        object edtProtocol: TEdit
          Left = 271
          Top = 98
          Width = 243
          Height = 21
          TabOrder = 6
        end
        object edtBookmark: TEdit
          Left = 272
          Top = 144
          Width = 242
          Height = 21
          TabOrder = 7
        end
        object edtPass: TEdit
          Left = 272
          Top = 190
          Width = 242
          Height = 21
          TabOrder = 8
        end
        object edtParameters: TEdit
          Left = 10
          Top = 234
          Width = 256
          Height = 21
          TabOrder = 9
        end
        object edtExtraInfo: TEdit
          Left = 272
          Top = 234
          Width = 242
          Height = 21
          TabOrder = 10
        end
      end
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 545
        Height = 31
        Align = alTop
        TabOrder = 1
        object btnBuild: TButton
          Left = 358
          Top = 2
          Width = 75
          Height = 22
          Caption = 'Build Url'
          Enabled = False
          TabOrder = 0
          OnClick = btnBuildClick
        end
        object btnCreate: TButton
          Left = 439
          Top = 3
          Width = 75
          Height = 22
          Caption = 'Create Url'
          Enabled = False
          TabOrder = 1
          OnClick = btnCreateClick
        end
        object btnAnalize: TButton
          Left = 3
          Top = 3
          Width = 66
          Height = 22
          Caption = 'Analize Url'
          TabOrder = 2
          OnClick = btnAnalizeClick
        end
        object btnCrack: TButton
          Left = 75
          Top = 3
          Width = 67
          Height = 22
          Caption = 'Crack Url'
          TabOrder = 3
          OnClick = btnCrackClick
        end
        object cbCrack: TCheckBox
          Left = 149
          Top = 6
          Width = 111
          Height = 17
          Caption = 'ICU_DECODE'
          TabOrder = 4
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Url Utils'
      ImageIndex = 1
      object Panel2: TPanel
        Left = 0
        Top = 200
        Width = 545
        Height = 72
        Align = alTop
        TabOrder = 0
        object Label12: TLabel
          Left = 109
          Top = 14
          Width = 93
          Height = 13
          Caption = 'String To Compare:'
        end
        object Button2: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 21
          Caption = 'Compare Url'
          TabOrder = 0
          OnClick = Button2Click
        end
        object edtComp1: TEdit
          Left = 109
          Top = 33
          Width = 411
          Height = 21
          TabOrder = 1
          Text = 'http://www.bsalsa.com/forum/'
        end
        object edtResult: TEdit
          Left = 10
          Top = 35
          Width = 73
          Height = 21
          TabOrder = 2
          Text = 'Result:'
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 545
        Height = 105
        Align = alTop
        TabOrder = 1
        object btnCan: TButton
          Left = 8
          Top = 9
          Width = 89
          Height = 25
          Caption = 'Canonicalize Url'
          TabOrder = 0
          OnClick = btnCanClick
        end
        object edtCan: TEdit
          Left = 8
          Top = 40
          Width = 512
          Height = 21
          TabOrder = 1
          Text = 'http://www.bsalsa.com/downloads/demo folder.html?option=test'
        end
        object edtCano: TEdit
          Left = 8
          Top = 67
          Width = 512
          Height = 21
          TabOrder = 2
        end
        object cbCFlag: TCheckBox
          Left = 105
          Top = 13
          Width = 114
          Height = 17
          Caption = ' ICU_NO_ENCODE'
          TabOrder = 3
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 105
        Width = 545
        Height = 95
        Align = alTop
        TabOrder = 2
        object Label10: TLabel
          Left = 352
          Top = 8
          Width = 55
          Height = 13
          Caption = 'Reletive Url'
        end
        object Label11: TLabel
          Left = 206
          Top = 6
          Width = 39
          Height = 13
          Caption = 'Base Url'
        end
        object btnCombine: TButton
          Left = 8
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Combine Url'
          TabOrder = 0
          OnClick = btnCombineClick
        end
        object edtCombine: TEdit
          Left = 8
          Top = 68
          Width = 513
          Height = 21
          TabOrder = 1
        end
        object edtReletive: TEdit
          Left = 352
          Top = 25
          Width = 169
          Height = 21
          TabOrder = 2
          Text = 'downloads/demo folder.html?option=test'
        end
        object edtBase: TEdit
          Left = 206
          Top = 25
          Width = 140
          Height = 21
          TabOrder = 3
          Text = ' http://www.bsalsa.com/'
        end
        object cbFlag: TCheckBox
          Left = 10
          Top = 37
          Width = 111
          Height = 17
          Caption = ' ICU_NO_ENCODE'
          TabOrder = 4
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Url / Server'
      ImageIndex = 2
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 545
        Height = 297
        Align = alClient
        TabOrder = 0
        object Label15: TLabel
          Left = 8
          Top = 12
          Width = 499
          Height = 23
          Caption = 'Note: Every server OS supports different properties!'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -19
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label16: TLabel
          Left = 8
          Top = 41
          Width = 119
          Height = 13
          Caption = 'For avaliable flags go to:'
        end
        object LinkLabel1: TLinkLabel
          Left = 133
          Top = 41
          Width = 109
          Height = 29
          AutoSize = False
          Launch.AsHttp.Address = 'http://msdn2.microsoft.com/en-us/library/aa385351.aspx'
          Caption = 'Query HttpQueryFlags'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsUnderline]
          Transparent = True
        end
        object btnValid: TButton
          Left = 8
          Top = 192
          Width = 72
          Height = 23
          Caption = 'Valid Url'
          TabOrder = 0
          OnClick = btnValidClick
        end
        object btnSize: TButton
          Left = 86
          Top = 192
          Width = 72
          Height = 23
          Caption = 'Url File Size'
          TabOrder = 1
          OnClick = btnSizeClick
        end
        object btnProtocol: TButton
          Left = 8
          Top = 163
          Width = 72
          Height = 23
          Caption = 'Protocol'
          TabOrder = 2
          OnClick = btnProtocolClick
        end
        object EdtRes: TEdit
          Left = 8
          Top = 251
          Width = 150
          Height = 21
          TabOrder = 3
        end
        object btnCharSet: TButton
          Left = 8
          Top = 221
          Width = 72
          Height = 23
          Caption = 'Charset'
          TabOrder = 4
          OnClick = btnCharSetClick
        end
        object btnEntity: TButton
          Left = 86
          Top = 221
          Width = 72
          Height = 23
          Caption = 'EntityTag'
          TabOrder = 5
          OnClick = btnEntityClick
        end
        object btnServer: TButton
          Left = 86
          Top = 134
          Width = 72
          Height = 23
          Caption = 'Server'
          TabOrder = 6
          OnClick = btnServerClick
        end
        object btnStatusCode: TButton
          Left = 8
          Top = 134
          Width = 72
          Height = 23
          Caption = 'Status Code'
          TabOrder = 7
          OnClick = btnStatusCodeClick
        end
        object btnModified: TButton
          Left = 8
          Top = 105
          Width = 72
          Height = 23
          Caption = 'Modified'
          TabOrder = 8
          OnClick = btnModifiedClick
        end
        object btnUrlDate: TButton
          Left = 86
          Top = 105
          Width = 72
          Height = 23
          Caption = 'Url Date'
          TabOrder = 9
          OnClick = btnUrlDateClick
        end
        object btnUrlType: TButton
          Left = 86
          Top = 163
          Width = 72
          Height = 23
          Caption = 'Url Type'
          TabOrder = 10
          OnClick = btnUrlTypeClick
        end
        object Memo1: TMemo
          Left = 164
          Top = 104
          Width = 365
          Height = 169
          ScrollBars = ssBoth
          TabOrder = 11
        end
        object btnDetails: TButton
          Left = 375
          Top = 73
          Width = 73
          Height = 23
          Caption = 'Details'
          TabOrder = 12
          OnClick = btnDetailsClick
        end
        object btnQueryAll: TButton
          Left = 454
          Top = 73
          Width = 75
          Height = 23
          Caption = 'Query All'
          TabOrder = 13
          OnClick = btnQueryAllClick
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Url Preview'
      ImageIndex = 3
      object Memo2: TMemo
        Left = 0
        Top = 41
        Width = 545
        Height = 256
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 545
        Height = 41
        Align = alTop
        TabOrder = 1
        object btnRead: TButton
          Left = 0
          Top = 10
          Width = 75
          Height = 25
          Caption = 'Read File'
          TabOrder = 0
          OnClick = btnReadClick
        end
        object btnCache: TButton
          Left = 81
          Top = 10
          Width = 75
          Height = 25
          Caption = 'Is Url Cached'
          TabOrder = 1
          OnClick = btnCacheClick
        end
        object edtCache: TEdit
          Left = 162
          Top = 14
          Width = 121
          Height = 21
          TabOrder = 2
        end
      end
    end
  end
end
