object frmSettingsManager: TfrmSettingsManager
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Keyman Settings Manager'
  ClientHeight = 447
  ClientWidth = 704
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 720
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    704
    447)
  PixelsPerInch = 96
  TextHeight = 13
  object imgElevate: TImage
    Left = 346
    Top = 363
    Width = 16
    Height = 16
    Anchors = [akLeft, akBottom]
    AutoSize = True
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
      001008060000001FF3FF61000000017352474200AECE1CE90000000467414D41
      0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000002
      AF4944415478DA85935D48935118C7FF7BF7A16E7B5B534B25D29BC2E8BB04FB
      1033BF924CA61B2D234ACAD845ED228C82098157411174511745DE4451C10C22
      96330D45FB508B29A5929898CEE91CDB9A9BF375EFFB6EEFD631BA68D9ECC081
      C339FFE7F79CE77F9E234282118BC544ADAD1D853CCF85753A4D5F229DE85F9B
      56AB759D54A67A381D4CAA2CDF3E80F1A9ACE70B8B8241ABD5FA5705D86CB674
      A552591760228D6E4E9E3EE2F0C354FE024C90C25747EE9CCBABBA3E31F9ED49
      4343837F0560C2EEEA72FAD8A2002B506A9502BE450EA3D3F3B85ADC0E840240
      98876F490D379311199B5535D768F517E3003DB6EFBEC13956AD9088209550F8
      116461F704714747CA67098067012E42602C5E3BF2DF1FADA92F8C03B4F68CF8
      FB673815CB72BFB207423C9C7E161FCEBC0216DD249027B78891194507AFEFAD
      A83114C4015E767E9EEB996233BD01065F48EDE39E2544893E74E13139259905
      099932B2A6D0E6AFEEAED49D2F8E07BCF9647B3B13C91BB67B3138E58340A293
      A5C4BCD36688C42250E2142296220631BADDD5CF34DAE3A7E23DE86E7B50B0A9
      CB206615003346EAF520CCF1C8BC5B079A4EC17A5A0EB954868DB40C2776738D
      1A4DD58D3880D5DA7E646FCE507B866C9E18465E29348DF01283ECE6B3484BA5
      9146203CB141BB55118BB97AB7994CA6D138C072E77D7CD732B02FAB7F0F2214
      B9810B61C683DC47E7909E4623492C455254844B074556CDB1B2AAE590158D64
      B1580EEFCC99EDCC4E9EA4C0F911093AB0ABC58035B40202C95E9FB7868BBA87
      F38D46C350C2566E6BB3DCDA9FEBB9B2366A87C038516639497C97A060831CC5
      3941534949C9CD55FF42535393A4E8D081A73B36B3FA54991BD7FA4A911965B0
      45E9B95F51516A2492E87F3F93D96C16D3B4EAB29A168CCE8514211CF4DEAEAD
      D5DFFB3B3821E08F41FDD60889043F01326A2C20F97EFC660000000049454E44
      AE426082}
    Visible = False
  end
  object lblDefault: TLabel
    Left = 8
    Top = 391
    Width = 39
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Default:'
    ExplicitTop = 461
  end
  object imgElevateTSFApplications: TImage
    Left = 146
    Top = 420
    Width = 16
    Height = 16
    Anchors = [akLeft, akBottom]
    AutoSize = True
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
      001008060000001FF3FF61000000017352474200AECE1CE90000000467414D41
      0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000002
      AF4944415478DA85935D48935118C7FF7BF7A16E7B5B534B25D29BC2E8BB04FB
      1033BF924CA61B2D234ACAD845ED228C82098157411174511745DE4451C10C22
      96330D45FB508B29A5929898CEE91CDB9A9BF375EFFB6EEFD631BA68D9ECC081
      C339FFE7F79CE77F9E234282118BC544ADAD1D853CCF85753A4D5F229DE85F9B
      56AB759D54A67A381D4CAA2CDF3E80F1A9ACE70B8B8241ABD5FA5705D86CB674
      A552591760228D6E4E9E3EE2F0C354FE024C90C25747EE9CCBABBA3E31F9ED49
      4343837F0560C2EEEA72FAD8A2002B506A9502BE450EA3D3F3B85ADC0E840240
      98876F490D379311199B5535D768F517E3003DB6EFBEC13956AD9088209550F8
      116461F704714747CA67098067012E42602C5E3BF2DF1FADA92F8C03B4F68CF8
      FB673815CB72BFB207423C9C7E161FCEBC0216DD249027B78891194507AFEFAD
      A83114C4015E767E9EEB996233BD01065F48EDE39E2544893E74E13139259905
      099932B2A6D0E6AFEEAED49D2F8E07BCF9647B3B13C91BB67B3138E58340A293
      A5C4BCD36688C42250E2142296220631BADDD5CF34DAE3A7E23DE86E7B50B0A9
      CB206615003346EAF520CCF1C8BC5B079A4EC17A5A0EB954868DB40C2776738D
      1A4DD58D3880D5DA7E646FCE507B866C9E18465E29348DF01283ECE6B3484BA5
      9146203CB141BB55118BB97AB7994CA6D138C072E77D7CD732B02FAB7F0F2214
      B9810B61C683DC47E7909E4623492C455254844B074556CDB1B2AAE590158D64
      B1580EEFCC99EDCC4E9EA4C0F911093AB0ABC58035B40202C95E9FB7868BBA87
      F38D46C350C2566E6BB3DCDA9FEBB9B2366A87C038516639497C97A060831CC5
      3941534949C9CD55FF42535393A4E8D081A73B36B3FA54991BD7FA4A911965B0
      45E9B95F51516A2492E87F3F93D96C16D3B4EAB29A168CCE8514211CF4DEAEAD
      D5DFFB3B3821E08F41FDD60889043F01326A2C20F97EFC660000000049454E44
      AE426082}
    ExplicitTop = 490
  end
  object gridDebugOption: TStringGrid
    Left = 8
    Top = 8
    Width = 688
    Height = 243
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 16
    DefaultDrawing = False
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 0
    OnClick = gridDebugOptionClick
    OnDrawCell = gridDebugOptionDrawCell
    OnSetEditText = gridDebugOptionSetEditText
  end
  object memoDescription: TMemo
    Left = 8
    Top = 266
    Width = 336
    Height = 89
    Anchors = [akLeft, akBottom]
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object cmdApply: TButton
    Left = 528
    Top = 414
    Width = 87
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Apply'
    TabOrder = 2
    OnClick = cmdApplyClick
  end
  object cmdClearAll: TButton
    Left = 561
    Top = 383
    Width = 135
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Reset all to default'
    TabOrder = 3
    OnClick = cmdClearAllClick
  end
  object cmdClose: TButton
    Left = 621
    Top = 414
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 4
    OnClick = cmdCloseClick
  end
  object editRegKey: TEdit
    Left = 8
    Top = 361
    Width = 336
    Height = 21
    Anchors = [akLeft, akBottom]
    ParentColor = True
    ReadOnly = True
    TabOrder = 5
  end
  object editDefault: TEdit
    Left = 53
    Top = 388
    Width = 210
    Height = 21
    Anchors = [akLeft, akBottom]
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
  end
  object cmdReset: TButton
    Left = 269
    Top = 388
    Width = 75
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = '&Reset'
    TabOrder = 7
    OnClick = cmdResetClick
  end
  object cmdExport: TButton
    Left = 561
    Top = 321
    Width = 135
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Export...'
    TabOrder = 8
    OnClick = cmdExportClick
  end
  object chkShowDevelopmentSettings: TCheckBox
    Left = 360
    Top = 422
    Width = 167
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Show Development Settings'
    TabOrder = 9
    OnClick = chkShowDevelopmentSettingsClick
  end
  object cmdImport: TButton
    Left = 561
    Top = 352
    Width = 135
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Import...'
    TabOrder = 10
    OnClick = cmdImportClick
  end
  object cmdTSFApplicationSettings: TButton
    Left = 8
    Top = 415
    Width = 135
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add TSF Override...'
    TabOrder = 11
    OnClick = cmdTSFApplicationSettingsClick
  end
  object dlgImport: TOpenDialog
    DefaultExt = 'json'
    Filter = 'Keyman Settings Files (*.json)|*.json|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Import Keyman Settings from File'
    Left = 424
    Top = 152
  end
  object dlgExport: TSaveDialog
    DefaultExt = 'json'
    Filter = 'Keyman Settings Files (*.json)|*.json|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Export Keyman Settings to File'
    Left = 368
    Top = 152
  end
end
