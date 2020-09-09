object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 660
  ClientWidth = 1026
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 0
    Top = 0
    Width = 1026
    Height = 660
    ActivePage = tabRegisterProfile
    Align = alClient
    TabOrder = 0
    OnChange = pagesChange
    object tabInputMethodStatus: TTabSheet
      Caption = 'Input Method Status'
      object Splitter1: TSplitter
        Left = 0
        Top = 225
        Width = 1018
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 399
        ExplicitWidth = 192
      end
      object grid: TStringGrid
        Left = 0
        Top = 0
        Width = 1018
        Height = 225
        Align = alTop
        ColCount = 8
        DefaultRowHeight = 16
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
        ScrollBars = ssVertical
        TabOrder = 0
        OnClick = gridClick
      end
      object memoDetail: TMemo
        Left = 0
        Top = 228
        Width = 1018
        Height = 363
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object Panel1: TPanel
        Left = 0
        Top = 591
        Width = 1018
        Height = 41
        Align = alBottom
        Caption = 'Panel1'
        TabOrder = 2
        DesignSize = (
          1018
          41)
        object cmdRefresh: TButton
          Left = 3
          Top = 3
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Refresh'
          TabOrder = 0
          OnClick = cmdRefreshClick
        end
        object cmdExit: TButton
          Left = 927
          Top = 3
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'E&xit'
          TabOrder = 1
          OnClick = cmdExitClick
        end
        object Button4: TButton
          Left = 566
          Top = 3
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'EnumTSF'
          TabOrder = 2
          OnClick = Button3Click
        end
        object Button3: TButton
          Left = 478
          Top = 3
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'EnumCategories'
          TabOrder = 3
          OnClick = Button3Click
        end
        object Button2: TButton
          Left = 341
          Top = 3
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Button2'
          TabOrder = 4
          OnClick = Button2Click
        end
        object Button1: TButton
          Left = 260
          Top = 3
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Show &Default'
          TabOrder = 5
          OnClick = Button1Click
        end
        object cmdLoadKeyboardLayout: TButton
          Left = 179
          Top = 3
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Load 0409'
          TabOrder = 6
          OnClick = cmdLoadKeyboardLayoutClick
        end
        object cmdUnload: TButton
          Left = 84
          Top = 3
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Unload'
          TabOrder = 7
          OnClick = cmdUnloadClick
        end
      end
    end
    object tabLoadKeyboardLayout: TTabSheet
      Caption = 'LoadKeyboardLayout'
      ImageIndex = 1
      object cmdLoadKeyboardLayout1: TButton
        Left = 16
        Top = 16
        Width = 122
        Height = 25
        Caption = 'LoadKeyboardLayout'
        TabOrder = 0
        OnClick = cmdLoadKeyboardLayout1Click
      end
      object editLoadKeyboardLayout_KLID: TEdit
        Left = 144
        Top = 18
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '00000448'
      end
      object cmdUnloadKeyboardLayout: TButton
        Left = 17
        Top = 189
        Width = 122
        Height = 25
        Caption = 'UnloadKeyboardLayout'
        TabOrder = 2
        OnClick = cmdUnloadKeyboardLayoutClick
      end
      object chkLoadKeyboardLayout_KLF_ACTIVATE: TCheckBox
        Left = 288
        Top = 20
        Width = 145
        Height = 17
        Caption = 'KLF_ACTIVATE'
        TabOrder = 3
      end
      object chkLoadKeyboardLayout_KLF_NOTELLSHELL: TCheckBox
        Tag = 128
        Left = 288
        Top = 43
        Width = 145
        Height = 17
        Caption = 'KLF_NOTELLSHELL'
        TabOrder = 4
      end
      object chkLoadKeyboardLayout_KLF_REORDER: TCheckBox
        Tag = 8
        Left = 288
        Top = 66
        Width = 145
        Height = 17
        Caption = 'KLF_REORDER'
        TabOrder = 5
      end
      object chkLoadKeyboardLayout_KLF_REPLACELANG: TCheckBox
        Tag = 16
        Left = 288
        Top = 89
        Width = 145
        Height = 17
        Caption = 'KLF_REPLACELANG'
        TabOrder = 6
      end
      object chkLoadKeyboardLayout_KLF_SUBSTITUTE_OK: TCheckBox
        Left = 288
        Top = 112
        Width = 145
        Height = 17
        Caption = 'KLF_SUBSTITUTE_OK'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object chkLoadKeyboardLayout_KLF_SETFORPROCESS: TCheckBox
        Tag = 256
        Left = 288
        Top = 135
        Width = 145
        Height = 17
        Caption = 'KLF_SETFORPROCESS'
        TabOrder = 8
      end
      object editUnloadKeyboardLayout_HKL: TEdit
        Left = 145
        Top = 191
        Width = 121
        Height = 21
        TabOrder = 9
      end
    end
    object tabRegisterProfile: TTabSheet
      Caption = 'RegisterProfile'
      ImageIndex = 2
      object Label1: TLabel
        Left = 288
        Top = 24
        Width = 38
        Height = 13
        Caption = 'LangID:'
      end
      object Label2: TLabel
        Left = 288
        Top = 51
        Width = 59
        Height = 13
        Caption = 'ProfileGUID:'
      end
      object Label3: TLabel
        Left = 288
        Top = 78
        Width = 27
        Height = 13
        Caption = 'Desc:'
      end
      object cmdITfInputProcessorProfileMgrRegisterProfile: TButton
        Left = 9
        Top = 15
        Width = 233
        Height = 25
        Caption = 'ITfInputProcessorProfileMgr::RegisterProfile'
        TabOrder = 0
        OnClick = cmdITfInputProcessorProfileMgrRegisterProfileClick
      end
      object ITfInputProcessorProfileMgrUnregisterProfile: TButton
        Left = 9
        Top = 238
        Width = 233
        Height = 25
        Caption = 'ITfInputProcessorProfileMgr::UnregisterProfile'
        TabOrder = 1
        OnClick = ITfInputProcessorProfileMgrUnregisterProfileClick
      end
      object chkRegisterProfile_EnabledByDefault: TCheckBox
        Left = 360
        Top = 102
        Width = 193
        Height = 17
        Caption = 'EnabledByDefault'
        TabOrder = 6
      end
      object chkRegisterProfile_TF_RP_HIDDENINSETTINGUI: TCheckBox
        Left = 360
        Top = 125
        Width = 193
        Height = 17
        Caption = 'TF_RP_HIDDENINSETTINGUI'
        TabOrder = 7
      end
      object chkRegisterProfile_TF_RP_LOCALPROCESS: TCheckBox
        Left = 360
        Top = 148
        Width = 193
        Height = 17
        Caption = 'TF_RP_LOCALPROCESS'
        TabOrder = 8
      end
      object chkRegisterProfile_TF_RP_LOCALTHREAD: TCheckBox
        Left = 360
        Top = 171
        Width = 193
        Height = 17
        Caption = 'TF_RP_LOCALTHREAD'
        TabOrder = 9
      end
      object editRegisterProfile_LangID: TEdit
        Left = 360
        Top = 21
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '040C'
      end
      object editRegisterProfile_ProfileGUID: TEdit
        Left = 360
        Top = 48
        Width = 233
        Height = 21
        TabOrder = 3
        Text = '{84494147-B8AE-4676-A8F7-4C29BADC22BD}'
      end
      object cmdRegisterProfile_NewGUID: TButton
        Left = 599
        Top = 46
        Width = 80
        Height = 25
        Caption = 'New &GUID'
        TabOrder = 4
        OnClick = cmdRegisterProfile_NewGUIDClick
      end
      object editRegisterProfile_Desc: TEdit
        Left = 360
        Top = 75
        Width = 121
        Height = 21
        TabOrder = 5
        Text = 'Example Input Method'
      end
      object chkUnregisterProfile_TF_URP_ALLPROFILES: TCheckBox
        Left = 360
        Top = 245
        Width = 193
        Height = 17
        Caption = 'TF_URP_ALLPROFILES'
        TabOrder = 10
      end
      object chkUnregisterProfile_TF_URP_LOCALPROCESS: TCheckBox
        Left = 360
        Top = 268
        Width = 193
        Height = 17
        Caption = 'TF_URP_LOCALPROCESS '
        TabOrder = 11
      end
      object chkUnregisterProfile_TF_URP_LOCALTHREAD: TCheckBox
        Left = 360
        Top = 291
        Width = 193
        Height = 17
        Caption = 'TF_URP_LOCALTHREAD '
        TabOrder = 12
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'ItfInputProcessorProfiles'
      ImageIndex = 3
      object cmdITfInputProcessorProfilesRegister: TButton
        Left = 3
        Top = 5
        Width = 233
        Height = 25
        Caption = 'ITfInputProcessorProfiles::Register'
        TabOrder = 0
      end
      object cmdITfInputProcessorProfilesAddLanguageProfile: TButton
        Left = 3
        Top = 36
        Width = 233
        Height = 25
        Caption = 'ITfInputProcessorProfiles::AddLanguageProfile'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
      object cmdITfInputProcessorProfilesEnableLanguageProfile: TButton
        Left = 3
        Top = 67
        Width = 233
        Height = 25
        Caption = 'ITfInputProcessorProfiles::EnableLanguageProfile'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'InstallLayoutOrTip'
      ImageIndex = 4
      object cmdInstallLayoutOrTip: TButton
        Left = 27
        Top = 32
        Width = 153
        Height = 25
        Caption = 'InstallLayoutOrTip'
        TabOrder = 0
        OnClick = cmdInstallLayoutOrTipClick
      end
      object cmdInstallLayoutOrTipUserReg: TButton
        Left = 27
        Top = 63
        Width = 153
        Height = 25
        Caption = 'InstallLayoutOrTipUserReg'
        TabOrder = 1
        OnClick = cmdInstallLayoutOrTipUserRegClick
      end
      object editInstallLayoutOrTip_psz: TEdit
        Left = 224
        Top = 34
        Width = 489
        Height = 21
        TabOrder = 2
        Text = 
          '040C:{FE0420F1-38D1-4B4C-96BF-E7E20A74CFB7}{84494147-B8AE-4676-A' +
          '8F7-4C29BADC22BD}'
      end
      object chkInstallLayoutOrTip_ILOT_UNINSTALL: TCheckBox
        Tag = 1
        Left = 224
        Top = 61
        Width = 257
        Height = 17
        Caption = 'ILOT_UNINSTALL '
        TabOrder = 3
      end
      object chkInstallLayoutOrTip_ILOT_DEFPROFILE: TCheckBox
        Tag = 2
        Left = 224
        Top = 84
        Width = 257
        Height = 17
        Caption = 'ILOT_DEFPROFILE '
        TabOrder = 4
      end
      object chkInstallLayoutOrTip_ILOT_DEFUSER4: TCheckBox
        Tag = 4
        Left = 224
        Top = 219
        Width = 257
        Height = 17
        Caption = 'ILOT_DEFUSER4 '
        TabOrder = 5
      end
      object chkInstallLayoutOrTip_ILOT_NOAPPLYTOCURRENTSESSION: TCheckBox
        Tag = 32
        Left = 224
        Top = 108
        Width = 257
        Height = 17
        Caption = 'ILOT_NOAPPLYTOCURRENTSESSION '
        TabOrder = 6
      end
      object chkInstallLayoutOrTip_ILOT_DISABLED: TCheckBox
        Tag = 128
        Left = 224
        Top = 154
        Width = 257
        Height = 17
        Caption = 'ILOT_DISABLED'
        TabOrder = 7
      end
      object chkInstallLayoutOrTip_ILOT_CLEANINSTALL: TCheckBox
        Tag = 64
        Left = 224
        Top = 131
        Width = 257
        Height = 17
        Caption = 'ILOT_CLEANINSTALL '
        TabOrder = 8
      end
    end
  end
end
