object Form1: TForm1
  Left = 533
  Top = 366
  Width = 423
  Height = 281
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    415
    247)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 44
    Height = 13
    Caption = 'FileName'
  end
  object Label_Owner: TLabel
    Left = 32
    Top = 64
    Width = 97
    Height = 13
    Caption = 'Current owner : (null)'
  end
  object JvPoweredByJVCL1: TJvPoweredByJVCL
    Left = 184
    Top = 208
    URL = 'http://homepages.borland.com/jedi/jvcl/PoweredByJVCL.htm'
  end
  object JvFilenameEdit1: TJvFilenameEdit
    Left = 32
    Top = 34
    Width = 353
    Height = 21
    TabOrder = 0
    Text = 'JvFilenameEdit1'
    OnChange = JvFilenameEdit1Change
  end
  object Button_ShowSecurityDialog: TButton
    Left = 248
    Top = 64
    Width = 139
    Height = 25
    Caption = 'Show security properties'
    Enabled = False
    TabOrder = 1
    OnClick = Button_ShowSecurityDialogClick
  end
  object Button_ChangeOwner: TButton
    Left = 32
    Top = 88
    Width = 105
    Height = 25
    Caption = 'Change owner'
    Enabled = False
    TabOrder = 2
    OnClick = Button_ChangeOwnerClick
  end
  object Memo: TMemo
    Left = 32
    Top = 120
    Width = 361
    Height = 81
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'Memo')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object JvObjectPickerDialog_2: TJvObjectPickerDialog
    Options = [opSkipTargetComputerDCCheck]
    Scopes = <
      item
        DownLevelFilter = [dlUsers, dlWorld, dlAuthenticatedUser, dlBatch, dlService, dlSystem, dlAllWellKnownSids, dlLocalService, dlNetworkService]
        ScopeTypes = [stTargetComputer, stUpLevelJoinedDomain, stDownLevelJoinedDomain]
        ScopeFlags = [sfProviderWinNT, sfProviderLDAP, sfProviderGC, sfDownLevelBuiltInPath]
        UpLevelFilterBoth = [ulIncludeAdvancedView]
        UpLevelFilterMixed = [ulUsers]
      end>
    TargetComputer = 'chris'
    Left = 264
    Top = 24
  end
  object JvObjectPickerDialog: TJvObjectPickerDialog
    Options = [opSkipTargetComputerDCCheck]
    Scopes = <
      item
        DownLevelFilter = [dlUsers, dlLocalGroups, dlGlobalGroups, dlComputers, dlWorld, dlAuthenticatedUser, dlAnonymous, dlBatch, dlCreatorOwner, dlCreatorGroup, dlDialUp, dlInteractive, dlNetwork, dlService, dlSystem, dlExcludeBuiltinGroups, dlTerminalServer, dlAllWellKnownSids, dlLocalService, dlNetworkService, dlRemoteLogon]
        ScopeTypes = [stTargetComputer, stUpLevelJoinedDomain, stDownLevelJoinedDomain, stEnterpriseDomain, stGlobalCatalog, stExternalUpLevelDomain, stExternalDownLevelDomain, stUserEnteredUpLevelScope, stUserEnteredDownLevelScope]
        ScopeFlags = [sfStartingScope, sfProviderWinNT]
        UpLevelFilterBoth = [ulIncludeAdvancedView, ulUsers, ulBuiltInGroups, ulWellKnownPrincipals, ulUniversalDistributionListGroups, ulUniversalSecurityGroups, ulGlobalDistributionListGroups, ulGlobalSecurityGroups, ulDomainLocalDistributionListGroups, ulDomainLocalSecurityGroups, ulContacts, ulComputers]
        UpLevelFilterNative = [ulIncludeAdvancedView, ulUsers, ulBuiltInGroups, ulWellKnownPrincipals, ulUniversalDistributionListGroups, ulUniversalSecurityGroups, ulGlobalDistributionListGroups, ulGlobalSecurityGroups, ulDomainLocalDistributionListGroups, ulDomainLocalSecurityGroups, ulContacts, ulComputers]
        UpLevelFilterMixed = [ulIncludeAdvancedView, ulUsers, ulBuiltInGroups, ulWellKnownPrincipals, ulUniversalDistributionListGroups, ulUniversalSecurityGroups, ulGlobalDistributionListGroups, ulGlobalSecurityGroups, ulDomainLocalDistributionListGroups, ulDomainLocalSecurityGroups, ulContacts, ulComputers]
      end>
    Left = 80
    Top = 24
  end
  object JvObjectPickerDialog_OK: TJvObjectPickerDialog
    Scopes = <
      item
        DownLevelFilter = [dlUsers, dlLocalGroups, dlGlobalGroups, dlComputers, dlWorld, dlAuthenticatedUser, dlBatch, dlService, dlSystem, dlAllWellKnownSids, dlLocalService, dlNetworkService]
        ScopeTypes = [stTargetComputer, stUpLevelJoinedDomain, stDownLevelJoinedDomain, stEnterpriseDomain, stGlobalCatalog, stExternalUpLevelDomain, stExternalDownLevelDomain, stUserEnteredUpLevelScope, stUserEnteredDownLevelScope]
        ScopeFlags = [sfProviderWinNT, sfProviderLDAP, sfProviderGC, sfDownLevelBuiltInPath]
        UpLevelFilterBoth = [ulIncludeAdvancedView, ulUsers, ulBuiltInGroups, ulWellKnownPrincipals, ulUniversalDistributionListGroups, ulUniversalSecurityGroups, ulGlobalDistributionListGroups, ulGlobalSecurityGroups, ulDomainLocalDistributionListGroups, ulDomainLocalSecurityGroups, ulContacts, ulComputers]
        UpLevelFilterMixed = [ulUsers]
      end>
    Left = 312
    Top = 24
  end
end
