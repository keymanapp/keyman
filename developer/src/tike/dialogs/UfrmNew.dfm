inherited frmNew: TfrmNew
  Left = 294
  Top = 305
  HelpContext = 1201
  ActiveControl = lvItems
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New'
  ClientHeight = 337
  ClientWidth = 365
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  ExplicitWidth = 371
  ExplicitHeight = 366
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 12
    Top = 251
    Width = 50
    Height = 13
    Caption = '&File Name:'
    FocusControl = editFileName
  end
  object lblPath: TLabel
    Left = 12
    Top = 224
    Width = 25
    Height = 13
    Caption = '&Path:'
    FocusControl = editPath
  end
  object lvItems: TListView
    Left = 12
    Top = 12
    Width = 341
    Height = 205
    Columns = <
      item
        Caption = 'File type'
        Width = 120
      end
      item
        Caption = 'Source'
      end
      item
        Caption = 'Output'
        Width = 75
      end>
    FullDrag = True
    HideSelection = False
    Items.ItemData = {
      05B20100000700000000000000FFFFFFFFFFFFFFFF02000000FFFFFFFF000000
      00084B006500790062006F00610072006400042E006B006D006E00985BF92504
      2E006B006D007800C85DF92501000000FFFFFFFFFFFFFFFF02000000FFFFFFFF
      00000000075000610063006B00610067006500042E006B0070007300A060F925
      0A2E006B006D0070002C0020002E006500780065001061F92504000000FFFFFF
      FFFFFFFFFF01000000FFFFFFFF00000000045400650078007400042E00740078
      0074002064F92506000000FFFFFFFFFFFFFFFF01000000FFFFFFFF0000000004
      480054004D004C00052E00680074006D006C003865F92506000000FFFFFFFFFF
      FFFFFF01000000FFFFFFFF0000000003480054004D00042E00680074006D00A8
      65F92505000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000000358004D00
      4C00042E0078006D006C001866F92500000000FFFFFFFFFFFFFFFF02000000FF
      FFFFFF00000000054D006F00640065006C00092E006D006F00640065006C002E
      0074007300085CF925092E006D006F00640065006C002E006A007300405CF925
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    LargeImages = ilLarge
    PopupMenu = mnuItems
    SmallImages = ilSmall
    TabOrder = 0
    OnDblClick = lvItemsDblClick
    OnSelectItem = lvItemsSelectItem
  end
  object cmdOK: TButton
    Left = 108
    Top = 300
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 187
    Top = 300
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object chkAddToProject: TCheckBox
    Left = 62
    Top = 275
    Width = 106
    Height = 21
    Caption = '&Add file to project'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object editFileName: TEdit
    Left = 64
    Top = 248
    Width = 205
    Height = 21
    TabOrder = 2
    OnChange = editFileNameChange
  end
  object cmdBrowse: TButton
    Left = 275
    Top = 248
    Width = 78
    Height = 21
    Caption = '&Browse...'
    TabOrder = 4
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 64
    Top = 221
    Width = 289
    Height = 21
    TabOrder = 1
    OnChange = editFileNameChange
  end
  object ilLarge: TImageList
    ColorDepth = cd32Bit
    BlendColor = clWindow
    BkColor = clWhite
    Height = 32
    Width = 32
    Left = 56
    Top = 292
  end
  object ilSmall: TImageList
    ColorDepth = cd32Bit
    BlendColor = clWindow
    BkColor = clWhite
    Left = 18
    Top = 292
  end
  object mnuItems: TPopupMenu
    Left = 372
    Top = 108
    object mnuShowIcons: TMenuItem
      Caption = 'Show &icons'
      OnClick = mnuShowIconsClick
    end
    object mnuShowDetails: TMenuItem
      Caption = 'Show &details'
      OnClick = mnuShowDetailsClick
    end
  end
  object dlgSave: TSaveDialog
    Title = 'Create New File'
    Left = 276
    Top = 272
  end
end
