inherited frmCloneLocalProjectParameters: TfrmCloneLocalProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Clone Project'
  ClientHeight = 231
  ClientWidth = 721
  Position = poScreenCenter
  ExplicitWidth = 727
  ExplicitHeight = 260
  PixelsPerInch = 96
  TextHeight = 13
  object cmdOK: TButton
    Left = 561
    Top = 198
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 640
    Top = 198
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object gbNewProjectDetails: TGroupBox
    Left = 8
    Top = 63
    Width = 705
    Height = 129
    Caption = 'New Project Details'
    TabOrder = 1
    object lblFileName: TLabel
      Left = 12
      Top = 22
      Width = 76
      Height = 13
      Caption = '&New project ID:'
      FocusControl = editKeyboardID
    end
    object lblProjectFilename: TLabel
      Left = 12
      Top = 100
      Width = 101
      Height = 13
      Caption = 'New project &filename'
      FocusControl = editProjectFilename
    end
    object lblPath: TLabel
      Left = 12
      Top = 73
      Width = 83
      Height = 13
      Caption = 'Destination &path:'
      FocusControl = editPath
    end
    object editKeyboardID: TEdit
      Left = 144
      Top = 19
      Width = 205
      Height = 21
      TabOrder = 0
      OnChange = editKeyboardIDChange
    end
    object editProjectFilename: TEdit
      Left = 144
      Top = 97
      Width = 553
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
      OnChange = editKeyboardIDChange
    end
    object cmdBrowse: TButton
      Left = 624
      Top = 70
      Width = 73
      Height = 21
      Caption = '&Browse...'
      TabOrder = 3
      OnClick = cmdBrowseClick
    end
    object editPath: TEdit
      Left = 144
      Top = 70
      Width = 474
      Height = 21
      TabOrder = 2
      OnChange = editPathChange
    end
    object chkRelocateExternal: TCheckBox
      Left = 144
      Top = 46
      Width = 257
      Height = 17
      Caption = 'Relocate &external files into new project folder'
      TabOrder = 1
    end
  end
  object gbSourceProjectDetails: TGroupBox
    Left = 8
    Top = 8
    Width = 705
    Height = 49
    Caption = 'Source Project'
    TabOrder = 0
    object lblKeyboardName: TLabel
      Left = 12
      Top = 19
      Width = 117
      Height = 13
      Caption = '&Source project filename:'
      FocusControl = editSourceProjectFilename
    end
    object editSourceProjectFilename: TEdit
      Left = 144
      Top = 16
      Width = 474
      Height = 21
      TabOrder = 0
      OnChange = editSourceProjectFilenameChange
    end
    object cmdBrowseSourceProject: TButton
      Left = 624
      Top = 16
      Width = 73
      Height = 21
      Caption = 'B&rowse...'
      TabOrder = 1
      OnClick = cmdBrowseSourceProjectClick
    end
  end
  object dlgBrowseSourceProject: TFileOpenDialog
    DefaultExtension = '.kpj'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Keyman Project Files (*.kpj)'
        FileMask = '*.kpj'
      end>
    OkButtonLabel = 'Select'
    Options = [fdoPathMustExist, fdoFileMustExist, fdoNoTestFileCreate]
    Title = 'Select source project'
    Left = 584
    Top = 16
  end
end
