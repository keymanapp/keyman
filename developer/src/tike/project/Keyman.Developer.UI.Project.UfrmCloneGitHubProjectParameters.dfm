inherited frmCloneGitHubProjectParameters: TfrmCloneGitHubProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Clone Project from GitHub'
  ClientHeight = 269
  ClientWidth = 601
  Position = poScreenCenter
  ExplicitWidth = 607
  ExplicitHeight = 298
  PixelsPerInch = 96
  TextHeight = 13
  object cmdOK: TButton
    Left = 441
    Top = 236
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = cmdOKClick
    ExplicitLeft = 449
    ExplicitTop = 218
  end
  object cmdCancel: TButton
    Left = 520
    Top = 236
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 528
    ExplicitTop = 218
  end
  object gbNewProjectDetails: TGroupBox
    Left = 9
    Top = 103
    Width = 584
    Height = 129
    Caption = 'New Project Details'
    TabOrder = 1
    object lblFileName: TLabel
      Left = 16
      Top = 24
      Width = 76
      Height = 13
      Caption = '&New project ID:'
      FocusControl = editKeyboardID
    end
    object lblProjectFilename: TLabel
      Left = 16
      Top = 102
      Width = 101
      Height = 13
      Caption = 'New project &filename'
      FocusControl = editProjectFilename
    end
    object lblPath: TLabel
      Left = 16
      Top = 75
      Width = 83
      Height = 13
      Caption = 'Destination &path:'
      FocusControl = editPath
    end
    object editKeyboardID: TEdit
      Left = 148
      Top = 21
      Width = 205
      Height = 21
      TabOrder = 0
      OnChange = editKeyboardIDChange
    end
    object editProjectFilename: TEdit
      Left = 148
      Top = 99
      Width = 429
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
      OnChange = editKeyboardIDChange
    end
    object cmdBrowse: TButton
      Left = 504
      Top = 72
      Width = 73
      Height = 21
      Caption = '&Browse...'
      TabOrder = 3
      OnClick = cmdBrowseClick
    end
    object editPath: TEdit
      Left = 148
      Top = 72
      Width = 350
      Height = 21
      TabOrder = 2
      OnChange = editPathChange
    end
    object chkRelocateExternal: TCheckBox
      Left = 148
      Top = 48
      Width = 257
      Height = 17
      Caption = 'Relocate &external files into new project folder'
      TabOrder = 1
    end
  end
  object gbSourceProjectDetails: TGroupBox
    Left = 8
    Top = 8
    Width = 585
    Height = 89
    Caption = 'Source Project'
    TabOrder = 0
    object Label1: TLabel
      Left = 17
      Top = 19
      Width = 58
      Height = 13
      Caption = 'GitHub URL:'
      FocusControl = editGitHubURL
    end
    object Label2: TLabel
      Left = 17
      Top = 43
      Width = 512
      Height = 13
      Caption = 
        'A URL to a GitHub repository (or subfolder within a repository) ' +
        'that matches the Keyman project file layout'
    end
    object Label3: TLabel
      Left = 17
      Top = 62
      Width = 420
      Height = 13
      Caption = 
        'e.g. https://github.com/keyman-keyboards/khmer_angkor/tree/main/' +
        'khmer_angkor.kpj'
    end
    object editGitHubURL: TEdit
      Left = 149
      Top = 16
      Width = 428
      Height = 21
      TabOrder = 0
      OnChange = editPathChange
    end
  end
end
