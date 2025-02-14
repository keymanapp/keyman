inherited frmCloneGitHubProjectParameters: TfrmCloneGitHubProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Clone Project from GitHub'
  ClientHeight = 297
  ClientWidth = 738
  Position = poScreenCenter
  ExplicitWidth = 744
  ExplicitHeight = 326
  PixelsPerInch = 96
  TextHeight = 13
  object cmdOK: TButton
    Left = 578
    Top = 264
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = cmdOKClick
    ExplicitLeft = 441
    ExplicitTop = 236
  end
  object cmdCancel: TButton
    Left = 657
    Top = 264
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 520
    ExplicitTop = 236
  end
  object gbNewProjectDetails: TGroupBox
    Left = 9
    Top = 103
    Width = 720
    Height = 154
    Caption = 'New Project Details'
    TabOrder = 1
    object lblFileName: TLabel
      Left = 16
      Top = 24
      Width = 76
      Height = 13
      Caption = '&New project ID:'
      FocusControl = editProjectID
    end
    object lblProjectFilename: TLabel
      Left = 15
      Top = 126
      Width = 101
      Height = 13
      Caption = 'New project &filename'
      FocusControl = editProjectFilename
    end
    object lblPath: TLabel
      Left = 15
      Top = 99
      Width = 83
      Height = 13
      Caption = 'Destination &path:'
      FocusControl = editPath
    end
    object lblSourceProjectLexicalModel: TLabel
      Left = 15
      Top = 48
      Width = 531
      Height = 13
      Caption = 
        'The source project appears to be a lexical model. The new projec' +
        't ID must use the author.bcp47.uniq pattern.'
    end
    object lblSourceProjectKeyboard: TLabel
      Left = 16
      Top = 48
      Width = 682
      Height = 13
      Caption = 
        'The source project appears to be a keyboard project. The new pro' +
        'ject ID must be a valid keyboard id pattern (alphanumeric and un' +
        'derscore).'
    end
    object editProjectID: TEdit
      Left = 148
      Top = 21
      Width = 205
      Height = 21
      TabOrder = 0
      OnChange = editProjectIDChange
    end
    object editProjectFilename: TEdit
      Left = 147
      Top = 123
      Width = 564
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
      OnChange = editProjectIDChange
    end
    object cmdBrowse: TButton
      Left = 644
      Top = 96
      Width = 67
      Height = 21
      Caption = '&Browse...'
      TabOrder = 3
      OnClick = cmdBrowseClick
    end
    object editPath: TEdit
      Left = 147
      Top = 96
      Width = 491
      Height = 21
      TabOrder = 2
      OnChange = editGitHubURLChange
    end
    object chkRelocateExternal: TCheckBox
      Left = 147
      Top = 72
      Width = 257
      Height = 17
      Caption = 'Relocate &external files into new project folder'
      TabOrder = 1
    end
  end
  object gbSourceProjectDetails: TGroupBox
    Left = 8
    Top = 8
    Width = 721
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
      Width = 563
      Height = 21
      TabOrder = 0
      OnChange = editGitHubURLChange
    end
  end
end
