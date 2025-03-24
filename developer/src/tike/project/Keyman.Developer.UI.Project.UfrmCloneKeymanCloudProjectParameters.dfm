inherited frmCloneKeymanCloudProjectParameters: TfrmCloneKeymanCloudProjectParameters
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Clone Project from Keyman Cloud'
  ClientHeight = 506
  ClientWidth = 840
  Position = poScreenCenter
  ExplicitWidth = 856
  ExplicitHeight = 545
  PixelsPerInch = 96
  TextHeight = 13
  object cmdOK: TButton
    Left = 680
    Top = 473
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 759
    Top = 473
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object panWebHost: TPanel
    Left = 0
    Top = 0
    Width = 842
    Height = 334
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
  end
  object gbNewProjectDetails: TGroupBox
    Left = 8
    Top = 340
    Width = 823
    Height = 129
    Anchors = [akLeft, akRight, akBottom]
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
      Width = 669
      Height = 21
      TabStop = False
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
      OnChange = editKeyboardIDChange
    end
    object cmdBrowse: TButton
      Left = 744
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
      Width = 590
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
end
