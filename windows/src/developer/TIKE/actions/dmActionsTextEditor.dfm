object modActionsTextEditor: TmodActionsTextEditor
  OldCreateOrder = False
  Height = 478
  Width = 705
  object actionsTextEditor: TActionList
    Images = frmKeymanDeveloper.lstImages
    Left = 84
    Top = 116
    object actTextEditor_ShowCharacter: TAction
      Caption = 'S&how Character'
      Hint = 'Show Character|Display selected character in character map'
      ShortCut = 24659
      OnExecute = actTextEditor_ShowCharacterExecute
      OnUpdate = actTextEditor_ShowCharacterUpdate
    end
    object actTextEditor_ConvertToCharacters: TAction
      Caption = '&Convert to Characters'
      Hint = 
        'Convert to Characters|Convert the selected text to Unicode value' +
        's and vice versa'
      ShortCut = 24661
      OnExecute = actTextEditor_ConvertToCharactersExecute
      OnUpdate = actTextEditor_ConvertToCharactersUpdate
    end
    object actTextEditor_ReformatXML: TAction
      Caption = 'Reformat &XML Document'
      ImageIndex = 24
      OnExecute = actTextEditor_ReformatXMLExecute
      OnUpdate = actTextEditor_ReformatXMLUpdate
    end
  end
  object mnuTextEditor: TPopupMenu
    Left = 336
    Top = 224
    object ShowCharacter1: TMenuItem
      Action = actTextEditor_ShowCharacter
    end
    object ConverttoCharacters1: TMenuItem
      Action = actTextEditor_ConvertToCharacters
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Undo1: TMenuItem
      Action = modActionsMain.actEditUndo
    end
    object Redo1: TMenuItem
      Action = modActionsMain.actEditRedo
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Action = modActionsMain.actEditCut
    end
    object Copy1: TMenuItem
      Action = modActionsMain.actEditCopy
    end
    object Paste1: TMenuItem
      Action = modActionsMain.actEditPaste
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Action = modActionsMain.actEditSelectAll
    end
  end
end
