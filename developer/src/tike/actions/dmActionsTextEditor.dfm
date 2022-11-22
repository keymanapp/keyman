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
end
