object modActionsModelEditor: TmodActionsModelEditor
  OldCreateOrder = False
  Height = 150
  Width = 215
  object actionsModelEditor: TActionList
    Images = frmKeymanDeveloper.lstImages
    Left = 60
    Top = 24
    object actModelCompile: TAction
      Category = 'Lexical Model'
      Caption = '&Compile Model'
      ImageIndex = 37
      ShortCut = 118
      OnExecute = actModelCompileExecute
      OnUpdate = actModelCompileUpdate
    end
    object actModelIncludeDebugInformation: TAction
      Category = 'Lexical Model'
      Caption = 'Include &Debug Information'
      OnExecute = actModelIncludeDebugInformationExecute
    end
    object actModelTest: TAction
      Category = 'Lexical Model'
      Caption = 'Test &Lexical Model'
      OnExecute = actModelTestExecute
      OnUpdate = actModelTestUpdate
    end
  end
end
