inherited frmHelp: TfrmHelp
  Action = actHelpContextRefresh
  Caption = 'Help'
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ActionList1: TActionList
    Left = 244
    Top = 180
    object actHelpContextRefresh: TAction
      Caption = 'actHelpContextRefresh'
      OnUpdate = actHelpContextRefreshUpdate
    end
  end
end
