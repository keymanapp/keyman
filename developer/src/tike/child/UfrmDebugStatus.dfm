inherited frmDebugStatus: TfrmDebugStatus
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Debug Status'
  ClientHeight = 281
  ClientWidth = 418
  FormStyle = fsStayOnTop
  ExplicitWidth = 434
  ExplicitHeight = 320
  PixelsPerInch = 96
  TextHeight = 13
  object pagesDebug: TPageControl
    Left = 0
    Top = 0
    Width = 418
    Height = 281
    ActivePage = tabDebugKey
    Align = alClient
    TabOrder = 0
    object tabDebugKey: TTabSheet
      Caption = 'State'
      ImageIndex = 4
    end
    object tabDebugStores: TTabSheet
      Caption = 'Elements'
    end
    object tabDebugCallStack: TTabSheet
      Caption = 'Call stack'
      ImageIndex = 1
    end
    object tabDebugDeadkeys: TTabSheet
      Caption = 'Deadkeys'
      ImageIndex = 2
    end
    object tabDebugRegressionTesting: TTabSheet
      Caption = 'Regression testing'
      ImageIndex = 3
    end
    object tabDebugOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 6
    end
    object tabDebugPlatform: TTabSheet
      Caption = 'Platform'
      ImageIndex = 7
    end
    object tabDebugEvents: TTabSheet
      Caption = 'Events'
      ImageIndex = 5
    end
  end
end
