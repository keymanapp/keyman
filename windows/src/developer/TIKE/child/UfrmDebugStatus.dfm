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
    ActivePage = tabDebugRegressionTesting
    Align = alClient
    TabOrder = 0
    object tabDebugKey: TTabSheet
      Caption = 'State'
      ImageIndex = 4
    end
    object tabDebugStores: TTabSheet
      Caption = 'Elements'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 418
      ExplicitHeight = 258
    end
    object tabDebugCallStack: TTabSheet
      Caption = 'Call stack'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 418
      ExplicitHeight = 258
    end
    object tabDebugDeadkeys: TTabSheet
      Caption = 'Deadkeys'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 418
      ExplicitHeight = 258
    end
    object tabDebugRegressionTesting: TTabSheet
      Caption = 'Regression testing'
      ImageIndex = 3
    end
  end
end
