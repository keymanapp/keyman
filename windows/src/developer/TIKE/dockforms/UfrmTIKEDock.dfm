inherited TIKEDockForm: TTIKEDockForm
  Caption = 'TIKEDockForm'
  ClientHeight = 247
  ClientWidth = 480
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  FormStyle = fsStayOnTop
  OnHide = FormHide
  ExplicitWidth = 496
  ExplicitHeight = 286
  PixelsPerInch = 96
  TextHeight = 13
  object dockClient: TJvDockClient
    DirectDrag = False
    DockStyle = frmKeymanDeveloper.JvDockVSNetStyle1
    OnFormHide = dockClientFormHide
    Left = 152
    Top = 48
  end
end
