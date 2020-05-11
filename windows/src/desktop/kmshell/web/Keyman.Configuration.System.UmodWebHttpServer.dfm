object modWebHttpServer: TmodWebHttpServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 150
  Width = 215
  object http: TIdHTTPServer
    Bindings = <>
    DefaultPort = 0
    KeepAlive = True
    OnCommandGet = httpCommandGet
    Left = 88
    Top = 56
  end
end
