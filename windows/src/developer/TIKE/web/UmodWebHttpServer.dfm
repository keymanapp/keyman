object modWebHttpServer: TmodWebHttpServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object http: TIdHTTPServer
    Bindings = <>
    DefaultPort = 8008
    OnCommandGet = httpCommandGet
    Left = 88
    Top = 56
  end
end
