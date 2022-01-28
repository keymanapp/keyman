object modWebHttpServer: TmodWebHttpServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object http: TIdHTTPServer
    Bindings = <
      item
        IP = '127.0.0.1'
        Port = 0
      end>
    DefaultPort = 0
    KeepAlive = True
    OnCommandGet = httpCommandGet
    Left = 88
    Top = 56
  end
end
