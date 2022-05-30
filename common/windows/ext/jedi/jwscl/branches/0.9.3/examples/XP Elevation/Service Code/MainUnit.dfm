object XPService: TXPService
  OldCreateOrder = False
  DisplayName = 'XP Elevation Service'
  StartType = stManual
  OnExecute = ServiceExecute
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
