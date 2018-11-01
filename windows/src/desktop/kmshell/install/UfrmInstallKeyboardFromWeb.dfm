inherited frmInstallKeyboardFromWeb: TfrmInstallKeyboardFromWeb
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsDialog
  Caption = 'Download Keyboard From keyman.com'
  ClientHeight = 400
  ClientWidth = 740
  Position = poScreenCenter
  ExplicitWidth = 746
  ExplicitHeight = 429
  PixelsPerInch = 96
  TextHeight = 13
  inherited web: TKeymanEmbeddedWB
    Width = 740
    Height = 400
    Silent = False
    ExplicitWidth = 816
    ExplicitHeight = 395
    ControlData = {
      4C00000078330000482100000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object dlgSaveFile: TSaveDialog
    Filter = 
      'Keyman Keyboard Files (*.kmx;*.kmp)|*.kmx;*.kmp|All Files (*.*)|' +
      '*.*'
    Left = 320
    Top = 180
  end
end
