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
  object dlgSaveFile: TSaveDialog
    Filter = 
      'Keyman Keyboard Files (*.kmx;*.kmp)|*.kmx;*.kmp|All Files (*.*)|' +
      '*.*'
    Left = 320
    Top = 180
  end
end
