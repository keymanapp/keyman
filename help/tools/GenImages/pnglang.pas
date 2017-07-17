{Portable Network Graphics Delphi Language Info (24 July 2002)}

{Feel free to change the text bellow to adapt to your language}
{Also if you have a translation to other languages and want to}
{share it, send me: gubadaud@terra.com.br                     }
unit pnglang;

interface

{$DEFINE English}
{.$DEFINE Portuguese}
{.$DEFINE German}

{Language strings for english}
resourcestring
  {$IFDEF English}
  EPngInvalidCRCText = 'This "Portable Network Graphics" image is not valid ' +
      'because it contains invalid pieces of data (crc error)';
  EPNGInvalidIHDRText = 'The "Portable Network Graphics" image could not be ' +
      'loaded because one of its main piece of data (ihdr) might be corrupted';
  EPNGMissingMultipleIDATText = 'This "Portable Network Graphics" image is ' +
    'invalid because it has missing image parts.';
  EPNGZLIBErrorText = 'Could not decompress the image because it contains ' +
    'invalid compressed data.'#13#10 + ' Description: ';
  EPNGInvalidPaletteText = 'The "Portable Network Graphics" image contains ' +
    'an invalid palette.';
  EPNGInvalidFileHeaderText = 'The file being readed is not a valid '+
    '"Portable Network Graphics" image because it contains an invalid header.' +
    ' This file may be corruped, try obtaining it again.';
  EPNGIHDRNotFirstText = 'This "Portable Network Graphics" image is not ' +
    'supported or it might be invalid.'#13#10 + '(IHDR chunk is not the first)';
  EPNGNotExistsText = 'The png file could not be loaded because it does not ' +
    'exists.';
  EPNGSizeExceedsText = 'This "Portable Network Graphics" image is not ' +
    'supported because either it''s width or height exceeds the maximum ' +
    'size, which is 65535 pixels length.';
  EPNGUnknownPalEntryText = 'There is no such palette entry.';
  EPNGMissingPaletteText = 'This "Portable Network Graphics" could not be ' +
    'loaded because it uses a color table which is missing.';
  EPNGUnknownCriticalChunkText = 'This "Portable Network Graphics" image ' +
    'contains an unknown critical part which could not be decoded.';
  EPNGUnknownCompressionText = 'This "Portable Network Graphics" image is ' +
    'encoded with an unknown compression scheme which could not be decoded.';
  EPNGUnknownInterlaceText = 'This "Portable Network Graphics" image uses ' +
    'an unknown interlace scheme which could not be decoded.';
  EPNGCannotAssignChunkText = 'The chunks must be compatible to be assigned.';
  EPNGUnexpectedEndText = 'This "Portable Network Graphics" image is invalid ' +
    'because the decoder found an unexpected end of the file.';
  EPNGNoImageDataText = 'This "Portable Network Graphics" image contains no ' +
    'data.';
  EPNGCannotChangeSizeText = 'The "Portable Network Graphics" image can not ' +
    'be resize by changing width and height properties. Try assigning the ' +
    'image from a bitmap.';
  EPNGCannotAddChunkText = 'The program tried to add a existent critical ' +
    'chunk to the current image which is not allowed.';
  EPNGCannotAddInvalidImageText = 'It''s not allowed to add a new chunk ' +
    'because the current image is invalid.';
  EPNGCouldNotLoadResourceText = 'The png image could not be loaded from the ' +
    'resource ID.';
  EPNGOutMemoryText = 'Some operation could not be performed because the ' +
    'system is out of resources. Close some windows and try again.';
  EPNGCannotChangeTransparentText = 'Setting bit transparency color is not ' +
    'allowed for png images containing alpha value for each pixel ' +
    '(COLOR_RGBALPHA and COLOR_GRAYSCALEALPHA)';
  EPNGHeaderNotPresentText = 'This operation is not valid because the ' +
    'current image contains no valid header.';
  EPNGAlphaNotSupportedText = 'The current "Portable Network Graphics" image ' +
    'color type either contains already alpha information or it can not ' +
    'be converted.';
  {$ENDIF}
  {$IFDEF Portuguese}
  EPngInvalidCRCText = 'Essa imagem "Portable Network Graphics" n�o � v�lida ' +
      'porque cont�m chunks inv�lidos de dados (erro crc)';
  EPNGInvalidIHDRText = 'A imagem "Portable Network Graphics" n�o pode ser ' +
      'carregada porque um dos seus chunks importantes (ihdr) pode estar '+
      'inv�lido';
  EPNGMissingMultipleIDATText = 'Essa imagem "Portable Network Graphics" � ' +
    'inv�lida porque tem chunks de dados faltando.';
  EPNGZLIBErrorText = 'N�o foi poss�vel descomprimir os dados da imagem ' +
    'porque ela cont�m dados inv�lidos.'#13#10 + ' Descri��o: ';
  EPNGInvalidPaletteText = 'A imagem "Portable Network Graphics" cont�m ' +
    'uma paleta inv�lida.';
  EPNGInvalidFileHeaderText = 'O arquivo sendo lido n�o � uma imagem '+
    '"Portable Network Graphics" v�lida porque cont�m um cabe�alho inv�lido.' +
    ' O arquivo pode estar corrompida, tente obter ela novamente.';
  EPNGIHDRNotFirstText = 'Essa imagem "Portable Network Graphics" n�o � ' +
    'suportada ou pode ser inv�lida.'#13#10 + '(O chunk IHDR n�o � o ' +
    'primeiro)';
  EPNGNotExistsText = 'A imagem png n�o pode ser carregada porque ela n�o ' +
    'existe.';
  EPNGSizeExceedsText = 'Essa imagem "Portable Network Graphics" n�o � ' +
    'suportada porque a largura ou a altura ultrapassam o tamanho m�ximo, ' +
    'que � de 65535 pixels de di�metro.';
  EPNGUnknownPalEntryText = 'N�o existe essa entrada de paleta.';
  EPNGMissingPaletteText = 'Essa imagem "Portable Network Graphics" n�o pode ' +
    'ser carregada porque usa uma paleta que est� faltando.';
  EPNGUnknownCriticalChunkText = 'Essa imagem "Portable Network Graphics" ' +
    'cont�m um chunk cr�tico desconhe�ido que n�o pode ser decodificado.';
  EPNGUnknownCompressionText = 'Essa imagem "Portable Network Graphics" est� ' +
    'codificada com um esquema de compress�o desconhe�ido e n�o pode ser ' +
    'decodificada.';
  EPNGUnknownInterlaceText = 'Essa imagem "Portable Network Graphics" usa um ' +
    'um esquema de interlace que n�o pode ser decodificado.';
  EPNGCannotAssignChunkText = 'Os chunk devem ser compat�veis para serem ' +
    'copiados.';
  EPNGUnexpectedEndText = 'Essa imagem "Portable Network Graphics" � ' +
    'inv�lida porque o decodificador encontrou um fim inesperado.';
  EPNGNoImageDataText = 'Essa imagem "Portable Network Graphics" n�o cont�m ' +
    'dados.';
  EPNGCannotChangeSizeText = 'A imagem "Portable Network Graphics" n�o pode ' +
    'ser redimensionada mudando as propriedades width e height. Tente ' +
    'copiar a imagem de um bitmap usando a fun��o assign.';
  EPNGCannotAddChunkText = 'O programa tentou adicionar um chunk cr�tico ' +
    'j� existente para a imagem atual, oque n�o � permitido.';
  EPNGCannotAddInvalidImageText = 'N�o � permitido adicionar um chunk novo ' +
    'porque a imagem atual � inv�lida.';
  EPNGCouldNotLoadResourceText = 'A imagem png n�o pode ser carregada apartir' +
    ' do resource.';
  EPNGOutMemoryText = 'Uma opera��o n�o pode ser completada porque o sistema ' +
    'est� sem recursos. Fecha algumas janelas e tente novamente.';
  EPNGCannotChangeTransparentText = 'Definir transpar�ncia booleana n�o � ' +
    'permitido para imagens png contendo informa��o alpha para cada pixel ' +
    '(COLOR_RGBALPHA e COLOR_GRAYSCALEALPHA)';
  EPNGHeaderNotPresentText = 'Essa opera��o n�o � v�lida porque a ' +
    'imagem atual n�o cont�m um cabe�alho v�lido.';
  EPNGAlphaNotSupportedText = 'A imagem "Portable Network Graphics" ou j� ' +
    'cont�m informa��es de transpar�ncia, ou n�o suporta a convers�o.';
  {$ENDIF}
  {Language strings for German}
  {$IFDEF German}
  EPngInvalidCRCText = 'Dieses "Portable Network Graphics" Image ist ' +
      'ung�ltig, weil Teile der Daten ung�ltig sind (CRC-Fehler).';
  EPNGInvalidIHDRText = 'Dieses "Portable Network Graphics" Image konnte ' +
      'nicht geladen werden, weil eine der Hauptdaten (IHDR) besch�digt ' +
      'sein k�nnte.';
  EPNGMissingMultipleIDATText = 'Dieses "Portable Network Graphics" Image ' +
    'ist ung�ltig, weil Grafikdaten fehlen.';
  EPNGZLIBErrorText = 'Die Grafik konnte nicht entpackt werden, weil sie ' +
    'fehlerhafte komprimierte Daten enth�lt.'#13#10 + ' Beschreibung: ';
  EPNGInvalidPaletteText = 'Das "Portable Network Graphics" Image enth�lt ' +
    'eine ung�ltige Palette.';
  EPNGInvalidFileHeaderText = 'Die Datei, die gelesen wird, ist kein ' +
    'g�ltiges "Portable Network Graphics" Image, da es keinen g�ltigen ' +
    'Header enth�lt. Die Datei k�nnte besch�digt sein, versuchen Sie, ' +
    'eine neue Kopie zu bekommen.';
  EPNGIHDRNotFirstText = 'Dieses "Portable Network Graphics" Image wird ' +
    'nicht unterst�tzt bzw. es k�nnte ung�ltig sein.'#13#10 +
    '(Der IHDR-Chunk ist nicht der erste Chunk in der Datei).';
  EPNGNotExistsText = 'Die PNG Datei konnte nicht geladen werden, da sie ' +
    'nicht existiert.';
  EPNGSizeExceedsText = 'Dieses "Portable Network Graphics" Image wird nicht ' +
    'unterst�tzt, weil entweder seine Breite oder seine H�he das Maximum von ' +
    '65535 Pixeln �berschreitet.';
  EPNGUnknownPalEntryText = 'Es gibt keinen solchen Palettenwert.';
  EPNGMissingPaletteText = 'Dieses "Portable Network Graphics" Image konnte ' +
    'nicht geladen werden, weil die ben�tigte Farbtabelle fehlt.';
  EPNGUnknownCriticalChunkText = 'Dieses "Portable Network Graphics" Image ' +
    'enh�lt einen unbekannten kritischen Teil, welcher nicht entschl�sselt ' +
    'werden kann.';
  EPNGUnknownCompressionText = 'Dieses "Portable Network Graphics" Image ' +
    'wurde mit einem unbekannten Komprimierungsalgorithmus kodiert, welcher ' +
    'nicht entschl�sselt werden kann.';
  EPNGUnknownInterlaceText = 'Dieses "Portable Network Graphics" Image ' +
    'benutzt ein unbekanntes Interlace-Schema, welcher nicht entschl�sselt ' +
    'werden kann.';
  EPNGCannotAssignChunkText = 'Die Chunks m�ssen kompatibel sein, um ' +
    'zugewiesen werden zu k�nnen.';
  EPNGUnexpectedEndText = 'Dieses "Portable Network Graphics" Image ist ' +
    'ung�ltig, der Dekoder stie� unerwarteterweise auf das Ende der Datei.';
  EPNGNoImageDataText = 'Dieses "Portable Network Graphics" Image enth�lt ' +
    'keine Daten.';
  EPNGCannotChangeSizeText = 'Das "Portable Network Graphics" Image kann ' +
    'nicht durch �ndern der Eigenschaften Width und Height in seinen ' +
    'Abmessungen ge�ndert werden. Versuchen Sie das Image von einer Bitmap ' +
    'aus zuzuweisen.';
  EPNGCannotAddChunkText = 'Das Programm versucht einen existierenden ' +
    'kritischen Chunk zum aktuellen Image hinzuzuf�gen. Dies ist nicht ' +
    'zul�ssig.';
  EPNGCannotAddInvalidImageText = 'Es ist nicht zul�ssig, dem aktuellen ' +
    'Image einen neuen Chunk hinzuzuf�gen, da es ung�ltig ist.';
  EPNGCouldNotLoadResourceText = 'Das PNG Image konnte nicht von den ' +
    'Resourcendaten geladen werden.';
  EPNGOutMemoryText = 'Es stehen nicht gen�gend Resourcen im System zur ' +
    'Verf�gung, um die Operation auszuf�hren. Schlie�en Sie einige Fenster '+
    'und versuchen Sie es erneut.';
  EPNGCannotChangeTransparentText = 'Das Setzen der Bit-' +
    'Transparent-Farbe ist fuer PNG-Images die Alpha-Werte fuer jedes ' +
    'Pixel enthalten (COLOR_RGBALPHA und COLOR_GRAYSCALEALPHA) nicht ' +
    'zulaessig';
  EPNGHeaderNotPresentText = 'Die Datei, die gelesen wird, ist kein ' +
    'g�ltiges "Portable Network Graphics" Image, da es keinen g�ltigen ' +
    'Header enth�lt.';
  EPNGAlphaNotSupportedText = 'The current "Portable Network Graphics" image ' +
    'color type either contains already alpha information or it can not ' +
    'be converted.';
  {$ENDIF}

implementation

end.
