{$B-}
unit HyperLinksDecorator;

{

 Functions to 'decorate' hyper-links
   (search for URLs and e-mails and replace 
    them with appropriate HTML-links).
 Uses TRegExpr library.

 (c) 2002 Andrey V. Sorokin, Saint Petersburg, Russia
  mailto:anso@mail.ru
  http://www.RegExpStudio.com

 v. 0.101 2002.08.30 
  -=- (-) Missed closing tag </a>

 Note:
  This functions have to be optimized - they construct result strings
  with step by step concatenation that can take a lot of resources while
  processing big input texts with many hyper links.

}

interface

uses
 RegExpr;

type
TDecorateURLsFlags = (
 // describes, which parts of hyper-link must be included
 // into VISIBLE part of the link:
  durlProto, // Protocol (like 'ftp://' or 'http://')
  durlAddr,  // TCP address or domain name (like 'RegExpStudio.com')
  durlPort,  // Port number if specified (like ':8080')
  durlPath,  // Path to document (like 'index.html')
  durlBMark, // Book mark (like '#mark')
  durlParam  // URL params (like '?ID=2&User=13')
 );

TDecorateURLsFlagSet = set of TDecorateURLsFlags;


function DecorateURLs (
 // can find hyper links like 'http://...' or 'ftp://..'
 // as well as links without protocol, but start with 'www.'

 const AText : string;
 // Input text to find hyper-links

  AFlags : TDecorateURLsFlagSet = [durlAddr, durlPath]
 // Which part of hyper-links found must be included into visible
 // part of URL, for example if [durlAddr] then hyper link
 // 'www.RegExpStudio.com/contacts.html' will be decorated as
 // '<a href="http://www.RegExpStudio.com/contacts.html">www.RegExpStudio.com</a>'

  ) : string;
 // Returns input text with decorated hyper links


function DecorateEMails (
 // Replaces all syntax correct e-mails
 // with '<a href="mailto:ADDR">ADDR</a>'
 // For example, replaces 'anso@mail.ru'
 // with '<a href="mailto:anso@mail.ru">anso@mail.ru</a>'.

 const AText : string
 // Input text to find e-mails

  ) : string;
 // Returns input text with decorated e-mails


implementation

uses
 SysUtils; // we are using AnsiCompareText

function DecorateURLs (const AText : string;
  AFlags : TDecorateURLsFlagSet = [durlAddr, durlPath]
  ) : string; 
const 
  URLTemplate = 
   '(?i)' 
   + '(' 
   + '(FTP|HTTP)://'             // Protocol 
   + '|www\.)'                   // trick to catch links without
                                 // protocol - by detecting of starting 'www.'
   + '([\w\d\-]+(\.[\w\d\-]+)+)' // TCP addr or domain name
   + '(:\d\d?\d?\d?\d?)?'        // port number
   + '(((/[%+\w\d\-\\\.]*)+)*)'  // unix path
   + '(\?[^\s=&]+=[^\s=&]+(&[^\s=&]+=[^\s=&]+)*)?'
                                 // request (GET) params
   + '(#[\w\d\-%+]+)?';          // bookmark
var
  PrevPos : integer;
  s, Proto, Addr, HRef : string;
begin
  Result := ''; 
  PrevPos := 1; 
  with TRegExpr.Create do try 
     Expression := URLTemplate; 
     if Exec (AText) then 
      REPEAT 
        s := ''; 
        if AnsiCompareText (Match [1], 'www.') = 0 then begin
           Proto := 'http://';
           Addr := Match [1] + Match [3];
           HRef := Proto + Match [0];
          end
         else begin
           Proto := Match [1];
           Addr := Match [3];
           HRef := Match [0];
          end;
        if durlProto in AFlags
         then s := s + Proto;
        if durlAddr in AFlags
         then s := s + Addr;
        if durlPort in AFlags
         then s := s + Match [5];
        if durlPath in AFlags
         then s := s + Match [6];
        if durlParam in AFlags
         then s := s + Match [9];
        if durlBMark in AFlags
         then s := s + Match [11];
        Result := Result + System.Copy (AText, PrevPos,
         MatchPos [0] - PrevPos) + '<a href="' + HRef + '">' + s + '</a>'; //###0.101
        PrevPos := MatchPos [0] + MatchLen [0];
      UNTIL not ExecNext;
     Result := Result + System.Copy (AText, PrevPos, MaxInt); // Tail
    finally Free;
   end;
end; { of function DecorateURLs
--------------------------------------------------------------}

function DecorateEMails (const AText : string) : string;
 const
  MailTemplate =
   '[_a-zA-Z\d\-\.]+@[_a-zA-Z\d\-]+(\.[_a-zA-Z\d\-]+)+';
 var
  PrevPos : integer;
 begin
  Result := '';
  PrevPos := 1;
  with TRegExpr.Create do try
     Expression := MailTemplate;
     if Exec (AText) then
      REPEAT
        Result := Result + System.Copy (AText, PrevPos,
         MatchPos [0] - PrevPos) + '<a href="mailto:' + Match [0] + '">' + Match [0] + '</a>';
        PrevPos := MatchPos [0] + MatchLen [0];
      UNTIL not ExecNext;
     Result := Result + System.Copy (AText, PrevPos, MaxInt); // Tail
    finally Free;
   end;
 end; { of function DecorateEMails
--------------------------------------------------------------}


end.
