{*******************************************************}
{              EmbeddedWB - Drag And Drop Demo          }
{  by  Eran Bodankin (bsalsa) bsalsa@bsalsa.com         }
{                       Enjoy!                          }
{   UPDATES:                                            }
{               http://www.bsalsa.com                   }
{*******************************************************}
{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please,  consider donation in our web site!  }
{*******************************************************************************}

unit frmMain;

interface

uses
   sysUtils, Classes, Controls, Forms, EmbeddedWB, StdCtrls, IEAddress,
   ExtCtrls, Dialogs, ActiveX, OleCtrls, SHDocVw_EWB, Windows, ComObj, EwbCore;

type
   TForm1 = class(TForm)
      Panel1: TPanel;
      Button1: TButton;
      IEAddress1: TIEAddress;
      Panel2: TPanel;
      Memo1: TMemo;
      Panel3: TPanel;
      EmbeddedWB1: TEmbeddedWB;
      Label1: TLabel;
      Label2: TLabel;
    procedure EmbeddedWB1DropEvent(Sender: TObject; const dataObj: IDataObject;
      grfKeyState: Integer; pt: TPoint; var dwEffect: Integer;
      var Rezult: HRESULT);
    procedure EmbeddedWB1DragEnter(Sender: TObject; const dataObj: IDataObject;
      grfKeyState: Integer; pt: TPoint; var dwEffect: Integer;
      var Rezult: HRESULT);
    procedure EmbeddedWB1DragLeave(Sender: TObject);
    procedure EmbeddedWB1DragOver2(Sender: TObject; grfKeyState: Integer;
      pt: TPoint; var dwEffect: Integer; var Rezult: HRESULT);
      procedure FormDestroy(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure Button1Click(Sender: TObject);
    procedure EmbeddedWB1GetDropTarget(Sender: TCustomEmbeddedWB;
      var DropTarget: IDropTarget);
   private
    { Private declarations }
   public
    { Public declarations }
   end;

var
   Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
   Memo1.Clear;
   EmbeddedWB1.Go(IEAddress1.Text);
end;

procedure TForm1.EmbeddedWB1DragEnter(Sender: TObject;
  const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer; var Rezult: HRESULT);
begin
   Memo1.Lines.Add('============');
   Memo1.Lines.Add('OnDragEnter event - Key Status: ' + IntToStr(grfKeyState));
   Memo1.Lines.Add('OnDragEnter event - Point X: ' + IntToStr(pt.x) + ' Point Y: ' + IntToStr(pt.y));
   Memo1.Lines.Add('OnDragEnter event - Command: ' + IntToStr(dwEffect));
   Memo1.Lines.Add('============');
end;

procedure TForm1.EmbeddedWB1DragLeave(Sender: TObject);
begin
   Memo1.Lines.Add('OnDragLeave event.');
   Memo1.Lines.Add('============');
end;

procedure TForm1.EmbeddedWB1DragOver2(Sender: TObject; grfKeyState: Integer;
  pt: TPoint; var dwEffect: Integer; var Rezult: HRESULT);
begin
   Memo1.Lines.Add('OnDragOver2 event - Key State: ' + IntToStr(grfKeyState));
   Memo1.Lines.Add('OnDragOver2 event - Point X: ' + IntToStr(pt.x) + ' Point Y: ' + IntToStr(pt.y));
   Memo1.Lines.Add('OnDragOver2 event - Command: ' + IntToStr(dwEffect));
   Memo1.Lines.Add('============');
end;

procedure TForm1.EmbeddedWB1DropEvent(Sender: TObject;
  const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer; var Rezult: HRESULT);
begin
   Memo1.Lines.Add('OnDrop event - Key State: ' + IntToStr(grfKeyState));
   Memo1.Lines.Add('OnDrop event - Point X: ' + IntToStr(pt.x) + ' Point Y: ' + IntToStr(pt.y));
   Memo1.Lines.Add('OnDrop event - Command: ' + IntToStr(dwEffect));
   Memo1.Lines.Add('============');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   OleCheck(RegisterDragDrop(Handle, EmbeddedWB1));
// To disable Drag and drop, remove the OleCheck
// and use the boolean property EnableDragAndDrop after removing the OleCheck
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   RevokeDragDrop(EmbeddedWB1.Handle);
end;

procedure TForm1.EmbeddedWB1GetDropTarget(Sender: TCustomEmbeddedWB;
  var DropTarget: IDropTarget);
begin
   Memo1.Lines.Add('OnGetDropTarget event.');
   Memo1.Lines.Add('============');
end;

{initialization
   OleInitialize(nil);
finalization
   try
      OleUninitialize;
   except
   end;     }
end.

