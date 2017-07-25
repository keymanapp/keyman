(*
  Name:             SetupForm
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Jun 2007

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Jun 2007 - mcdurdin - I817 - Translate to Unicode and remove Forms dependence
                    23 Aug 2007 - mcdurdin - Initial version from Desktop
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit SetupForm;  // I3306

interface

uses
  Windows,
  Classes,
  Messages,
  TntDialogHelp;

type
  TSetupForm = class(TComponent)
  private
    FHandle: HWND;
    FModalResult: Integer;
    DlgFunc: Pointer;
    procedure IntDlgMain(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowModal: Integer;
    property Handle: HWND read FHandle;
    property ModalResult: Integer read FModalResult write FModalResult;
  public
    function DialogID: Integer; virtual; abstract;
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormShow(Sender: TObject); virtual;
    function ProcessCommand(ID, NotificationCode: Integer; hControl: HWND): Boolean; virtual;
    procedure DlgMain(var Message: TMessage); virtual;
  end;

implementation

uses
  SysUtils;

{ TSetupForm }

constructor TSetupForm.Create(AOwner: TComponent);
begin
  inherited;
  DlgFunc := MakeObjectInstance(IntDlgMain);
  FHandle := CreateDialog(HInstance, MakeIntResourceW(DialogID), 0, DlgFunc);

  if Handle = 0 then RaiseLastOSError;
  FormCreate(Self);
end;

destructor TSetupForm.Destroy;
begin
  DestroyWindow(Handle);
  FreeObjectInstance(DlgFunc);
  inherited Destroy;
end;

procedure TSetupForm.DlgMain(var Message: TMessage);
begin
  case Message.Msg of
    WM_INITDIALOG:
      Message.Result := 1;
    WM_COMMAND:
      if ProcessCommand(LOWORD(Message.WParam), HIWORD(Message.WParam), Message.LParam)
        then Message.Result := 1
        else Message.Result := 0;
    else
      Message.Result := 0;
  end;
end;

procedure TSetupForm.IntDlgMain(var Message: TMessage);
begin
  DlgMain(Message);
end;

procedure TSetupForm.FormCreate(Sender: TObject);
begin

end;

procedure TSetupForm.FormShow(Sender: TObject);
begin

end;

function TSetupForm.ProcessCommand(ID, NotificationCode: Integer;
  hControl: HWND): Boolean;
begin
  Result := False;
end;

function TSetupForm.ShowModal: Integer;
var
  msg: TMsg;
begin
  ShowWindow(Handle, SW_SHOWNORMAL);
  FormShow(Self);
  repeat
    if not GetMessage(msg, 0, 0, 0) then ModalResult := mrCancel
    else
    begin
      if not IsDialogMessage(Handle, msg) then
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end;
    if ModalResult <> 0 then ShowWindow(Handle, SW_HIDE);
  until ModalResult <> 0;
  Result := ModalResult;
end;

end.
