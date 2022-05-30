{******************************************************************************}
{ JEDI API example <name> 											                        		   }
{ http://jedi-apilib.sourceforge.net										                       }
{ 																			                                       }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 																		                                     	   }
{ Author: Michael Morstein                        													   }
{ Creation date: 10th June 2008 					   			                             }
{ Last modification date:	10th June 2008                                       }
{ 																			                                       }
{ Description: Calculates a hash of a user-definied file                       }
{ 																			                                       }
{ 																			                                       }
{ Preparations: Needs the JWSCL (JwsclTypes, JwsclStreams, JwsclCryptProvider  }
{ 																		                                     	   }
{ Article link: http://blog.delphi-jedi.net     					               		   }
{ 																			                                       }
{ Version history: 10th June initial release					                    	   }
{ 																			                                       }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in 		     }
{ productive environments.                                                     }
{******************************************************************************}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JwsclTypes, JwsclStreams, JwsclCryptProvider, StdCtrls, Buttons;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    Button1: TButton;
    ComboBox1: TComboBox;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Label3: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    HashFile: String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetFileHash(Algorithm: TJwHashAlgorithm; const Filename: String) : String;
var
  Stream: TJwFileStreamEx;
  Hash: TJwHash;
  HashSize: Cardinal;
  HashData: Pointer;
  i: Integer;
begin
  Stream := TJwFileStreamEx.Create(Filename,fmOpenRead);
  Hash := TJwHash.Create(Algorithm);
  try
    Hash.HashData(Stream.Memory,Stream.Size);
    HashData := Hash.RetrieveHash(HashSize);

    for i:= 1 to HashSize do
    begin
      Result := Result + IntToHex(PByte(HashData)^,2);
      inc(PByte(HashData));
    end;
    dec(PByte(HashData),HashSize);

    TJwHash.FreeBuffer(HashData);
  finally
    Hash.Free;
    Stream.Free;
  end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if Opendialog1.Execute then
  begin
    HashFile := Opendialog1.FileName;
    Edit1.Text := HashFile;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Caption := GetFileHash(TJwHashAlgorithm(Combobox1.ItemIndex+1),HashFile);
end;

end.
