{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit contains classes and methods to create a proper bug report for JEDI API service.

Author
Christian Wimmer

License
The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and  
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

Note
The Original Code is JwsclEurekaLogUtils.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
unit JwsclEurekaLogUtils;

interface
uses
  Classes,
  TypInfo,
  SysUtils,
  JwaWindows,
  JclFileUtils,
  JwsclToken,
  JwsclExceptions,
  ExceptionLog;

type
  {TJwEurekaLogNotify defines a class with event methods that are
  used with the TEurekaLog component.

  }
  TJwEurekaLogNotify = class(TComponent)
  protected
    fApplicationName : String;
  public
     { Create creates a new class instance. You should set the owner
       to the Eurekalog component you want to use so this class is
       automatically freed. This class does not anything of the
       Eurekalog component.
       
       
       Parameters
       AOwner :           Set the owner of this class. If the owner
                          is freed this class instance will also be
                          freed; otherwise the creator have to free
                          it. 
       ApplicationName :  Name of this application. Must not be empty
       
       Exceptions
       EJwsclInvalidParameterException :  This exception will be
                                          raised if parameter ApplicationName
                                          is empty.                           }
     constructor Create(AOwner: TComponent;const ApplicationName : String);
     destructor Destroy; override;

     {WebFieldsRequestNotify adds file version information to the bug report
      }
     procedure WebFieldsRequestNotify(
        EurekaExceptionRecord: TEurekaExceptionRecord;
        WebFields: TStrings);

     { This property sets and gets the Name of the Application. It
       is used to set the BugReport ApplicationName.               }
     property ApplicationName : String read fApplicationName write fApplicationName;
  end;

procedure JEDI_WebFieldsRequestNotify(
        EurekaExceptionRecord: TEurekaExceptionRecord;
        WebFields: TStrings);

{$IFDEF DEBUG}
//for testing Eurekalog only!
procedure CreateLeak;
{$ENDIF DEBUG}
implementation

{$IFDEF DEBUG}
procedure CreateLeak;
var P : Pointer;
begin
  GetMEm(P, 100);
end;
{$ENDIF DEBUG}

constructor TJwEurekaLogNotify.Create(AOwner: TComponent;
  const ApplicationName : String); 
begin
  inherited Create(AOwner);
  if Length(ApplicationName) = 0 then
    raise EJwsclInvalidParameterException.CreateFmtEx(
      'ApplicationName must not be empty', 'Create', ClassName,
      'JwsclEurekaLogUtils', 0, False, []);

  fApplicationName := ApplicationName;
end;

destructor TJwEurekaLogNotify.Destroy;
begin
  fApplicationName := '';
  inherited;
end;

procedure TJwEurekaLogNotify.WebFieldsRequestNotify(
      EurekaExceptionRecord: TEurekaExceptionRecord;
      WebFields: TStrings);
begin
  WebFieldsRequestNotify(EurekaExceptionRecord,WebFields);

  if WebFields.IndexOf('ApplicationName') > 0 then
  begin
    if Length(WebFields.Values['ApplicationName']) = 0 then
      WebFields.Values['ApplicationName'] := 'JEDI Application';
  end;
end;


procedure JEDI_WebFieldsRequestNotify(
    EurekaExceptionRecord: TEurekaExceptionRecord;
    WebFields: TStrings);

procedure AddField(const FieldName, Value : String); overload;
begin
  WebFields.Add(FieldName+'='+Value);
end;
procedure AddField(const FieldName: String; const Value : Cardinal); overload;
begin
  WebFields.Add(FieldName+'='+IntToStr(Value)+'/$'+IntToHex(Value,4));
end;

var
  F : TJclFileVersionInfo;
  i1 : TFileFlag;
  AppName,
  S : String;
begin

  try
    F := TJclFileVersionInfo.Create(ParamStr(0));
    with F do  //<--- only for F.AddField
    begin
      AddField('Info_BinFileVersion',BinFileVersion);
      AddField('Info_BinProductVersion',BinProductVersion);
      AddField('Info_Comments',Comments);
      AddField('Info_CompanyName',CompanyName);
      AddField('Info_FileDescription',FileDescription);
      AddField('Info_FixedInfo','()');
      AddField('Info_FixedInfo.dwSignature', FixedInfo.dwSignature);
      AddField('Info_FixedInfo.dwStrucVersion',FixedInfo.dwStrucVersion);
      AddField('Info_FixedInfo.dwFileVersionMS',FixedInfo.dwFileVersionMS);
      AddField('Info_FixedInfo.dwFileVersionLS',FixedInfo.dwFileVersionLS);
      AddField('Info_FixedInfo.dwProductVersionMS',FixedInfo.dwProductVersionMS);
      AddField('Info_FixedInfo.dwProductVersionLS',FixedInfo.dwProductVersionLS);
      AddField('Info_FixedInfo.dwFileFlagsMask',FixedInfo.dwFileFlagsMask);
      AddField('Info_FixedInfo.dwFileFlags',FixedInfo.dwFileFlags);
      AddField('Info_FixedInfo.dwFileOS',FixedInfo.dwFileOS);
      AddField('Info_FixedInfo.dwFileType',FixedInfo.dwFileType);
      AddField('Info_FixedInfo.dwFileSubtype',FixedInfo.dwFileSubtype);
      AddField('Info_FixedInfo.dwFileDateMS',FixedInfo.dwFileDateMS);
      AddField('Info_FixedInfo.dwFileDateLS',FixedInfo.dwFileDateLS);

      S := '';
      for i1 := low(TFileFlag) to high(TFileFlag) do
      begin
        S := S + ','+GetEnumName(TypeInfo(TFileFlag), integer(i1));
      end;
      System.Delete(S,1,1);
      AddField('Info_FileFlags',S);

      AddField('Info_FileOS',FileOS);
      AddField('Info_FileSubType',FileSubType);
      AddField('Info_FileType',FileType);
      AddField('Info_FileVersion',FileVersion);
      AddField('Info_Items',Items.CommaText);
      AddField('Info_InternalName',InternalName);
      AddField('Info_LanguageCount',LanguageCount);
      AddField('Info_LegalCopyright',LegalCopyright);
      AddField('Info_LegalTradeMarks',LegalTradeMarks);
      AddField('Info_OriginalFilename',OriginalFilename);
      AddField('Info_PrivateBuild',PrivateBuild);
      AddField('Info_ProductName',ProductName);
      AppName := ProductName;
      AddField('Info_ProductVersion',ProductVersion);
      AddField('ApplicationName',AppName);      
      Free;
    end;
  except
    on E : Exception do
      OutputDebugStringA(PChar('Could not get version information: '+E.Message));
  end;
end;


end.
 