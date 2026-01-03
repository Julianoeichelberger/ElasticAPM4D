unit Apm4D.Settings.User;

interface

type
  TUserSettings = class
  strict private
    FId: string;
    FName: string;
    FEmail: string;
    function GetUserName: string;
    function GetUserIdFromSystem: string;
    function GetEmail: string;
  public
    constructor Create;

    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

implementation

Uses
{$IFDEF MSWINDOWS} Winapi.Windows, Vcl.Forms, System.Win.ComObj, {$ENDIF}
  System.SysUtils, System.DateUtils, System.Variants;

{ TUserSettings }

constructor TUserSettings.Create;
begin
  FId := GetUserIdFromSystem;
  FName := GetUserName;
  FEmail := GetEmail;
end;

function TUserSettings.GetUserName: string;
begin
{$IFDEF MSWINDOWS}
  Result := GetEnvironmentVariable('USERNAME');
{$ELSE}
  Result := GetEnvironmentVariable('USER');
{$ENDIF}
end;

function TUserSettings.GetUserIdFromSystem: string;
{$IFDEF MSWINDOWS}
var
  TokenHandle: THandle;
  TokenUser: PTokenUser;
  dwLength: DWORD;
  StringSid: PWideChar;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  // Windows: Get SID (Security Identifier)
  try
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    begin
      try
        GetTokenInformation(TokenHandle, Winapi.Windows.TokenUser, nil, 0, dwLength);
        if GetLastError = ERROR_INSUFFICIENT_BUFFER then
        begin
          TokenUser := AllocMem(dwLength);
          try
            if GetTokenInformation(TokenHandle, Winapi.Windows.TokenUser, TokenUser, dwLength, dwLength) then
            begin
              if ConvertSidToStringSidW(TokenUser.User.Sid, StringSid) then
              begin
                try
                  Result := StringSid;
                finally
                  LocalFree(HLOCAL(StringSid));
                end;
              end;
            end;
          finally
            FreeMem(TokenUser);
          end;
        end;
      finally
        CloseHandle(TokenHandle);
      end;
    end;
  except
    Result := '';
  end;
{$ELSE}
  try
    Result := TFile.ReadAllText('/proc/self/loginuid').Trim;
  except
    try
      Result := GetEnvironmentVariable('UID');
    except
      Result := '';
    end;
  end;
{$ENDIF}
end;

function TUserSettings.GetEmail: string;
{$IFDEF MSWINDOWS}
//var
//  ADSysInfo: OleVariant;
//  ADUser: OleVariant;
{$ENDIF}
begin
  result := '';
{$IFDEF MSWINDOWS}
//  try
//    ADSysInfo := CreateOleObject('ADSystemInfo');
//    ADUser := CreateOleObject('LDAP');
//    ADUser := ADUser.OpenDSObject('LDAP://' + VarToStr(ADSysInfo.UserName), '', '', 0);
//    if not VarIsNull(ADUser.mail) and not VarIsEmpty(ADUser.mail) then
//      result := VarToStr(ADUser.mail);
//  except
//  end;
{$ENDIF}
end;

end.
