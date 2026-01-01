{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.SendThread;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  Vcl.ExtCtrls;

type
  TDataSend = Record
    Json: Widestring;
    Header: string;
  End;

  TDataSendList = TList<TDataSend>;

  TThreadResult = (trSuspended, trFinished, trSending);

  TSendThread = class(TThread)
  private
    FURL: string;
    FSecret: string;
    FTotalErrors: Integer;
    FConnectionError: Integer;
    FInternalList: TDataSendList;
    FResult: TThreadResult;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUrl: string; const ASecret: string = '');
    destructor Destroy; override;

    property InternalList: TDataSendList read FInternalList write FInternalList;
    property Result: TThreadResult read FResult;

    property TotalErrors: Integer read FTotalErrors write FTotalErrors;
    property ConnectionError: Integer read FConnectionError write FConnectionError;
  end;

implementation

uses
  IDHttp, Forms, IOUtils, Apm4D.Settings, IdSSLOpenSSL;

function SendToElasticAPM(AUrl, ASecret, AHeader: string; const AJson: Widestring): Integer;

  procedure SaveLog(ARespCode: Integer; AMessage: string);
  begin
    if TApm4DSettings.Log.OutputFileDir.IsEmpty then
      exit;
    with TStringList.Create do
    begin
      Add(AMessage);
      Add(AJson);
      SaveToFile(IncludeTrailingPathDelimiter(TApm4DSettings.Log.OutputFileDir) + TPath.GetFileNameWithoutExtension
        (Application.ExeName) + '_' + ARespCode.ToString + '_' + FormatDateTime('hh-mm-sss', now) + '.log');
      Free;
    end;
  end;

var
  Http: TIdHTTP;
  DataSend, Stream: TStringStream;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  if AJson = '' then
    exit(200);
  Http := TIdHTTP.Create;
  if AUrl.StartsWith('https:') then
  begin
    SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Http);
    SSLHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    Http.IOHandler := SSLHandler;
  end;

  DataSend := TStringStream.Create(AJson, TEncoding.UTF8);
  Stream := TStringStream.Create('');
  try
    Http.Request.ContentType := 'application/x-ndjson';
    Http.Request.Charset := 'gzip';
    Http.Request.CustomHeaders.AddValue('elastic-apm-traceparent', AHeader);
    if not ASecret.IsEmpty then
    begin
      Http.Request.CustomHeaders.AddValue('Authorization', 'Bearer ' + ASecret);
    end;

    try
      Http.Post(AUrl, DataSend, Stream);
      Result := 200;
      SaveLog(Result, 'Ok');
    except
      on E: EIdHTTPProtocolException do
      begin
        Result := E.ErrorCode;
        SaveLog(Result, E.Message);
      end;
      on E: Exception do
      begin
        Result := 500;
        SaveLog(Result, E.Message);
      end;
    end;
  finally
    DataSend.Free;
    Stream.Free;
    Http.Free;
  end;
end;

{ TSendThread }

constructor TSendThread.Create(const AUrl: string; const ASecret: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FURL := AUrl;
  FSecret := ASecret;
  FInternalList := TDataSendList.Create;
  FResult := trSuspended;
end;

destructor TSendThread.Destroy;
begin
  FreeAndNil(FInternalList);
  inherited;
end;

procedure TSendThread.Execute;
var
  DataSend: TDataSend;
  HttpCode: Integer;
  OcurrConnectionError: Boolean;
begin
  inherited;
  OcurrConnectionError := False;
  FResult := trSending;
  try
    for DataSend in FInternalList.List do
    begin
      HttpCode := SendToElasticAPM(FURL, FSecret, DataSend.Header, DataSend.Json);
      if HttpCode = 429 then
      begin
        Sleep(1000);
        Inc(FTotalErrors);
        HttpCode := SendToElasticAPM(FURL, FSecret, DataSend.Header, DataSend.Json);
      end;
      if (HttpCode >= 500) or (HttpCode = 429) then
      begin
        if not OcurrConnectionError then
        begin
          Inc(FConnectionError);
          OcurrConnectionError := True;
        end;
        Inc(FTotalErrors);
        Terminate;
      end;
    end;
  finally
    if not OcurrConnectionError then
      FConnectionError := 0;
    FResult := trFinished;
  end;
end;

end.
