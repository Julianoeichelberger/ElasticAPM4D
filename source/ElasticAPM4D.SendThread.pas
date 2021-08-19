unit ElasticAPM4D.SendThread;

interface

Uses
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
    FTotalErrors: Integer;
    FConnectionError: Integer;
    FInternalList: TDataSendList;
    FResult: TThreadResult;
  protected
    procedure Execute; override;
  public
    constructor Create(const AUrl: string);
    destructor Destroy; override;

    property InternalList: TDataSendList read FInternalList write FInternalList;
    property Result: TThreadResult read FResult;

    property TotalErrors: Integer read FTotalErrors write FTotalErrors;
    property ConnectionError: Integer read FConnectionError write FConnectionError;
  end;

implementation

uses
  IDHttp, Forms, IOUtils, ElasticAPM4D.Config;

function SendToElasticAPM(AUrl, AHeader: string; const AJson: Widestring): Integer;

  procedure SaveLog(ARespCode: Integer);
  begin
    if TConfig.GetLogOutputFilePath.IsEmpty then
      exit;
    with TStringList.Create do
    begin
      Text := AJson;
      SaveToFile(IncludeTrailingPathDelimiter(TConfig.GetLogOutputFilePath) + TPath.GetFileNameWithoutExtension
        (Application.ExeName) + '_' + ARespCode.ToString + '_' + FormatDateTime('hh-mm-sss', now) + '.txt');
      Free;
    end;
  end;

var
  Http: TIdHTTP;
  DataSend, Stream: TStringStream;
begin
  if AJson = '' then
    exit(200);
  Http := TIdHTTP.Create;
  DataSend := TStringStream.Create(AJson, TEncoding.UTF8);
  Stream := TStringStream.Create('');
  try
    Http.Request.ContentType := 'application/x-ndjson';
    Http.Request.Charset := 'gzip';
    Http.Request.CustomHeaders.AddValue('elastic-apm-traceparent', AHeader);
    try
      Http.Post(AUrl, DataSend, Stream);
      Result := 200;
      SaveLog(Result);
    except
      on E: EIdHTTPProtocolException do
      begin
        Result := E.ErrorCode;
        SaveLog(Result);
      end;
      on E: Exception do
      begin
        Result := 500;
        SaveLog(Result);
      end;
    end;
  finally
    DataSend.Free;
    Stream.Free;
    Http.Free;
  end;
end;

{ TSendThread }

constructor TSendThread.Create(const AUrl: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FURL := AUrl;
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
      HttpCode := SendToElasticAPM(FURL, DataSend.Header, DataSend.Json);
      if HttpCode = 429 then
      begin
        Sleep(1000);
        Inc(FTotalErrors);
        HttpCode := SendToElasticAPM(FURL, DataSend.Header, DataSend.Json);
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
