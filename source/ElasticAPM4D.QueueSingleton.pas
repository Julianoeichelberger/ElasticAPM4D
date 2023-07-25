{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit ElasticAPM4D.QueueSingleton;
interface

Uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs, Vcl.ExtCtrls,
  ElasticAPM4D.SendThread, ElasticAPM4D.Config;

type
  TQueueSingleton = class
  private type
    TThreadController = class(TThread)
    private
      FThread: TSendThread;
      FMasterList: TDataSendList;
      FSession: TCriticalSection;
      FEvent: TEvent;
      procedure CloneList(AList: TDataSendList);
      function ListIsEmpty: Boolean;
      procedure FinalizeThread;
    protected
      procedure Execute; override;
    public
      constructor Create; virtual;
      destructor Destroy; override;

      procedure Add(const AJson, AHeader: string);
    end;
  private
    class var FThreadMain: TThreadController;
    class var FSession: TCriticalSection;
  private
    class procedure Init;
    class procedure Finalize;
  public
    class procedure StackUp(const AJson, AHeader: string);
  end;

implementation

{ TQueueSingleton.TThreadController }

procedure TQueueSingleton.TThreadController.CloneList(AList: TDataSendList);
var
  DataSend: TDataSend;
begin
  FSession.Enter;
  try
    for DataSend in FMasterList.List do
      AList.Add(DataSend);
    FMasterList.Clear;
  finally
    FSession.Release;
  end;
end;

constructor TQueueSingleton.TThreadController.Create;
begin
  inherited Create(True);
  FEvent := TEvent.Create(nil, False, True, 'TThreadControllerEvent');
  FSession := TCriticalSection.Create;
  FMasterList := TDataSendList.Create;
end;

destructor TQueueSingleton.TThreadController.Destroy;
begin
  FinalizeThread;
  FMasterList.Free;
  FSession.Free;
  FEvent.Free;
  inherited;
end;

procedure TQueueSingleton.TThreadController.Execute;
var
  ErrorCount, ErrorSeq: Integer;
  LEvent: TWaitResult;
begin
  ErrorCount := 0;
  ErrorSeq := 0;
  while not Terminated do
  begin
    LEvent := Self.FEvent.WaitFor(TConfig.GetUpdateTime);

    case LEvent of
      wrSignaled:
        begin
          if Assigned(FThread) then
          begin
            if (FThread.Result = trFinished) then
            begin
              ErrorCount := FThread.TotalErrors;
              ErrorSeq := FThread.ConnectionError;

              if ((ErrorSeq * TConfig.GetUpdateTime) >= TConfig.GetMaxJsonPerThread) then
              begin
                TConfig.SetActive(False);
                Terminate;
              end;
            end
            else
              Self.FEvent.SetEvent;
          end;
        end;
      wrTimeout:
        Self.FEvent.SetEvent;
    else
      Terminate;
    end;

    if not ListIsEmpty then
    begin
      FinalizeThread;
      FThread := TSendThread.Create(TConfig.GetUrlElasticAPM);
      FThread.TotalErrors := ErrorCount;
      FThread.ConnectionError := ErrorSeq;
      CloneList(FThread.InternalList);
      FThread.Start;
    end;
  end;
end;

procedure TQueueSingleton.TThreadController.FinalizeThread;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

function TQueueSingleton.TThreadController.ListIsEmpty: Boolean;
begin
  FSession.Enter;
  try
    Result := FMasterList.Count = 0;
  finally
    FSession.Release;
  end;
end;

procedure TQueueSingleton.TThreadController.Add(const AJson, AHeader: string);
var
  DataSend: TDataSend;
begin
  FSession.Enter;
  try
    DataSend.Json := AJson;
    DataSend.Header := AHeader;
    FMasterList.Add(DataSend);
    if FMasterList.Count >= TConfig.GetMaxJsonPerThread then
      Self.FEvent.SetEvent;
  finally
    FSession.Release;
  end;
end;

{ TQueueSingleton }

class procedure TQueueSingleton.Init;
begin
  FSession := TCriticalSection.Create;
end;

class procedure TQueueSingleton.Finalize;
begin
  if Assigned(FThreadMain) then
  begin
    FThreadMain.Terminate;
    FThreadMain.WaitFor;
    FThreadMain.Free;
  end;
  FSession.Free;
end;

class procedure TQueueSingleton.StackUp(const AJson, AHeader: string);
begin
  FSession.Enter;
  try
    if not TConfig.GetActive then
      exit;

    if not Assigned(FThreadMain) then
    begin
      FThreadMain := TThreadController.Create;
      FThreadMain.FreeOnTerminate := False;
      FThreadMain.Start;
    end;
    FThreadMain.Add(AJson, AHeader);
  finally
    FSession.Release;
  end;
end;

initialization

TQueueSingleton.Init;

finalization

TQueueSingleton.Finalize;

end.
