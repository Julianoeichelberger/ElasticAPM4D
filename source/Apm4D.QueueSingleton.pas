{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.QueueSingleton;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs, Vcl.ExtCtrls,
  Apm4D.SendThread, Apm4D.Settings;

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
    LEvent := Self.FEvent.WaitFor(TApm4DSettings.Elastic.UpdateTime);

    case LEvent of
      wrSignaled:
        begin
          if Assigned(FThread) then
          begin
            if (FThread.Result = trFinished) then
            begin
              ErrorCount := FThread.TotalErrors;
              ErrorSeq := FThread.ConnectionError;

              if ((ErrorSeq * TApm4DSettings.Elastic.UpdateTime) >=
                TApm4DSettings.Elastic.MaxJsonPerThread) then
              begin
                TApm4DSettings.Deactivate;
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
      FThread := TSendThread.Create(TApm4DSettings.Elastic.Url, TApm4DSettings.Elastic.Secret);
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
    if FMasterList.Count >= TApm4DSettings.Elastic.MaxJsonPerThread then
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
    if not TApm4DSettings.IsActive then
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
