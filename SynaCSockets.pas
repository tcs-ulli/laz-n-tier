{==============================================================================|
| Project : OnLine Data Process Component                                      |
|==============================================================================|
| Copyright (c)2010-2011, Yang JiXian                                          |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Yang JiXian nor the names of its contributors may        |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Yang JiXian (PRC).             |
| Portions created by Yang JiXian are Copyright (c)1999-2011.                  |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================}

unit SynaCSockets;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SyncObjs, SysUtils, Blcksock, Synsock, Synautil, SynaIP, SSL_OpenSSL,
  {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, cmem, {$ENDIF}{$ENDIF}
  SynaSockUtils;

type

  TCSocket = class;

  TCSocketClient = class(TCustomSocket)
  public
    DisConnected: boolean;
    procedure Init;
  end;

  TOnDataAvailable = procedure(Sender: TObject; ClientThrd: TObject;
    FDSock: TCSocketClient; ReceiveData: string; Error: word) of object;
  TOnSockStatus = procedure(Sender: TObject; FSSock: TCSocketClient;
    Value: string) of object;
  TOnProgress = procedure(Sender: TObject; FSSock: TCSocketClient;
    FReason: THookSocketReason; Value: integer) of object;
  TOnConnectionChange = procedure(Sender: TObject; TCount: integer) of object;

  TCSocketThread = class(TThread)
  private
    Sock: TCSocketClient;
    FContinue: boolean;
  public
    FJob: TJobs;
    FAOwner: TCSocket;
    FReceiveData, FResponseData: ansistring;
    OnlineRecvStr: ansistring;
    FTOnValue: ansistring;
    FTOnReason: THookSocketReason;
    constructor Create(aOwner: TCSocket);
    destructor Destroy; override;
    procedure Execute; override;
    procedure DoOnDataAvailable;
    procedure DoClose;
    procedure SetOwnerStr;
    procedure ClearOwnerJob;
    procedure GetOwnerJob;
    procedure OwnerOpen;
    procedure OwnerClose;
    procedure SockCallBack(Sender: TObject; Reason: THookSocketReason;
      const Value: string);
    procedure DoOnConnect;
    procedure DoOnDisConnect;
    procedure DoOnProgress;
  end;

  TCSocket = class(TComponent)
  private
    ThrdIsRunning: boolean;
    FThrd: TCSocketThread;
    FWithSSL: boolean;
  public
    FJob: TJobs;
    OnlineContinue: boolean;
    IsRunning: boolean;
    FWHost: ansistring;
    FWPort: ansistring;
    FHostIP: ansistring;
    Active: boolean;
    FTimeOut: integer;
    FRecvStr: ansistring;
    FSocket: TCSocketClient;
    FOnResolvingBegin: TOnSockStatus;
    FOnResolvingEnd: TOnSockStatus;
    FOnSocketCreate: TOnSockStatus;
    FOnSocketClose: TOnSockStatus;
    FOnBind: TOnSockStatus;
    FOnConnect: TOnSockStatus;
    FOnCanRead: TOnSockStatus;
    FOnCanWrite: TOnSockStatus;
    FOnAccept: TOnSockStatus;
    FOnWait: TOnSockStatus;
    FOnSockError: TOnSockStatus;
    FOnDataAvailable: TOnDataAvailable;
    FOnProgress: TOnProgress;
    FReason: THookSocketReason;
    FCS: SyncObjs.TCriticalSection;
    FMaxSendBandwidth, FMaxRecvBandwidth: integer;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function ProcessData(DataStr: ansistring): ansistring;
    function T_ProceData(DataStr: ansistring): ansistring;
    function P_ProceData(DataStr: ansistring): ansistring;
    procedure Connect;
    procedure T_Connect;
    procedure P_Connect;
    procedure Close;
  published
    property WithSSL: boolean read FWithSSL write FWithSSL;
    property TimeOut: integer read FTimeOut write FTimeout;
    property Host: ansistring read FWHost write FWHost;
    property Port: ansistring read FWPort write FWPort;
    property MaxSendBandwidth: integer read FMaxSendBandwidth write FMaxSendBandwidth;
    property MaxRecvBandwidth: integer read FMaxRecvBandwidth write FMaxRecvBandwidth;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnResolvingBegin: TOnSockStatus
      read FOnResolvingBegin write FOnResolvingBegin;
    property OnResolvingEnd: TOnSockStatus read FOnResolvingEnd write FOnResolvingEnd;
    property OnSocketCreate: TOnSockStatus read FOnSocketCreate write FOnSocketCreate;
    property OnSocketClose: TOnSockStatus read FOnSocketClose write FOnSocketClose;
    property OnBind: TOnSockStatus read FOnBind write FOnBind;
    property OnConnect: TOnSockStatus read FOnConnect write FOnConnect;
    property OnCanRead: TOnSockStatus read FOnCanRead write FOnCanRead;
    property OnCanWrite: TOnSockStatus read FOnCanWrite write FOnCanWrite;
    property OnAccept: TOnSockStatus read FOnAccept write FOnAccept;
    property OnWait: TOnSockStatus read FOnWait write FOnWait;
    property OnSockError: TOnSockStatus read FOnSockError write FOnSockError;
    property OnDataAvailable: TOnDataAvailable
      read FOnDataAvailable write FOnDataAvailable;
  end;

{$IFDEF WINDOWS}
{$IFDEF MSWindows}
function GetTickCount: DWORD; stdcall; external 'kernel32.dll' Name 'GetTickCount';
{$ELSE}
function GetTickCount: DWORD; stdcall; external KernelDLL Name 'GetTickCount';
{$ENDIF}
{$ELSE}
function GetTickCount: DWord;
{$ENDIF}

implementation

{==============================================================================}

{$IFNDEF WINDOWS}

function GetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * 24 * 60 * 60 * 1000));
end;

{$ENDIF}

procedure TCSocketClient.Init;
begin
  DisConnected := False;
end;

constructor TCSocketThread.Create(aOwner: TCSocket);
begin
  inherited Create(False);
  FAOwner := aOwner;
  FreeOnTerminate := True;
  FContinue := False;
end;

destructor TCSocketThread.Destroy;
begin
  inherited Destroy;
end;

procedure TCSocketThread.SetOwnerStr;
begin
  FAOwner.FRecvStr := OnlineRecvStr;
end;

procedure TCSocketThread.OwnerOpen;
begin
  FAOwner.ThrdIsRunning := True;
end;

procedure TCSocketThread.OwnerClose;
begin
  FAOwner.ThrdIsRunning := False;
  FAOwner.Active := False;
end;

procedure TCSocketThread.ClearOwnerJob;
begin
  FAOwner.FJob.Job := doNone;
end;

procedure TCSocketThread.GetOwnerJob;
begin
  FJob.Job := FAOwner.FJob.Job;
  FJob.Data := FAOwner.FJob.Data;
end;

procedure TCSocketThread.Execute;
var
  s: ansistring;
begin
  Sock := TCSocketClient.Create;
  Sock.OnStatus := SockCallBack;
  try
    Sock.MaxSendBandwidth := FAOwner.FMaxSendBandwidth;
    Sock.MaxRecvBandwidth := FAOwner.FMaxRecvBandwidth;
    SynChronize(OwnerOpen);

    with Sock do
    begin
      FContinue := True;

      while not Terminated and FContinue do
      begin
        if Terminated then
          Break;

        if LastError <> 0 then
          Break;

        if CanRead(20) then
        begin
          s := RecvOnlineData(FAOwner.TimeOut);
          if s <> '' then
          begin
            FReceiveData := s;
            Synchronize(DoOnDataAvailable);
            if FResponseData <> '' then
              SendOnlineData(FResponseData);
          end;
        end;

        Synchronize(GetOwnerJob);

        if FJob.Job <> doNone then
        begin
          if FJob.Job = doConn then
          begin
            if Terminated then
              Break;
            Connect(FAOwner.FHostIP, FAOwner.FWPort);
            if LastError <> 0 then
              Synchronize(DoClose)
            else
              FAOwner.Active := True;

            if FAOwner.WithSSL then
            begin
              Sock.SSL.SSLType := LT_ALL;
              sock.SSLDoConnect;
              if sock.lasterror <> 0 then
                Break;
            end;

          end;

          if FJob.Job = doDisconn then
          begin
            CloseSocket;
            FContinue := False;
            SynChronize(ClearOwnerJob);
            SynChronize(OwnerClose);
            FJob.Job := doNone;
            Terminate;
          end;

          try
            if FJob.Job = doSend then
            begin
              if Terminated then
                Break;
              SendString(FJob.Data);
              OnlineRecvStr := '';
              SynChronize(SetOwnerStr);
              if CanRead(FAOwner.TimeOut) then
              begin

                s := RecvOnlineData(FAOwner.TimeOut);

                OnlineRecvStr := s;
                SynChronize(SetOwnerStr);
              end;
            end;
          except
          end;

          Sleep(0);
          SynChronize(ClearOwnerJob);
        end;
      end;
    end;
    SynChronize(OwnerClose);
  finally
    if FAOwner.WithSSL then
      Sock.SSLDoShutdown;
    Sock.Free;
  end;
  FAOwner.Active := False;
end;

procedure TCSocketThread.DoClose;
begin
  FAOwner.FJob.Job := doNone;
  if Assigned(FAOwner.FOnSocketClose) then
    FAOwner.FOnSocketClose(FAOwner, Sock, '');
  FAOwner.Active := False;
  FContinue := False;
  FAOwner.FJob.Job := doNone;
  FJob.Job := doNone;
  FAOwner.ThrdIsRunning := False;
  Terminate;
end;

procedure TCSocketThread.DoOnDataAvailable;
begin
  if assigned(FAOwner.FOnDataAvailable) then
    FAOwner.FOnDataAvailable(FAOwner, Self, Sock, FReceiveData, Sock.LastError);
end;

procedure TCSocketThread.DoOnConnect;
begin
  FAOwner.FOnConnect(FAOwner, Sock, FTOnValue);
end;

procedure TCSocketThread.DoOnDisConnect;
begin
  FAOwner.FOnSocketClose(FAOwner, Sock, FTOnValue);
end;

procedure TCSocketThread.DoOnProgress;
begin
  FAOwner.FOnProgress(FAOwner, Sock, FTOnReason, StrToIntDef(FTOnValue, 0));
end;

procedure TCSocketThread.SockCallBack(Sender: TObject; Reason: THookSocketReason;
  const Value: string);
begin
  if not terminated then
    try
      FTOnReason := Reason;
      FTOnVaLUE := Value;
      if Assigned(FAOwner.FOnProgress) then
        SynChronize(DoOnProgress);
      case Reason of
        HR_ResolvingBegin:
          if Assigned(FAOwner.FOnResolvingBegin) then
            FAOwner.FOnResolvingBegin(FAOwner, Sock, Value);
        HR_ResolvingEnd:
          if Assigned(FAOwner.FOnResolvingEnd) then
            FAOwner.FOnResolvingEnd(FAOwner, Sock, Value);
        HR_SocketCreate:
          if Assigned(FAOwner.FOnSocketCreate) then
            FAOwner.FOnSocketCreate(FAOwner, Sock, Value);
        HR_SocketClose:
        begin
          if Assigned(FAOwner.FOnSocketClose) then
            SynChronize(DoOnDisConnect);
          FContinue := False;
          FJob.Job := doNone;
          SynChronize(OwnerClose);
          SynChronize(ClearOwnerJob);
          Terminate;
        end;
        HR_Bind:
          if Assigned(FAOwner.FOnBind) then
            FAOwner.FOnBind(FAOwner, Sock, Value);
        HR_Connect:
        begin
          if Assigned(FAOwner.FOnConnect) then
            SynChronize(DoOnConnect);
          SynChronize(OwnerOpen);
        end;
        HR_CanRead:
          if Assigned(FAOwner.FOnCanRead) then
            FAOwner.FOnCanRead(FAOwner, Sock, Value);
        HR_CanWrite:
          if Assigned(FAOwner.FOnCanWrite) then
            FAOwner.FOnCanWrite(FAOwner, Sock, Value);
        HR_Accept:
          if Assigned(FAOwner.FOnAccept) then
            FAOwner.FOnAccept(FAOwner, Sock, Value);
        HR_Wait:
          if Assigned(FAOwner.FOnWait) then
            FAOwner.FOnWait(FAOwner, Sock, Value);
        HR_Error:
          if Assigned(FAOwner.FOnSockError) then
            FAOwner.FOnSockError(FAOwner, Sock, Value);
      end;
    except
    end;
end;

constructor TCSocket.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCS := SyncObjs.TCriticalSection.Create;
  FSocket := TCSocketClient.Create;
  Active := False;
  FTimeOut := 12000;
  FMaxSendBandwidth := 0;
  FMaxRecvBandwidth := 0;
  ThrdIsRunning := False;
  IsRunning := False;
  FWithSSL := False;
end;

destructor TCSocket.Destroy;
begin
  Close;
  FSocket.Free;

  try
    if Active then
    begin
      FThrd.Terminate;
      OnlineContinue := False;
    end;
  except
  end;

  FCS.Free;
  inherited Destroy;
end;

function TCSocket.T_ProceData(DataStr: ansistring): ansistring;
begin
  Result := '';
  if not IsRunning then
    try
      IsRunning := True;
      if Active and ThrdIsRunning then
      begin
        FJob.Job := doSend;
        FJob.Data := DataStr;
        while (FJob.Job <> doNone) and OnlineContinue do
          Sleep(0);
        Result := FRecvStr;
      end;
    finally
      IsRunning := False;
    end;
end;

function TCSocket.ProcessData(DataStr: ansistring): ansistring;
begin
  if (csDesigning in ComponentState) then
    Result := P_ProceData(DataStr)
  else
    Result := T_ProceData(DataStr);
end;

function TCSocket.P_ProceData(DataStr: ansistring): ansistring;
var
  Buf: ansistring;
begin
  Result := '';
  FSocket.SendString(DataStr);
  if FSocket.LastError <> 0 then
  begin
    Result := 'ERROR' + IntToStr(FSocket.LastError);
    Exit;
  end;
  Buf := FSocket.RecvPacket(FTimeOut);
  Result := Result + Buf;
  repeat
    Buf := FSocket.RecvPacket(6);
    if FSocket.LastError <> 0 then
      Break;
    if Buf = '' then
      Break;
    Result := Result + Buf;
  until (Buf = '') or (FSocket.LastError <> 0);
end;

procedure TCSocket.Close;
begin
  FJob.Job := doDisConn;
  OnlineContinue := False;
  if (csDesigning in ComponentState) then
    FSocket.CloseSocket;
end;

procedure TCSocket.T_Connect;
var
  xct, i: integer;
begin
  if (not Active) and (not ThrdIsRunning) then
  begin
    FThrd := TCSocketThread.Create(Self);
    FJob.Job := doConn;
    OnlineContinue := True;
    xct := GetTickCount;
    while (FJob.Job <> doNone) and OnlineContinue do
    begin
      Sleep(1);
      i := GetTickCount - xct;
      if i > 62000 then
        OnlineContinue := False;
    end;
  end;
end;

procedure TCSocket.Connect;
begin
  if not IsIP(FWHost) then
    FHostIP := FSocket.ResolveName(FWHost)
  else
    FHostIP := FWHost;

  if (csDesigning in ComponentState) then
    P_Connect
  else
    T_Connect;
end;

procedure TCSocket.P_Connect;
begin
  try
    if not Active then
    begin
      try
        FSocket.Connect(FHostIP, FWPort);
      except
        FSocket.CloseSocket;
      end;
    end;
    if FSocket.LastError = 0 then
      Active := True
    else
      Active := False;
  except
  end;
end;

end.

