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

unit UniDataServer;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, ServerProc, Db, SynaSSockets,
  DataProcUtils, UniDaProv, Uni, MemDS, UniProvider, OracleUniProvider, DAScript,
  UniScript;

type
  TUniDataServer = class(TComponent)
  private
    FLocalOnly: Boolean;
    SSocketServer: TSSocketServer;
    QuerySQL: TUniDaSQLQuery;
    QueryExec: TUniDaSQLQuery;
    ZSQLProc: TUniDaScriptQuery;
    ZSQLStored: TUniDaStoredProcQuery;
    FAuthRequired: Boolean;
    FDisplayLines: TStrings;
    FUniDBConnection: TUniConnection;
    FActive: Boolean;
    FPort: string;
    FOnCustInternalCall: TOnCustInternalCall;
    FOnUserLogonCall: TOnUserLogonCall;
    procedure SetAuthenticate(Value: Boolean);
    procedure SetDisplayLines(Value: TStrings);
    procedure SetUniConnection(FServerConn: TUniConnection);
    procedure SetActive(Value: Boolean);
    procedure SetPort(Value: string);
    procedure SSocketServerSocketClose(Sender: TObject; FSSock: TSSocketClient;
      Value: string);
    procedure SSocketServerConnect(Sender: TObject; FSSock: TSSocketClient;
      Value: string);
    procedure SSocketServerDataAvailable(Sender, ClientThrd: TObject;
      FDSock: TSSocketClient; ReceiveData: string; Error: Word);
    procedure Display(Msg: string);
  public
    SrvConnBuffer: TServerConnBuffer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ListenOnPort(Port: integer);
    function DoCustInternalCall(CustInstrucx, CustSubInstrucx: Byte; CliParam: PChar;
      DataQuery: TServerSockQuery; DataSQLProc: TServerSockQuery;
      DataStoredProc: TServerSockQuery; User, SubFunctions: DataString
      ): DataString;
    function DoUserLogOnCall(User, Password: DataString): TLogonStyle;   
    property DisplayLines: TStrings read FDisplayLines write SetDisplayLines;
  published
    property Active: Boolean read FActive write SetActive;
    property LocalOnly: Boolean read FLocalOnly write FLocalOnly;
    property AuthRequired: Boolean read FAuthRequired write
      SetAuthenticate;
    property UniDBConnection: TUniConnection read FUniDBConnection write
      SetUniConnection;
    property Port: string read FPort write SetPort;
    property Server: TSSocketServer read SSocketServer write SSocketServer;
    property OnCustInternalCall: TOnCustInternalCall read FOnCustInternalCall
      write FOnCustInternalCall;
  end;

implementation

procedure TUniDataServer.Display(Msg: string);
begin
  SrvConnBuffer.Log(Msg);
end;

procedure TUniDataServer.SSocketServerConnect(Sender: TObject;
  FSSock: TSSocketClient; Value: string);
begin
  Display(FSSock.GetRemoteSinIP + ' Connected');
end;

procedure TUniDataServer.SSocketServerDataAvailable(Sender, ClientThrd: TObject;
  FDSock: TSSocketClient; ReceiveData: string; Error: Word);
var TempInstruc: TInstruction;
begin
  if FLocalOnly then
    if FDSock.GetRemoteSinIP <> '127.0.0.1' then
      Exit;

  TempInstruc := SrvConnBuffer.GetInstruction(ReceiveData);
  if SrvConnBuffer.MustAuthenticate then
    if (not FDSock.LogonStyle) and (TempInstruc <> InstrucLogin) then
    begin
      FDSock.CloseSocket;
      Exit;
    end;

  if TempInstruc = InstrucLogin then
  begin
    SrvConnBuffer.LogonStyle := False;
    SrvConnBuffer.FUSR := '';
    SrvConnBuffer.TempClientFunctions := '';
    SrvConnBuffer.TempClientPermTables := '';
    SrvConnBuffer.TempClientReadTables := '';
    SrvConnBuffer.TempORGID1 := '';
    SrvConnBuffer.TempORGID2 := '';
    SrvConnBuffer.TempSubFuncs := '';
  end
  else
  begin
    SrvConnBuffer.LogonStyle := FDSock.LogonStyle;
    SrvConnBuffer.FUSR := FDSock.USRID;
    SrvConnBuffer.TempClientFunctions := FDSock.ClientFunctions;
    SrvConnBuffer.TempClientPermTables := FDSock.ClientPermTables;
    SrvConnBuffer.TempClientReadTables := FDSock.ClientReadTables;
    SrvConnBuffer.TempORGID1 := FDSock.ORGID1;
    SrvConnBuffer.TempORGID2 := FDSock.ORGID2;
    SrvConnBuffer.TempSubFuncs := FDSock.ClientSubFuncs;
  end;

  if ReceiveData <> '' then
  begin
    Display(FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
    SrvConnBuffer.RecvBuffer := ReceiveData;
    try
      SrvConnBuffer.ProcessReadData;
    except

    end;

    if TempInstruc = InstrucLogin then
    begin
      FDSock.LogonStyle := SrvConnBuffer.LogonStyle;
      FDSock.USRID := SrvConnBuffer.FUSR;
      FDSock.ClientFunctions := SrvConnBuffer.TempClientFunctions;
      FDSock.ClientPermTables := SrvConnBuffer.TempClientPermTables;
      FDSock.ClientReadTables := SrvConnBuffer.TempClientReadTables;
      FDSock.ORGID1 := SrvConnBuffer.TempORGID1;
      FDSock.ORGID2 := SrvConnBuffer.TempORGID2;
      FDSock.ClientSubFuncs := SrvConnBuffer.TempSubFuncs;
    end;
    if SrvConnBuffer.SendBuffer <> '' then
      TCliThread(ClientThrd).ResponseData := SrvConnBuffer.SendBuffer;
    Display(FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
    Display('Data Length:' + IntToStr(Length(SrvConnBuffer.SendBuffer)));
  end;
end;

procedure TUniDataServer.SSocketServerSocketClose(Sender: TObject;
  FSSock: TSSocketClient; Value: string);
begin
  Display(FSSock.GetRemoteSinIP + ' DisConnected');
end;

constructor TUniDataServer.Create(aOwner: TComponent);
begin
  inherited;
  SSocketServer := TSSocketServer.Create(Self);
  SrvConnBuffer := TServerConnBuffer.Create(SSocketServer);
  QuerySQL := TUniDaSQLQuery.Create(SrvConnBuffer);
  QueryExec := TUniDaSQLQuery.Create(SrvConnBuffer);
  ZSQLProc := TUniDaScriptQuery.Create(SrvConnBuffer);
  ZSQLStored := TUniDaStoredProcQuery.Create(SrvConnBuffer);
  SrvConnBuffer.OnCustInternalCall := DoCustInternalCall;
  SrvConnBuffer.OnUserLogonCall := DoUserLogOnCall;
  SSocketServer.OnSocketClose := SSocketServerSocketClose;
  SSocketServer.OnConnect := SSocketServerConnect;
  SSocketServer.OnDataAvailable := SSocketServerDataAvailable;
  Display('Starting Database Engine...');
  Display('Waiting for clients...');
  SrvConnBuffer.NetQuery := QuerySQL;
  SrvConnBuffer.NetSQLRun := QueryExec;
  SrvConnBuffer.NetSQLProc := ZSQLProc;
  SrvConnBuffer.NetStoredProc := ZSQLStored;
  FLocalOnly := False;
  SrvConnBuffer.MustAuthenticate := False;
  FPort := '8080';
end;

destructor TUniDataServer.Destroy;
begin
  ServerLog := nil;
  if QuerySQL.NetData.Active then
    QuerySQL.Close;
  if FUniDBConnection <> nil then
    FUniDBConnection.Disconnect;
  SSocketServer.Close;
  UniDBConnection := nil;
  SrvConnBuffer.NetQuery := nil;
  SrvConnBuffer.NetSQLRun := nil;
  SrvConnBuffer.NetSQLProc := nil;
  SrvConnBuffer.NetStoredProc := nil;
  QuerySQL.Free;
  QueryExec.Free;
  ZSQLProc.Free;
  ZSQLStored.Free;
  SrvConnBuffer.Free;
  SSocketServer.Free;
  inherited;
end;

procedure TUniDataServer.SeTUniConnection(FServerConn: TUniConnection);
begin
  FUniDBConnection := FServerConn;
  QuerySQL.DBConnection := FServerConn;
  QueryExec.DBConnection := FServerConn;
  ZSQLProc.DBConnection := FServerConn;
  ZSQLStored.DBConnection := FServerConn;
end;

procedure TUniDataServer.ListenOnPort(Port: integer);
begin
  SSocketServer.Port := IntToStr(Port);
  SSocketServer.Listen;
  FActive := True;
end;

procedure TUniDataServer.SetAuthenticate(Value: Boolean);
begin
  FAuthRequired := Value;
  SrvConnBuffer.MustAuthenticate := FAuthRequired;
end;

procedure TUniDataServer.SetDisplayLines(Value: TStrings);
begin
  ServerLog := Value;
end;

procedure TUniDataServer.SetActive(Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    if FActive then
    begin
      SSocketServer.Port := FPort;
      SSocketServer.Listen;
    end
    else
      SSocketServer.Close;
end;

procedure TUniDataServer.SetPort(Value: string);
begin
  FPort := Value;
  if Active and not (csDesigning in ComponentState) then
  begin
    SSocketServer.Close;
    SSocketServer.Port := Value;
    SSocketServer.Listen;
  end
  else
    SSocketServer.Port := Value;
end;

function TUniDataServer.DoCustInternalCall(CustInstrucx, custSubInstrucx: Byte; CliParam: PChar;
  DataQuery: TServerSockQuery; DataSQLProc: TServerSockQuery;
  DataStoredProc: TServerSockQuery; User, SubFunctions: DataString
  ): DataString;
begin
  if Assigned(FOnCustInternalCall) then
    Result := FOnCustInternalCall(CustInstrucx, CustSubInstrucx, CliParam, DataQuery,
      DataSQLProc, DataStoredProc, User, SubFunctions);
end;

function TUniDataServer.DoUserLogOnCall(User: string; Password: string): TLogonStyle;
begin
  if Assigned(FOnUserLogOnCall) then
    Result := FOnUserLogOnCall(User, Password);
end;

end.

