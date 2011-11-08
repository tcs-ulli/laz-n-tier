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

unit ZeosDataServer;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  Messages, SysUtils, Classes, ServerProc, Db, SynaSSockets, ZAbstractRODataset,
  ZAbstractDataset, ZDataset, ZConnection, DataProcUtils, ZSqlProcessor,
  ZAbstractConnection, ZStoredProcedure, ZeosProv;

type
  TZeosDataServer = class(TComponent)
  private
    FLocalOnly: Boolean;
    SSocketServer: TSSocketServer;
    QuerySQL: TZeosSQLQuery;
    QueryExec: TZeosSQLQuery;
    ZSQLProc: TZeosScriptQuery;
    ZSQLStored: TZeosStoredProcQuery;
    FAuthRequired: Boolean;
    FDisplayLines: TStrings;
    FZeosDBConnection: TZConnection;
    FActive: Boolean;
    FPort: string;
    FOnCustInternalCall: TOnCustInternalCall;
    FOnUserLogonCall: TOnUserLogonCall;
    procedure SetAuthenticate(Value: Boolean);
    procedure SetDisplayLines(Value: TStrings);
    procedure SetZConnection(FServerConn: TZConnection);
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
    function DoCustInternalCall(CustInstrucx, CustSubInstrucx: Byte; CliParam: PAnsiChar;
      DataQuery: TServerSockQuery; DataSQLProc: TServerSockQuery;
      DataStoredProc: TServerSockQuery; User, SubFunctions: TNetProcString
      ): TNetProcString;
    function DoUserLogOnCall(User, Password: TNetProcString): TLogonStyle;   
    property DisplayLines: TStrings read FDisplayLines write SetDisplayLines;
    property Server: TSSocketServer read SSocketServer write SSocketServer;
  published
    property Active: Boolean read FActive write SetActive;
    property LocalOnly: Boolean read FLocalOnly write FLocalOnly;
    property AuthRequired: Boolean read FAuthRequired write
      SetAuthenticate;
    property ZeosDBConnection: TZConnection read FZeosDBConnection write
      SetZConnection;
    property Port: string read FPort write SetPort;
    property OnCustInternalCall: TOnCustInternalCall read FOnCustInternalCall
      write FOnCustInternalCall;
    property OnUserLogonCall: TOnUserLogonCall read FOnUserLogonCall
      write FOnUserLogonCall;  
  end;

implementation

procedure TZeosDataServer.Display(Msg: string);
begin
  SrvConnBuffer.Log(Msg);
end;

procedure TZeosDataServer.SSocketServerConnect(Sender: TObject;
  FSSock: TSSocketClient; Value: string);
begin
  Display(FSSock.GetRemoteSinIP + ' Connected');
end;

procedure TZeosDataServer.SSocketServerDataAvailable(Sender, ClientThrd: TObject;
  FDSock: TSSocketClient; ReceiveData: string; Error: Word);
var TempInstruc: TInstruction;
begin
  if FLocalOnly then
    if FDSock.GetRemoteSinIP <> '127.0.0.1' then
      Exit;

  TempInstruc := SrvConnBuffer.GetInstruction(ReceiveData);
  if SrvConnBuffer.MustAuthenticate then
    if (not FDSock.LogonStyle) and (TempInstruc <> IstLogin) then
    begin
      FDSock.CloseSocket;
      Exit;
    end;

  if TempInstruc = IstLogin then
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

    if TempInstruc = IstLogin then
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

procedure TZeosDataServer.SSocketServerSocketClose(Sender: TObject;
  FSSock: TSSocketClient; Value: string);
begin
  Display(FSSock.GetRemoteSinIP + ' DisConnected');
end;

constructor TZeosDataServer.Create(aOwner: TComponent);
begin
  inherited;
  SSocketServer := TSSocketServer.Create(Self);
  SrvConnBuffer := TServerConnBuffer.Create(SSocketServer);
  QuerySQL := TZeosSQLQuery.Create(SrvConnBuffer);
  QueryExec := TZeosSQLQuery.Create(SrvConnBuffer);
  ZSQLProc := TZeosScriptQuery.Create(SrvConnBuffer);
  ZSQLStored := TZeosStoredProcQuery.Create(SrvConnBuffer);
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

destructor TZeosDataServer.Destroy;
begin
  ServerLog := nil;
  if QuerySQL.NetData.Active then
    QuerySQL.Close;
  SSocketServer.Close;
  ZeosDBConnection := nil;
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

procedure TZeosDataServer.SetZConnection(FServerConn: TZConnection);
begin
  FZeosDBConnection := FServerConn;
  QuerySQL.DBConnection := FServerConn;
  QueryExec.DBConnection := FServerConn;
  ZSQLProc.DBConnection := FServerConn;
  ZSQLStored.DBConnection := FServerConn;
end;

procedure TZeosDataServer.ListenOnPort(Port: integer);
begin
  SSocketServer.Port := IntToStr(Port);
  SSocketServer.Listen;
  FActive := True;
end;

procedure TZeosDataServer.SetAuthenticate(Value: Boolean);
begin
  FAuthRequired := Value;
  SrvConnBuffer.MustAuthenticate := FAuthRequired;
end;

procedure TZeosDataServer.SetDisplayLines(Value: TStrings);
begin
  ServerLog := Value;
end;

procedure TZeosDataServer.SetActive(Value: Boolean);
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

procedure TZeosDataServer.SetPort(Value: string);
begin
  FPort := Value;
end;

function TZeosDataServer.DoCustInternalCall(CustInstrucx, custSubInstrucx: Byte; CliParam: PAnsiChar;
  DataQuery: TServerSockQuery; DataSQLProc: TServerSockQuery;
  DataStoredProc: TServerSockQuery; User, SubFunctions: TNetProcString
  ): TNetProcString;
begin
  if Assigned(FOnCustInternalCall) then
    Result := FOnCustInternalCall(CustInstrucx, CustSubInstrucx, CliParam, DataQuery,
      DataSQLProc, DataStoredProc, User, SubFunctions);
end;

function TZeosDataServer.DoUserLogOnCall(User, Password: TNetProcString): TLogonStyle;
begin
  Result := PermDenied;
  if Assigned(FOnUserLogOnCall) then
    Result := FOnUserLogOnCall(User, Password);
end;

end.

