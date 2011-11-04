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

unit UniDaProv;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses DataProcUtils, ServerProc, SysUtils, Classes, DB, SyncObjs,
{$IFDEF MSWINDOWS}Windows, {$ELSE}DynLibs, {$ENDIF}MD5, StrUtils,
  Uni, MemDS, UniProvider, OracleUniProvider, DAScript, UniScript;

type
  TUniDaCustomQuery = class(TServerSockQuery)
  private
  public
    StoredProcName: string;
    procedure Reconnect; override;
    function ExecSQL: integer; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(ParamStr: DataString): TParam; override;
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecScript: integer; override;
    function ExecStoredProc: string; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
    procedure InitializeStoredProc; override;
  published
  end;

  TUniDaSQLQuery = class(TUniDaCustomQuery)
  private
    FConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ParamByName(ParamStr: DataString): TParam; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
  published
    property DBConnection: TUniConnection read FConnection write SetConnection;
  end;

  TUniDaScriptQuery = class(TUniDaCustomQuery)
  private
    FConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ExecScript: integer; override;
    function ParamByName(ParamStr: DataString): TParam; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
  published
    property DBConnection: TUniConnection read FConnection write SetConnection;
  end;

  TUniDaStoredProcQuery = class(TUniDaCustomQuery)
  private
    FConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ParamByName(ParamStr: DataString): TParam; override;
    function ExecStoredProc: string; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
    procedure CreateDBParam(DataType: TFieldType; ParamName: string; ParamType: TParamType); override;
    procedure ClearParam;
    procedure InitializeStoredProc; override;
  published
    property DBConnection: TUniConnection read FConnection write SetConnection;
  end;

implementation

constructor TUniDaCustomQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TUniDaCustomQuery.Destroy;
begin
  inherited Destroy;
end;

function TUniDaCustomQuery.Open: boolean;
begin
  inherited Open;
  Result := False;
end;

function TUniDaCustomQuery.SuperOpen: boolean;
begin
  inherited SuperOpen;
  Result := False;
end;

procedure TUniDaCustomQuery.Close;
begin
  inherited Close;
end;

function TUniDaCustomQuery.ParamByName(ParamStr: DataString): TParam;
begin

end;

function TUniDaCustomQuery.ExecSQL: integer;
begin
  inherited ExecSQL;
  Result := 0;
end;

function TUniDaCustomQuery.SuperExecSQL: integer;
begin
  inherited SuperExecSQL;
  Result := 0;
end;

procedure TUniDaCustomQuery.Reconnect;
begin

end;

function TUniDaCustomQuery.ExecScript: integer;
begin

end;

function TUniDaCustomQuery.ExecStoredProc: string;
begin

end;

procedure TUniDaCustomQuery.StartTransaction;
begin

end;

procedure TUniDaCustomQuery.Commit;
begin

end;

procedure TUniDaCustomQuery.RollBack;
begin

end;

procedure TUniDaCustomQuery.InitializeStoredProc;
begin
  inherited;
end;

procedure TUniDaSQLQuery.SetConnection(Value: TUniConnection);
begin
  FConnection := Value;
  TUniQuery(NetData).Connection := Value;
end;

function TUniDaSQLQuery.Open: boolean;
begin
  Result := False;
  inherited Open;
  try
    TUniQuery(NetData).Close;
    TUniQuery(NetData).SQL := Self.SQL;
    TUniQuery(NetData).Open;
  except
    if Assigned(FConnection) then
      TUniQuery(NetData).Connection.Disconnect;
  end;
  Result := True;
end;

function TUniDaSQLQuery.SuperOpen: boolean;
begin
  inherited SuperOpen;
  Result := False;
  try
    TUniQuery(NetData).Close;
    TUniQuery(NetData).SQL := Self.SQL;
    TUniQuery(NetData).Open;
    Result := True;
  except
    if Assigned(FConnection) then
      TUniQuery(NetData).Connection.Disconnect;
  end;
end;

procedure TUniDaSQLQuery.Close;
begin
  inherited Close;
  TUniQuery(NetData).Close;
end;

function TUniDaSQLQuery.SuperExecSQL: integer;
begin
  inherited SuperExecSQL;
  Result := 0;
  try
    TUniQuery(NetData).SQL := Self.SQL;
    TUniQuery(NetData).ExecSQL;
    RowsAffected := TUniQuery(NetData).RowsAffected;
  except
    if Assigned(FConnection) then
      TUniQuery(NetData).Connection.Disconnect;
  end;
  Result := RowsAffected;
end;

function TUniDaSQLQuery.ExecSQL: integer;
var
  TName: DataString;
begin
  inherited ExecSQL;
  Result := 0;
  try
    TUniQuery(NetData).SQL := Self.SQL;
    TUniQuery(NetData).ExecSQL;
    RowsAffected := TUniQuery(NetData).RowsAffected;
  except
    if Assigned(FConnection) then
      TUniQuery(NetData).Connection.Disconnect;
  end;
  Result := RowsAffected;
end;

function TUniDaSQLQuery.ParamByName(ParamStr: DataString): TParam;
begin
  inherited ParamByName(ParamStr);
  Result := nil;
  try
    Result := TUniQuery(NetData).ParamByName(ParamStr);
  except
    if Assigned(FConnection) then
      TUniQuery(NetData).Connection.Disconnect;
  end;
end;

procedure TUniDaSQLQuery.Reconnect;
begin
  if Assigned(FConnection) then
    TUniQuery(NetData).Connection.Connected := False;
end;

procedure TUniDaSQLQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    TUniQuery(NetData).Connection.StartTransaction;
end;

procedure TUniDaSQLQuery.Commit;
begin
  if Assigned(FConnection) then
    TUniQuery(NetData).Connection.Commit;
end;

procedure TUniDaSQLQuery.RollBack;
begin
  if Assigned(FConnection) then
    TUniQuery(NetData).Connection.Rollback;
end;

constructor TUniDaSQLQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetData := TUniQuery.Create(nil);
end;

destructor TUniDaSQLQuery.Destroy;
begin
  NetData.Free;
  inherited;
end;

procedure TUniDaScriptQuery.SetConnection(Value: TUniConnection);
begin
  FConnection := Value;
  TUniScript(NetComponent).Connection := Value;
end;

function TUniDaScriptQuery.ExecScript: integer;
begin
  TUniScript(NetComponent).SQL.Assign(Script);
  TUniScript(NetComponent).Execute;
  Result := TUniScript(NetComponent).Params.Count;
end;

function TUniDaScriptQuery.Open: boolean;
begin
  Result := True;
end;

function TUniDaScriptQuery.SuperOpen: boolean;
begin
  Result := True;
end;

procedure TUniDaScriptQuery.Close;
begin

end;

function TUniDaScriptQuery.SuperExecSQL: integer;
begin
  Result := 0;
end;

function TUniDaScriptQuery.ExecSQL: integer;
begin
  Result := 0;
end;

function TUniDaScriptQuery.ParamByName(ParamStr: DataString): TParam;
begin

end;

procedure TUniDaScriptQuery.Reconnect;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Connected := False;
    FConnection.Connect;
  end;
end;

procedure TUniDaScriptQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    TUniScript(NetComponent).Connection.StartTransaction;
end;

procedure TUniDaScriptQuery.Commit;
begin
  if Assigned(FConnection) then
    TUniScript(NetComponent).Connection.Commit;
end;

procedure TUniDaScriptQuery.RollBack;
begin
  if Assigned(FConnection) then
    TUniScript(NetComponent).Connection.Rollback;
end;

constructor TUniDaScriptQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetComponent := TUniScript.Create(nil);
end;

destructor TUniDaScriptQuery.Destroy;
begin
  NetComponent.Free;
  inherited;
end;

procedure TUniDaStoredProcQuery.SetConnection(Value: TUniConnection);
begin
  FConnection := Value;
  TUniStoredProc(NetData).Connection := Value;
end;

function TUniDaStoredProcQuery.ExecStoredProc: string;
var i: integer;
begin
  TUniStoredProc(NetData).StoredProcName := StoredProcName;
  TUniStoredProc(NetData).ExecSQL;
  Result := '';
  for i := 0 to TUniStoredProc(NetComponent).Params.Count - 1 do
  begin
    Result := Result + TUniStoredProc(NetComponent).Params[0].AsString;
    if (i > 0) and (i < TUniStoredProc(NetComponent).Params.Count - 1) then
      Result := Result + ';';
  end;
end;

function TUniDaStoredProcQuery.Open: boolean;
begin
  Result := True;
end;

function TUniDaStoredProcQuery.SuperOpen: boolean;
begin
  Result := True;
end;

procedure TUniDaStoredProcQuery.Close;
begin
  inherited Close;
end;

function TUniDaStoredProcQuery.SuperExecSQL: integer;
begin
  Result := 0;
end;

function TUniDaStoredProcQuery.ExecSQL: integer;
begin
  Result := 0;
end;

function TUniDaStoredProcQuery.ParamByName(ParamStr: DataString): TParam;
begin
  try
    Result := TUniStoredProc(NetData).ParamByName(ParamStr);
  except
    TUniStoredProc(NetData).Connection.Disconnect;
  end;
end;

procedure TUniDaStoredProcQuery.Reconnect;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Connected := False;
    FConnection.Connect;
  end;
end;

procedure TUniDaStoredProcQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    TUniStoredProc(NetData).Connection.StartTransaction;
end;

procedure TUniDaStoredProcQuery.Commit;
begin
  if Assigned(FConnection) then
    TUniStoredProc(NetData).Connection.Commit;
end;

procedure TUniDaStoredProcQuery.RollBack;
begin
  if Assigned(FConnection) then
    TUniStoredProc(NetData).Connection.Rollback;
end;

constructor TUniDaStoredProcQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetData := TUniStoredProc.Create(nil);
  //TUniStoredProc(NetData).Params := Params;
end;

destructor TUniDaStoredProcQuery.Destroy;
begin
  NetData.Free;
  inherited;
end;

procedure TUniDaStoredProcQuery.ClearParam;
begin
  TUniStoredProc(NetData).Params.Clear;
end;

procedure TUniDaStoredProcQuery.CreateDBParam(DataType: TFieldType; ParamName: string; ParamType: TParamType);
begin
  TUniStoredProc(NetData).Params.CreateParam(DataType, ParamName, ParamType);
end;

procedure TUniDaStoredProcQuery.InitializeStoredProc;
begin
  inherited;
  //TUniStoredProc(NetData).Params.Assign(Params);
  //TUniStoredProc(NetData).Params := Params;
end;

initialization

finalization

end.

