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

unit ZeosProv;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses DataProcUtils, ServerProc, SysUtils, Classes, DB, ZAbstractTable, ZDataset,
  ZConnection, ZAbstractRODataset, ZAbstractDataset, ZStoredProcedure, SyncObjs,
{$IFDEF MSWINDOWS}Windows, {$ELSE}DynLibs,
{$ENDIF}MD5, ZSqlProcessor, StrUtils;

type
  TZeosConnection = class(TZConnection)
  end;

  TZeosCustomQuery = class(TServerSockQuery)
  private
  public
    StoredProcName: string;
    procedure Reconnect; override;
    function ExecSQL: integer; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(ParamStr: AnsiNetProcString): TParam; override;
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

  TZeosSQLQuery = class(TZeosCustomQuery)
  private
    FConnection: TZConnection;
    procedure SetConnection(Value: TZConnection);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ParamByName(ParamStr: AnsiNetProcString): TParam; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
  published
    property DBConnection: TZConnection read FConnection write SetConnection;
  end;

  TZeosScriptQuery = class(TZeosCustomQuery)
  private
    FConnection: TZConnection;
    procedure SetConnection(Value: TZConnection);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ExecScript: integer; override;
    function ParamByName(ParamStr: AnsiNetProcString): TParam; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
  published
    property DBConnection: TZConnection read FConnection write SetConnection;
  end;

  TZeosStoredProcQuery = class(TZeosCustomQuery)
  private
    FConnection: TZConnection;
    procedure SetConnection(Value: TZConnection);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ParamByName(ParamStr: AnsiNetProcString): TParam; override;
    function ExecStoredProc: string; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
    procedure InitializeStoredProc; override;
    procedure CreateDBParam(DataType: TFieldType; ParamName: string;
      ParamType: TParamType); override;
    procedure ClearParam;
  published
    property DBConnection: TZConnection read FConnection write SetConnection;
  end;

implementation

constructor TZeosCustomQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TZeosCustomQuery.Destroy;
begin
  inherited Destroy;
end;

function TZeosCustomQuery.Open: boolean;
begin
  inherited Open;
  Result := False;
end;

function TZeosCustomQuery.SuperOpen: boolean;
begin
  inherited SuperOpen;
  Result := False;
end;

procedure TZeosCustomQuery.Close;
begin
  inherited Close;
end;

function TZeosCustomQuery.ParamByName(ParamStr: AnsiNetProcString): TParam;
begin

end;

function TZeosCustomQuery.ExecSQL: integer;
begin
  inherited ExecSQL;
  Result := 0;
end;

function TZeosCustomQuery.SuperExecSQL: integer;
begin
  inherited SuperExecSQL;
  Result := 0;
end;

procedure TZeosCustomQuery.Reconnect;
begin

end;

function TZeosCustomQuery.ExecScript: integer;
begin

end;

function TZeosCustomQuery.ExecStoredProc: string;
begin

end;

procedure TZeosCustomQuery.StartTransaction;
begin

end;

procedure TZeosCustomQuery.Commit;
begin

end;

procedure TZeosCustomQuery.RollBack;
begin

end;

procedure TZeosCustomQuery.InitializeStoredProc;
begin
  inherited;
end;

procedure TZeosSQLQuery.SetConnection(Value: TZConnection);
begin
  FConnection := Value;
  TZQuery(NetData).Connection := Value;
end;

function TZeosSQLQuery.Open: boolean;
begin
  Result := False;
  inherited Open;

  TZQuery(NetData).Close;
  TZQuery(NetData).SQL := Self.SQL;
  TZQuery(NetData).Open;
  Result := True;
end;

function TZeosSQLQuery.SuperOpen: boolean;
begin
  inherited SuperOpen;
  Result := False;
  TZQuery(NetData).Close;
  TZQuery(NetData).SQL := Self.SQL;
  TZQuery(NetData).Open;
  Result := True;
end;

procedure TZeosSQLQuery.Close;
begin
  inherited Close;
  TZQuery(NetData).Close;
end;

function TZeosSQLQuery.SuperExecSQL: integer;
begin
  inherited SuperExecSQL;
  Result := 0;
  TZQuery(NetData).SQL := Self.SQL;
  TZQuery(NetData).ExecSQL;
  RowsAffected := TZQuery(NetData).RowsAffected;
  Result := RowsAffected;
end;

function TZeosSQLQuery.ExecSQL: integer;
var
  TName: AnsiNetProcString;
begin
  inherited ExecSQL;
  Result := 0;
  TZQuery(NetData).SQL := Self.SQL;
  TZQuery(NetData).ExecSQL;
  RowsAffected := TZQuery(NetData).RowsAffected;
  Result := RowsAffected;
end;

function TZeosSQLQuery.ParamByName(ParamStr: AnsiNetProcString): TParam;
begin
  inherited ParamByName(ParamStr);
  Result := nil;
  Result := TZQuery(NetData).ParamByName(ParamStr);
end;

procedure TZeosSQLQuery.Reconnect;
begin
  if Assigned(FConnection) then
    TZQuery(NetData).Connection.Reconnect;
end;

procedure TZeosSQLQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    TZQuery(NetData).Connection.StartTransaction;
end;

procedure TZeosSQLQuery.Commit;
begin
  if Assigned(FConnection) then
    TZQuery(NetData).Connection.Commit;
end;

procedure TZeosSQLQuery.RollBack;
begin
  if Assigned(FConnection) then
    TZQuery(NetData).Connection.Rollback;
end;

constructor TZeosSQLQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetData := TZQuery.Create(nil);
end;

destructor TZeosSQLQuery.Destroy;
begin
  NetData.Free;
  inherited;
end;

procedure TZeosScriptQuery.SetConnection(Value: TZConnection);
begin
  FConnection := Value;
  TZSQLProcessor(NetComponent).Connection := Value;
end;

function TZeosScriptQuery.ExecScript: integer;
begin
  TZSQLProcessor(NetComponent).Script.Assign(Script);
  TZSQLProcessor(NetComponent).Execute;
  Result := TZSQLProcessor(NetComponent).StatementCount;
end;

function TZeosScriptQuery.Open: boolean;
begin
  Result := True;
end;

function TZeosScriptQuery.SuperOpen: boolean;
begin
  Result := True;
end;

procedure TZeosScriptQuery.Close;
begin

end;

function TZeosScriptQuery.SuperExecSQL: integer;
begin
  Result := 0;
end;

function TZeosScriptQuery.ExecSQL: integer;
begin
  Result := 0;
end;

function TZeosScriptQuery.ParamByName(ParamStr: AnsiNetProcString): TParam;
begin
  Result := TZSQLProcessor(NetComponent).ParamByName(ParamStr);
end;

procedure TZeosScriptQuery.Reconnect;
begin
  if Assigned(FConnection) then
    TZSQLProcessor(NetComponent).Connection.Reconnect;
end;

procedure TZeosScriptQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    TZSQLProcessor(NetComponent).Connection.StartTransaction;
end;

procedure TZeosScriptQuery.Commit;
begin
  if Assigned(FConnection) then
    TZSQLProcessor(NetComponent).Connection.Commit;
end;

procedure TZeosScriptQuery.RollBack;
begin
  if Assigned(FConnection) then
    TZSQLProcessor(NetComponent).Connection.Rollback;
end;

constructor TZeosScriptQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetComponent := TZSQLProcessor.Create(nil);
end;

destructor TZeosScriptQuery.Destroy;
begin
  NetComponent.Free;
  inherited;
end;

procedure TZeosStoredProcQuery.SetConnection(Value: TZConnection);
begin
  FConnection := Value;
  TZStoredProc(NetData).Connection := Value;
end;

function TZeosStoredProcQuery.ExecStoredProc: string;
var
  i: integer;
begin
  TZStoredProc(NetData).StoredProcName := StoredProcName;
  TZStoredProc(NetData).ExecSQL;
  Result := '';
  for i := 0 to TZStoredProc(NetComponent).Params.Count - 1 do
  begin
    Result := Result + TZStoredProc(NetComponent).Params[0].AsString;
    if (i > 0) and (i < TZStoredProc(NetComponent).Params.Count - 1) then
      Result := Result + ';';
  end;
end;

function TZeosStoredProcQuery.Open: boolean;
begin
  Result := True;
end;

function TZeosStoredProcQuery.SuperOpen: boolean;
begin
  Result := True;
end;

procedure TZeosStoredProcQuery.Close;
begin
  inherited Close;
end;

function TZeosStoredProcQuery.SuperExecSQL: integer;
begin
  Result := 0;
end;

function TZeosStoredProcQuery.ExecSQL: integer;
begin
  Result := 0;
end;

function TZeosStoredProcQuery.ParamByName(ParamStr: AnsiNetProcString): TParam;
begin
  try
    Result := TZStoredProc(NetData).ParamByName(ParamStr);
  except
    TZStoredProc(NetData).Connection.Disconnect;
  end;
end;

procedure TZeosStoredProcQuery.Reconnect;
begin
  if Assigned(FConnection) then
    TZStoredProc(NetData).Connection.Reconnect;
end;

procedure TZeosStoredProcQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    TZStoredProc(NetData).Connection.StartTransaction;
end;

procedure TZeosStoredProcQuery.Commit;
begin
  if Assigned(FConnection) then
    TZStoredProc(NetData).Connection.Commit;
end;

procedure TZeosStoredProcQuery.RollBack;
begin
  if Assigned(FConnection) then
    TZStoredProc(NetData).Connection.Rollback;
end;

constructor TZeosStoredProcQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetData := TZStoredProc.Create(nil);
  TZStoredProc(NetData).Params := Params;
end;

destructor TZeosStoredProcQuery.Destroy;
begin
  NetData.Free;
  inherited;
end;

procedure TZeosStoredProcQuery.ClearParam;
begin
  TZStoredProc(NetData).Params.Clear;
end;

procedure TZeosStoredProcQuery.CreateDBParam(DataType: TFieldType;
  ParamName: string; ParamType: TParamType);
begin
  TZStoredProc(NetData).Params.CreateParam(DataType, ParamName, ParamType);
end;

procedure TZeosStoredProcQuery.InitializeStoredProc;
begin
  inherited;
  //TZStoredProc(NetData).Params.Assign(Params);
  //TZStoredProc(NetData).Params := Params;
end;

initialization

finalization

end.

