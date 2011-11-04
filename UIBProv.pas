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

unit UIBProv;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses DataProcUtils, ServerProc, SysUtils, Classes, DB, SyncObjs,
{$IFDEF MSWINDOWS}Windows, {$ELSE}DynLibs, {$ENDIF}MD5, StrUtils,
  uibdataset, uib;

type
  TUIBCustomQuery = class(TServerSockQuery)
  private
  public
    StoredProcName: string;
    UIBTransaction: TUIBTransaction;
    procedure Reconnect; override;
    function ExecSQL: integer; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(ParamStr: OnlineDataString): TParam; override;
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

  TUIBSQLQuery = class(TUIBCustomQuery)
  private
    FConnection: TUIBDataBase;
    procedure SetConnection(Value: TUIBDataBase);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ParamByName(ParamStr: OnlineDataString): TParam; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
  published
    property DBConnection: TUIBDataBase read FConnection write SetConnection;
  end;

  TUIBScriptQuery = class(TUIBCustomQuery)
  private
    FConnection: TUIBDataBase;
    procedure SetConnection(Value: TUIBDataBase);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ExecScript: integer; override;
    function ParamByName(ParamStr: OnlineDataString): TParam; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
  published
    property DBConnection: TUIBDataBase read FConnection write SetConnection;
  end;

  TUIBStoredProcQuery = class(TUIBCustomQuery)
  private
    FConnection: TUIBDataBase;
    procedure SetConnection(Value: TUIBDataBase);
  public
    function Open: boolean; override;
    function SuperOpen: boolean; override;
    procedure Close; override;
    function SuperExecSQL: integer; override;
    function ExecSQL: integer; override;
    function ParamByName(ParamStr: OnlineDataString): TParam; override;
    function ExecStoredProc: string; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reconnect; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure RollBack; override;
    procedure CreateDBParam(DataType: TFieldType; ParamName: string; ParamType: TParamType); override;
    procedure ClearParam;
    procedure SetStoredProcValue(FieldName: string; value: string);
    procedure InitializeStoredProc; override;
  published
    property DBConnection: TUIBDataBase read FConnection write SetConnection;
  end;

implementation

constructor TUIBCustomQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UIBTransaction := TUIBTransaction.Create(nil);
end;

destructor TUIBCustomQuery.Destroy;
begin
  UIBTransaction.Free;
  inherited Destroy;
end;

function TUIBCustomQuery.Open: boolean;
begin
  inherited Open;
  Result := False;
end;

function TUIBCustomQuery.SuperOpen: boolean;
begin
  inherited SuperOpen;
  Result := False;
end;

procedure TUIBCustomQuery.Close;
begin
  inherited Close;
end;

function TUIBCustomQuery.ParamByName(ParamStr: OnlineDataString): TParam;
begin

end;

function TUIBCustomQuery.ExecSQL: integer;
begin
  inherited ExecSQL;
  Result := 0;
end;

function TUIBCustomQuery.SuperExecSQL: integer;
begin
  inherited SuperExecSQL;
  Result := 0;
end;

procedure TUIBCustomQuery.Reconnect;
begin

end;

function TUIBCustomQuery.ExecScript: integer;
begin

end;

function TUIBCustomQuery.ExecStoredProc: string;
begin

end;

procedure TUIBCustomQuery.StartTransaction;
begin

end;

procedure TUIBCustomQuery.Commit;
begin

end;

procedure TUIBCustomQuery.RollBack;
begin

end;

procedure TUIBCustomQuery.InitializeStoredProc;
begin
  inherited;
end;

procedure TUIBSQLQuery.SetConnection(Value: TUIBDataBase);
begin
  FConnection := Value;
  TUIBDataSet(NetData).DataBase := Value;
  UIBTransaction.DataBase := Value;
end;

function TUIBSQLQuery.Open: boolean;
begin
  Result := False;
  inherited Open;
  try
    TUIBDataSet(NetData).Close;
    TUIBDataSet(NetData).SQL := Self.SQL;
    TUIBDataSet(NetData).Open;
  except
    if Assigned(FConnection) then
      FConnection.Connected := False;
  end;
  Result := True;
end;

function TUIBSQLQuery.SuperOpen: boolean;
begin
  inherited SuperOpen;
  Result := False;
  try
    TUIBDataSet(NetData).Close;
    TUIBDataSet(NetData).SQL := Self.SQL;
    TUIBDataSet(NetData).Open;
    Result := True;
  except
    if Assigned(FConnection) then
      FConnection.Connected := False;
  end;
end;

procedure TUIBSQLQuery.Close;
begin
  inherited Close;
  TUIBDataSet(NetData).Close;
end;

function TUIBSQLQuery.SuperExecSQL: integer;
begin
  inherited SuperExecSQL;
  Result := 0;
  try
    TUIBDataSet(NetData).SQL := Self.SQL;
    TUIBDataSet(NetData).ExecSQL;
    RowsAffected := TUIBDataSet(NetData).RowsAffected;
  except
    if Assigned(FConnection) then
      FConnection.Connected := False;
  end;
  Result := RowsAffected;
end;

function TUIBSQLQuery.ExecSQL: integer;
var
  TName: OnlineDataString;
begin
  inherited ExecSQL;
  Result := 0;
  try
    TUIBDataSet(NetData).SQL := Self.SQL;
    TUIBDataSet(NetData).ExecSQL;
    RowsAffected := TUIBDataSet(NetData).RowsAffected;
  except
    if Assigned(FConnection) then
      FConnection.Connected := False;
  end;
  Result := RowsAffected;
end;

function TUIBSQLQuery.ParamByName(ParamStr: OnlineDataString): TParam;
begin
  inherited ParamByName(ParamStr);

end;

procedure TUIBSQLQuery.Reconnect;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Connected := False;
    FConnection.Connected := True;
  end;
end;

procedure TUIBSQLQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    UIBTransaction.StartTransaction;
end;

procedure TUIBSQLQuery.Commit;
begin
  if Assigned(FConnection) then
    UIBTransaction.Commit;
end;

procedure TUIBSQLQuery.RollBack;
begin
  if Assigned(FConnection) then
    UIBTransaction.Rollback;
end;

constructor TUIBSQLQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetData := TUIBDataSet.Create(nil);
  TUIBDataSet(NetData).Transaction := UIBTransaction;
end;

destructor TUIBSQLQuery.Destroy;
begin
  NetData.Free;
  inherited;
end;

procedure TUIBScriptQuery.SetConnection(Value: TUIBDataBase);
begin
  FConnection := Value;
  TUIBScript(NetComponent).DataBase := Value;
  UIBTransaction.DataBase := Value;
end;

function TUIBScriptQuery.ExecScript: integer;
begin
  TUIBScript(NetComponent).Script.Assign(Script);
  TUIBScript(NetComponent).ExecuteScript;
  Result := TUIBScript(NetComponent).Script.Count;
end;

function TUIBScriptQuery.Open: boolean;
begin
  Result := True;
end;

function TUIBScriptQuery.SuperOpen: boolean;
begin
  Result := True;
end;

procedure TUIBScriptQuery.Close;
begin

end;

function TUIBScriptQuery.SuperExecSQL: integer;
begin
  Result := 0;
end;

function TUIBScriptQuery.ExecSQL: integer;
begin
  Result := 0;
end;

function TUIBScriptQuery.ParamByName(ParamStr: OnlineDataString): TParam;
begin

end;

procedure TUIBScriptQuery.Reconnect;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Connected := False;
    FConnection.Connected := True;
  end;
end;

procedure TUIBScriptQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    UIBTransaction.StartTransaction;
end;

procedure TUIBScriptQuery.Commit;
begin
  if Assigned(FConnection) then
    UIBTransaction.Commit;
end;

procedure TUIBScriptQuery.RollBack;
begin
  if Assigned(FConnection) then
    UIBTransaction.Rollback;
end;

constructor TUIBScriptQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetComponent := TUIBScript.Create(nil);
  TUIBScript(NetComponent).Transaction := UIBTransaction;
end;

destructor TUIBScriptQuery.Destroy;
begin
  NetComponent.Free;
  inherited;
end;

procedure TUIBStoredProcQuery.SetConnection(Value: TUIBDataBase);
begin
  FConnection := Value;
  TUIBQuery(NetComponent).DataBase := Value;
  UIBTransaction.DataBase := Value;
end;

function TUIBStoredProcQuery.ExecStoredProc: string;
var i: integer;
begin
  TUIBQuery(NetComponent).BuildStoredProc(StoredProcName);
  TUIBQuery(NetComponent).Open;
  Result := '';
  with TUIBQuery(NetComponent).Fields do
    for i := 0 to FieldCount - 1 do
    begin
      Result := Result + (AliasName[i] + ': ' + AsString[i]);
      if (i > 0) and (i < TUIBQuery(NetComponent).Fields.FieldCount - 1) then
        Result := Result + ';';
    end;
  TUIBQuery(NetComponent).Close(etmCommit);
end;

function TUIBStoredProcQuery.Open: boolean;
begin
  Result := True;
end;

function TUIBStoredProcQuery.SuperOpen: boolean;
begin
  Result := True;
end;

procedure TUIBStoredProcQuery.Close;
begin
  inherited Close;
end;

function TUIBStoredProcQuery.SuperExecSQL: integer;
begin
  Result := 0;
end;

function TUIBStoredProcQuery.ExecSQL: integer;
begin
  Result := 0;
end;

function TUIBStoredProcQuery.ParamByName(ParamStr: OnlineDataString): TParam;
begin

end;

procedure TUIBStoredProcQuery.Reconnect;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Connected := False;
    FConnection.Connected := True;
  end;
end;

procedure TUIBStoredProcQuery.StartTransaction;
begin
  if Assigned(FConnection) then
    UIBTransaction.StartTransaction;
end;

procedure TUIBStoredProcQuery.Commit;
begin
  if Assigned(FConnection) then
    UIBTransaction.Commit;
end;

procedure TUIBStoredProcQuery.RollBack;
begin
  if Assigned(FConnection) then
    UIBTransaction.Rollback;
end;

constructor TUIBStoredProcQuery.Create(AOwner: TComponent);
begin
  inherited;
  NetComponent := TUIBQuery.Create(nil);
  //TUIBQuery(NetComponent).Params := Params;
  TUIBQuery(NetComponent).Transaction := UIBTransaction;
end;

destructor TUIBStoredProcQuery.Destroy;
begin
  NetComponent.Free;
  inherited;
end;

procedure TUIBStoredProcQuery.ClearParam;
begin
  //TUIBQuery(NetComponent).Params.Clear;
end;

procedure TUIBStoredProcQuery.CreateDBParam(DataType: TFieldType; ParamName: string; ParamType: TParamType);
begin
  //TUIBQuery(NetComponent).Params.CreateParam(DataType, ParamName, ParamType);
end;

procedure TUIBStoredProcQuery.SetStoredProcValue(FieldName: string; value: string);
begin
  TUIBQuery(NetComponent).Params.ByNameAsString[FieldName] := Value;
end;

procedure TUIBStoredProcQuery.InitializeStoredProc;
begin
  inherited;
  //TUIBQuery(NetComponent).Params.Assign(Params);
  //TUIBQuery(NetComponent).Params := Params;
end;

initialization

finalization

end.

