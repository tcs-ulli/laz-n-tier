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


unit OnLineQuery;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  MemDataBase, Variants, NetConnection,
{$IFDEF FPC}
  //  LCLIntf, LCLType, LMessages,
{$ELSE}Windows, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, DB, SynaCSockets, ClientProc,
  DataProcUtils;

type
  TOnNetProcListChange = procedure() of object;

  TNetProcList = class(TStringList)
  private
    FOnNetProcListChange: TOnNetProcListChange;
  protected
  public
    function Add(const S: string): integer; override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure Exchange(Index1, Index2: integer); override;
    procedure Insert(Index: integer; const S: string); override;
    property OnNetProcListChange: TOnNetProcListChange
      read FOnNetProcListChange write FOnNetProcListChange;
  end;

  TOnlineQuery = class(TMemDB)
  private
    FOnlineConn: TOnlineConnection;
    FTableName, FPrimaryKey, FIndexFieldNames, FEditFields: TNetProcString;
    FSync, FLoading: boolean;
    FSQL, FSQLDataStr: TNetProcList;
    FReplaceFields: boolean;
    TmpR, FThinQuery: boolean;
    FCachedUpdate: boolean;
    FInstrucNum: TInstruction;
    FSubInstrucNum: byte;
    FClientParam: TNetProcString;
    procedure SetInstrucNum(Value: TInstruction);
    procedure DestroyQuerys;
    procedure OnLinePepare(SQL: TNetProcString; SQLInstruction: TSQLInstruction);
    function OnLineRun: integer;
    procedure SetIndexFieldNames(const Value: TNetProcString);
    procedure SetTableName(const Value: TNetProcString);
    procedure SetPrimaryKey(const Value: TNetProcString);
    procedure SetSync(Value: boolean);
    function GetSQL: TNetProcList;
    procedure SetSQL(Value: TNetProcList);
    function GetActive: boolean;
    procedure SetActive(Value: boolean); override;
    procedure SQLStringChange;
  protected
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
  public
    CacheList: TNetProcList;
    RetValue: TNetProcString;
    function ApplyCacheUpdate: boolean;
    function OnlineRequest(IsSQLOpen: boolean; xInstruc: integer;
      xParam: TNetProcString): integer;
    function InternalSQL(SubInstruc: integer; xParam: TNetProcString): integer;
    function OnlineScript(xInstruc: integer; xParam: TNetProcString): integer;
    function OnlineStoredProc(xInstruc: integer; xParam: TNetProcString): integer;
    function OnlineClientProcess(xInstruc: integer;
      xParam: TNetProcString): TNetProcString;
    function InternalProcess(xInstruc: integer; xParam: TNetProcString): TNetProcString;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Sync: boolean read FSync write SetSync;
    procedure GenerateInsertData;
    procedure GenerateUpdateData;
    procedure GenerateDeleteData;
    function ComputePrimaryKeyForSQLData(OldValues, NullValues: boolean): TNetProcString;
    procedure ReOpenTable;
    function GetServerTime: TNetProcString;
    procedure Open;
    property ThinQuery: boolean read FThinQuery write FThinQuery;
    property TableName: TNetProcString read FTableName write SetTableName;
  published
    property InstrucNum: TInstruction read FInstrucNum write SetInstrucNum;
    property InstrucSubNum: byte read FSubInstrucNum write FSubInstrucNum;
    property ClientParam: TNetProcString read FClientParam write FClientParam;
    property CachedUpdate: boolean read FCachedUpdate write FCachedUpdate;
    property OnlineConn: TOnlineConnection read FOnlineConn write FOnlineConn;
    property PrimaryKey: TNetProcString read FPrimaryKey write SetPrimaryKey;
    property SQL: TNetProcList read GetSQL write SetSQL;
    property GetFields: boolean read FReplaceFields write FReplaceFields;
    property Active: boolean read GetActive write SetActive;
    property Filtered;
    property Filter;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  TClientDataset = class(TOnlineQuery)
  published
    property InstrucNum;
    property ClientParam;
    property CachedUpdate;
    property OnlineConn;
    property PrimaryKey;
    property SQL;
    property GetFields;
    property Active;
    property Filtered;
    property Filter;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation

function TNetProcList.Add(const S: string): integer;
begin
  inherited Add(S);
  if Assigned(FOnNetProcListChange) then
    FOnNetProcListChange;
end;

procedure TNetProcList.Clear;
begin
  inherited Clear;
  if Assigned(FOnNetProcListChange) then
    FOnNetProcListChange;
end;

procedure TNetProcList.Delete(Index: integer);
begin
  inherited Delete(Index);
  if Assigned(FOnNetProcListChange) then
    FOnNetProcListChange;
end;

procedure TNetProcList.Exchange(Index1, Index2: integer);
begin
  inherited Exchange(Index1, Index2);
  if Assigned(FOnNetProcListChange) then
    FOnNetProcListChange;
end;

procedure TNetProcList.Insert(Index: integer; const S: string);
begin
  inherited Insert(Index, S);
  if Assigned(FOnNetProcListChange) then
    FOnNetProcListChange;
end;

function TOnlineQuery.GetActive: boolean;
begin
  Result := (State <> dsInactive) and (State <> dsOpening);
end;

procedure TOnlineQuery.SetActive(Value: boolean);
begin
  inherited;
  if Active then
    First;
end;

procedure TOnlineQuery.Open;
begin
  Active := True;
end;

procedure TOnlineQuery.SetInstrucNum(Value: TInstruction);
begin
  FInstrucNum := Value;
  if Value in [istInternalCustProc, istInternalOpen, InternalSQLInstruciton] then
    ThinQuery := True
  else
    ThinQuery := False;
end;

constructor TOnlineQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TNetProcList.Create;
  FSQLDataStr := TNetProcList.Create;
  FSync := True;
  FLoading := False;
  FReplaceFields := False;
  TmpR := False;
  FCachedUpdate := False;
  CacheList := TNetProcList.Create;
  FThinQuery := False;
  FInstrucNum := IstInternalOpen;
  FSubInstrucNum := 0;
  FSQL.OnNetProcListChange := SQLStringChange;
  DateSeparator := '-';
end;

destructor TOnlineQuery.Destroy;
begin
  DestroyQuerys;
  FSQL.Free;
  FSQLDataStr.Free;
  CacheList.Free;
  inherited Destroy;
end;

procedure TOnlineQuery.DestroyQuerys;
begin
  FSync := False;
  if Active then
    EmptyTable;
end;

function GetTableNameFromSQL(SQLText: TNetProcString): TNetProcString;
var
  TempSQL, SUBSQL: TNetProcString;
  WPS, SLen: integer;
begin
  TempSQL := UpperCase(SQLText);
  SLen := Length(SQLText);
  SUBSQL := Trim(Copy(SQLText, Pos('FROM ', TempSQL) + 5, SLen));
  WPS := Pos('WHERE', TempSQL);
  if WPS = 0 then
    Result := SUBSQL
  else
    Result := Copy(SUBSQL, 1, WPS - 1);
end;

function Min(A1, A2: integer): integer;
begin
  if A1 > A2 then
    Result := A2
  else
    Result := A1;
end;

function FieldToSQLString(Field: TField; TmpStr, Value: TNetProcString): TNetProcString;
var ValueStr: string;
begin
  case Field.DataType of
    ftUnknown, ftString, ftFixedChar, ftWideString, ftMemo, ftVariant, ftBlob,
    ftFmtMemo:
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := '''' + ' ' + ''''
      else
      begin
        ValueStr := Value;
        if Pos('''', Value) > 0 then
          ValueStr := StringReplace(Value, '''', '''''', [rfReplaceAll]);
        TmpStr := '''' + ValueStr + '''';
      end;
    end;
    ftTypedBinary:
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := 'NULL'
      else
        TmpStr := Value;
    end;
    ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD, ftBytes,
    ftVarBytes, ftAutoInc, ftLargeint:
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := '0';
    end;
    ftBoolean:
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := '0';
    end;
    ftDate, ftTime, ftDateTime:
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := '''' + '2009-11-01 11:11:11' + ''''
      else
        TmpStr := '''' + Value + '''';
    end;
    else
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := '''' + ' ' + ''''
      else
        TmpStr := Value;
    end;
  end;
  Result := TmpStr;
end;

procedure TOnlineQuery.GenerateInsertData;
var
  _SQL, _Into, _Values, _Where, S, S1, TmpStr, FieldStr: TNetProcString;
  Sep, IsBlob: boolean;
  Field: TField;
begin
  _Into := '';
  _Values := '';
  _Where := '';
  _SQL := '';
  Sep := False;
  S := FEditFields;
  while S <> '' do
  begin
    S1 := RetrieveStr(S, ';');
    Field := FindField(S1);
    if Field = nil then
      raise Exception.Create('Invalid EditField=' + S1);
    IsBlob := Field is TBlobField;
    if Sep then
    begin
      _Into := _Into + ', ';
      _Values := _Values + ', ';
      if not isBlob then
        _Where := _Where + ' and ' + ' ';
    end;

    Sep := True;
    _Into := _Into + S1;
    {$IFDEF OverDelphi2007}
    TmpStr := Field.AsAnsiString;
    {$ELSE}
    TmpStr := Field.AsString;
    {$ENDIF}

     {$IFDEF OverDelphi2007}
    FieldStr := Field.AsAnsiString;
    {$ELSE}
    FieldStr := Field.AsString;
    {$ENDIF}

    TmpStr := FieldToSQLString(Field, TmpStr, FieldStr);

    if isBlob then
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := '''' + ' ' + '''';
    end;

    _Values := _Values + TmpStr;
    if not isBlob then
      _Where := _Where + S1 + '=:' + S1;
  end;

  _SQL := _SQL + 'insert into ' + FTableName + ' (' + _Into + ')' +
    ' ' + 'values (' + _Values + ')';
{$IFDEF FPC}
  FSQLDataStr.Text := UTF8Decode(_SQL);
{$ELSE}
  FSQLDataStr.Text := _SQL;
{$ENDIF}
end;

function TOnlineQuery.ComputePrimaryKeyForSQLData(OldValues, NullValues: boolean):
TNetProcString;
var
  S, S1, TmpStr, FieldStr: TNetProcString;
  Sep: boolean;
  Field: TField;
begin
  Sep := False;
  S := FPrimaryKey;
  Result := '';
  while S <> '' do
  begin
    if Sep then
      Result := Result + ' and ';
    Sep := True;
    S1 := RetrieveStr(S, ';');
    Field := FindField(S1);
    Result := Result + S1;
    if NullValues then
      Result := Result + ' is Null '
    else if NullValues then
      Result := Result + '=:' + S1
    else
    begin
      TmpStr := VarToStr(Field.OldValue);
      FieldStr := TmpStr;
      TmpStr := FieldToSQLString(Field, TmpStr, FieldStr);
      Result := Result + '=' + TmpStr;
    end;
  end;
end;

procedure TOnlineQuery.GenerateUpdateData;
var
  _From, _Values, S, S1, TmpStr, FieldStr: TNetProcString;
  Sep: boolean;
  Field: TField;
  IsChanged: boolean;
begin
  _From := '';
  _Values := '';
  Sep := False;
  S := FEditFields;
  while S <> '' do
  begin
    S1 := RetrieveStr(S, ';');
    Field := FindField(S1);
    if Field = nil then
      raise Exception.Create('Invalid EditField=' + S1);

    {$IFDEF OverDelphi2007}
    if Field.AsAnsiString <> VarToStr(Field.OldValue) then
    {$ELSE}
      if Field.AsString <> VarToStr(Field.OldValue) then
    {$ENDIF}
        IsChanged := True
      else
        IsChanged := False;

    if Sep and IsChanged then
    begin
      _Values := _Values + ', ';
      _From := _From + ', ';
    end;

    if IsChanged then
    begin
    {$IFDEF OverDelphi2007}
      TmpStr := Field.AsAnsiString;
    {$ELSE}
      TmpStr := Field.AsString;
    {$ENDIF}

    {$IFDEF OverDelphi2007}
      FieldStr := Field.AsAnsiString;
    {$ELSE}
      FieldStr := Field.AsString;
    {$ENDIF}

      TmpStr := FieldToSQLString(Field, TmpStr, FieldStr);
      Sep := True;
      _Values := _Values + S1 + '=' + TmpStr;
    end;
    _From := _From + S1;
  end;
{$IFDEF FPC}
  FSQLDataStr.Text :=
    UTF8Decode('update ' + FTableName + ' set ' + _Values + ' ' +
    'where ' + ComputePrimaryKeyForSQLData(True, False));
{$ELSE}
  FSQLDataStr.Text :=
    'update ' + FTableName + ' set ' + _Values + ' ' + 'where ' +
    ComputePrimaryKeyForSQLData(True, False);
{$ENDIF}
end;

procedure TOnlineQuery.GenerateDeleteData;
begin
  FSQLDataStr.Text := 'Delete from ' + FTableName + ' where ' +
{$IFDEF FPC}
    UTF8Decode(ComputePrimaryKeyForSQLData(True, False));
{$ELSE}
  ComputePrimaryKeyForSQLData(True, False);
{$ENDIF}
end;

function TOnlineQuery.ApplyCacheUpdate: boolean;
var
  CacheCount, I: integer;
begin
  if (not Active) or (not FCachedUpdate) then
  begin
    Result := True;
    Exit;
  end;
  I := 0;
  while I < CacheList.Count do
  begin
    if Length(Trim(CacheList[I])) = 0 then
      CacheList.Delete(I)
    else
      I := I + 1;
  end;
  if CacheList.Count > 0 then
  begin
    CacheCount := FOnlineConn.Buffer.CacheExecSQL(Self, CacheList.Text);
    if CacheCount = CacheList.Count then
    begin
      Result := True;
      CacheList.Clear;
    end
    else
      Result := False;
  end
  else
    Result := True;
end;

function TOnlineQuery.OnlineRequest(IsSQLOpen: boolean; xInstruc: integer;
  xParam: TNetProcString): integer;
begin
  Result := 0;
  try
    Result := FOnlineConn.Buffer.DoSpecialSQL(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineScript(xInstruc: integer; xParam: TNetProcString): integer;
begin
  Result := 0;
  try
    Result := FOnlineConn.Buffer.DoSQLScript(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineStoredProc(xInstruc: integer;
  xParam: TNetProcString): integer;
begin
  Result := 0;
  try
    Result := FOnlineConn.Buffer.DoStoredProc(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineClientProcess(xInstruc: integer;
  xParam: TNetProcString): TNetProcString;
begin
  RetValue := '';
  Result := '';
  try
    Result := FOnlineConn.Buffer.ProcessDynamicCustProc(Self, xInstruc, xParam);
    RetValue := FOnlineConn.Buffer.ReturnStr;
  except
  end;
end;

function TOnlineQuery.InternalSQL(SubInstruc: integer; xParam: TNetProcString): integer;
begin
  Result := 0;
  try
    Result := FOnlineConn.Buffer.DoInternalSpecialSQL(Self, SubInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.InternalProcess(xInstruc: integer;
  xParam: TNetProcString): TNetProcString;
begin
  RetValue := '';
  try
    Result := FOnlineConn.Buffer.ProcessInternalCustProc(Self, xInstruc, xParam);
    RetValue := FOnlineConn.Buffer.ReturnStr;
  except
  end;
end;

procedure TOnlineQuery.OnLinePepare;
var
  I: integer;
  S: TNetProcString;
begin
  case SQLInstruction of
    IstSQLExec:
    begin
      if FCachedUpdate then
        CacheList.Add(SQL)
      else
        FOnlineConn.Buffer.ExecSQL(Self, SQL);
    end;
    IstSQLOpen: FOnlineConn.Buffer.OpenSQL(Self, SQL, FReplaceFields, TmpR);
    IstSQLFieldDefs: FOnlineConn.Buffer.UpdateFieldDefs(Self, SQL);
  end;

  SQL := UpperCase(SQL);
  for I := 0 to FieldCount - 1 do
  begin
    S := uppercase(Fields[I].FieldName);
    if pos(':' + S, SQL) > 0 then
    begin
      FOnlineConn.Buffer.AddSQLParam(Fields[I].FieldName, Fields[I].DataType,
        Fields[I].Value);
    end;
    if pos(':OLD_' + S, SQL) > 0 then
    begin
      FOnlineConn.Buffer.AddSQLParam('Old_' + Fields[I].FieldName,
        Fields[I].DataType, Fields[I].OldValue);
    end;
  end;
end;

function TOnlineQuery.OnLineRun;
begin
  Result := FOnlineConn.Buffer.RunSQL;
end;

procedure TOnlineQuery.SetIndexFieldNames(const Value: TNetProcString);
begin
  FIndexFieldNames := Value;
end;

procedure TOnlineQuery.SetTableName(const Value: TNetProcString);
begin
  CheckInactive;
  FTableName := Value;
end;

procedure TOnlineQuery.SetPrimaryKey(const Value: TNetProcString);
begin
  CheckInactive;
  FPrimaryKey := Value;
end;

procedure TOnlineQuery.SetSync(Value: boolean);
begin
  CheckBrowseMode;
  if State = dsBrowse then
    FSync := Value;
end;

function TOnlineQuery.GetSQL: TNetProcList;
begin
  Result := FSQL;
end;

procedure TOnlineQuery.SetSQL(Value: TNetProcList);
begin
  Close;
  DestroyQuerys;
  FSQL.Assign(Value);
  FTableName := GetTableNameFromSQL(FSQL.Text);
  FPrimaryKey := '';
end;

procedure TOnlineQuery.SQLStringChange;
begin
  Close;
  DestroyQuerys;
  FTableName := GetTableNameFromSQL(FSQL.Text);
  FPrimaryKey := '';
end;

procedure TOnlineQuery.InternalInitFieldDefs;
begin
  if not FLoading then
    if FThinQuery then
    begin
      inherited InternalInitFieldDefs;
      exit;
    end;

  if not FLoading then
  begin
    OnLinePepare(FSQL.Text, IstSQLFieldDefs);
    try
      OnLineRun;
    finally
    end;
  end;
end;

procedure TOnlineQuery.InternalOpen;
var
  i, Keyi, TotalKeyCount: integer;
begin
  if not FOnlineConn.FActive then
    if not FOnlineConn.Logon then
    begin
      FOnlineConn.Buffer.ReturnStr := '';
      FOnlineConn.Buffer.RecvBuffer := '';
      Exit;
    end;

  if (csDesigning in ComponentState) then
    TmpR := True
  else
    TmpR := False;

  if FThinQuery then
  begin
    if FLoading then
      inherited InternalOpen
    else
    begin
      FEditFields := '';
      FLoading := True;
      try
        FSync := False;

        if FInstrucNum = IstInternalOpen then
          InternalSQL(FSubInstrucNum, FClientParam)
        else
          OnlineRequest(True, byte(FInstrucNum), FClientParam);

        SetIndexFieldNames(FIndexFieldNames);
        FSync := True;
      finally
        FLoading := False;
      end;
    end;
  end
  else
  begin
    FTableName := GetTableNameFromSQL(FSQL.Text);
    if FLoading then
      inherited InternalOpen
    else
    begin
      if FTableName = '' then
        raise Exception.Create('TableName can''t be null, please check the SQL script');
      FLoading := True;
      try
        FSync := False;
        OnLinePepare(FSQL.Text, IstSQLOpen);
        OnLineRun;
        SetIndexFieldNames(FIndexFieldNames);
        FSync := True;
      finally
        FLoading := False;
      end;
    end;
  end;
  if FieldDefs.Count > 0 then
  begin
    TotalKeyCount := Min(3, FieldDefs.Count);
    if FPrimaryKey = '' then
    begin
      FPrimaryKey := '';
      for Keyi := 0 to TotalKeyCount - 1 do
        if Keyi = TotalKeyCount - 1 then
          FPrimaryKey := FPrimaryKey + FieldDefs[Keyi].Name
        else
          FPrimaryKey := FPrimaryKey + FieldDefs[Keyi].Name + ';';
    end;
    if FPrimaryKey = '' then
      raise Exception.Create('PrimaryKey can''t be null');
    FEditFields := '';
    for i := 0 to FieldDefs.Count - 1 do
    begin
      if i > 0 then
        FEditFields := FEditFields + ';';
      FEditFields := FEditFields + FieldDefs[I].Name;
    end;
  end;
end;

procedure TOnlineQuery.InternalClose;
begin
  DestroyQuerys;
  inherited InternalClose;
end;

procedure TOnlineQuery.InternalPost;
begin
  if FThinQuery then
  begin
    inherited InternalPost;
    exit;
  end;
  if FSync then
  begin
    if State = dsInsert then
    begin
      GenerateInsertData;
      OnLinePepare(FSQLDataStr.Text, IstSQLExec);
    end
    else
    begin
      GenerateUpdateData;
      OnLinePepare(FSQLDataStr.Text, IstSQLExec);
    end;
  end;
  inherited InternalPost;
end;

procedure TOnlineQuery.InternalDelete;
begin
  if FThinQuery then
  begin
    inherited InternalDelete;
    exit;
  end;

  if FSync then
  begin
    GenerateDeleteData;
    OnLinePepare(FSQLDataStr.Text, IstSQLExec);
  end;
  inherited InternalDelete;
end;

procedure TOnlineQuery.ReOpenTable;
begin
  if Active then
  begin
    Active := False;
    Active := True;
  end
  else
    Active := True;
end;

function TOnlineQuery.GetServerTime: TNetProcString;
begin
  Result := FOnlineConn.Buffer.GetServerTime;
end;

end.

