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
  MemDataBase, Variants, DateUtils, Messages, SysUtils, Classes, Graphics,
  Controls, DB, SynaCSockets, ClientProc, DataProcUtils;

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
    FOnlineConnection: TCustomOnlineConnection;
    FTableName, FPrimaryKey, FIndexFieldNames, FEditFields: AnsiNetProcString;
    FSync, FLoading: Boolean;
    FSQL, FSQLDataStr: TNetProcList;
    FReplaceFields: Boolean;
    TmpR, FThinQuery: Boolean;
    FCachedUpdate: Boolean;
    FInstrucNum: TInstruction;
    FSubInstrucNum: byte;
    FClientParam: AnsiNetProcString;
    FRowsAffected: integer;
    procedure SetInstrucNum(Value: TInstruction);
    procedure DestroyQuerys;
    procedure OnLinePepare(SQL: AnsiNetProcString; SQLInstruction:
      TSQLInstruction);
    function OnLineRun: integer;
    procedure SetIndexFieldNames(const Value: AnsiNetProcString);
    procedure SetTableName(const Value: AnsiNetProcString);
    procedure SetPrimaryKey(const Value: AnsiNetProcString);
    procedure SetSync(Value: Boolean);
    function GetSQL: TNetProcList;
    procedure SetSQL(Value: TNetProcList);
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean); override;
    procedure SQLStringChange;
  protected
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
  public
    CacheList: TNetProcList;
    RetValue: AnsiNetProcString;
    function ApplyCacheUpdate: Boolean;
    function OnlineRequest(IsSQLOpen: Boolean; xInstruc: integer;
      xParam: AnsiNetProcString): integer;
    function InternalSQL(SubInstruc: integer; xParam: AnsiNetProcString):
      integer;
    function OnlineScript(xInstruc: integer; xParam: AnsiNetProcString):
      integer;
    function OnlineStoredProc(xInstruc: integer; xParam: AnsiNetProcString):
      integer;
    function OnlineClientProcess(xInstruc: integer;
      xParam: AnsiNetProcString): AnsiNetProcString;
    function InternalProcess(xInstruc: integer; xParam: AnsiNetProcString):
      AnsiNetProcString;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Sync: Boolean read FSync write SetSync;
    procedure GenerateInsertData;
    function GenerateUpdateData: Boolean;
    procedure GenerateDeleteData;
    function ComputePrimaryKeyForSQLData(OldValues, NullValues: Boolean):
      AnsiNetProcString;
    procedure ReOpenTable;
    function GetServerTime: AnsiNetProcString;
    procedure Open;
    procedure ExecSQL;
    procedure ExecScript;
    function RowsAffected: longint;
    property ThinQuery: Boolean read FThinQuery write FThinQuery;
    property TableName: AnsiNetProcString read FTableName write SetTableName;
  published
    property InstrucNum: TInstruction read FInstrucNum write SetInstrucNum;
    property InstrucSubNum: byte read FSubInstrucNum write FSubInstrucNum;
    property ClientParam: AnsiNetProcString read FClientParam write
      FClientParam;
    property CachedUpdate: Boolean read FCachedUpdate write FCachedUpdate;
    property OnlineConnection: TCustomOnlineConnection
      read FOnlineConnection write FOnlineConnection;
    property PrimaryKey: AnsiNetProcString read FPrimaryKey write SetPrimaryKey;
    property SQL: TNetProcList read GetSQL write SetSQL;
    property GetFields: Boolean read FReplaceFields write FReplaceFields;
    property Active: Boolean read GetActive write SetActive;
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
    property OnlineConnection;
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

function TOnlineQuery.GetActive: Boolean;
begin
  Result := (State <> dsInactive) and (State <> dsOpening);
end;

procedure TOnlineQuery.SetActive(Value: Boolean);
begin
  if FOnlineConnection = nil then
    Exit;

  if Value then
    if not FOnlineConnection.NetActive then
      if not FOnlineConnection.Logon then
      begin
        FOnlineConnection.Buffer.ReturnStr := '';
        FOnlineConnection.Buffer.RecvBuffer := '';
        if not (csDesigning in ComponentState) then
          raise Exception.Create('Connection failed');
        Exit;
      end;

  if Value then
    if not FOnlineConnection.NetActive then
      Exit;

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
  if Value in [istInternalCustProc, istInternalOpen, InternalSQLInstruciton]
    then
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
  FInstrucNum := IstSQL;
  FSubInstrucNum := 0;
  FSQL.OnNetProcListChange := SQLStringChange;
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

function GetTableNameFromSQL(SQLText: AnsiNetProcString): AnsiNetProcString;
var
  TempSQL: AnsiNetProcString;
  FPS, WPS, SLen, TBLen: integer;
begin
  TempSQL := UpperCase(SQLText);
  if Pos('SELECT', TrimLeft(TempSQL)) <> 1 then
    Exit;
  SLen := Length(SQLText);
  FPS := Pos('FROM ', TempSQL) + 5;
  WPS := Pos('WHERE', TempSQL) - 2;
  if WPS = -2 then
  begin
    WPS := Pos('GROUP BY', TempSQL) - 2;
    if WPS = -2 then
      WPS := Pos('ORDER BY', TempSQL) - 2;
  end;
  if WPS = -2 then
    TBLen := SLen - FPS + 1
  else
    TBLen := WPS - FPS + 1;
  SetLength(Result, TBLen);
  move(SQLText[FPS], Result[1], TBLen);
  Result := Trim(Result);
end;

function Min(A1, A2: integer): integer;
begin
  if A1 > A2 then
    Result := A2
  else
    Result := A1;
end;

function FieldToSQLString(Field: TField; TmpStr, Value: AnsiNetProcString):
  AnsiNetProcString;
var
  ValueStr: string;
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
    ftSmallint, ftInteger, ftWord, ftBCD, ftBytes,
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
    ftFloat, ftCurrency:
      begin
        if Length(trim(TmpStr)) = 0 then
          TmpStr := '0.00'
        else
          TmpStr := SetAnsiDoubleStr(Trim(Value));
      end;
    ftDateTime:
      begin
        if Length(trim(TmpStr)) = 0 then
          TmpStr := '''' + '2009-11-01 11:11:11' + ''''
        else
          TmpStr := '''' + FormatDateTime('yyyy-MM-dd HH:mm:ss',
            StrToDateTime(Value)) + '''';
      end;
    ftDate:
      begin
        if Length(trim(TmpStr)) = 0 then
          TmpStr := '''' + '2009-11-01' + ''''
        else
          TmpStr := '''' + FormatDateTime('yyyy-MM-dd',
            StrToDateTime(Value)) + '''';
      end;
    ftTime:
      begin
        if Length(trim(TmpStr)) = 0 then
          TmpStr := '''' + '11:11:11' + ''''
        else
          TmpStr := '''' + FormatDateTime('HH:mm:ss',
            StrToDateTime(Value)) + '''';
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

function TOnlineQuery.ComputePrimaryKeyForSQLData(OldValues, NullValues:
  Boolean): AnsiNetProcString;
var
  S, S1, TmpStr, FieldStr: AnsiNetProcString;
  Sep: Boolean;
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

procedure TOnlineQuery.GenerateInsertData;
var
  _SQL, _Into, _Values, S, S1, TmpStr, FieldStr: AnsiNetProcString;
  Sep, IsBlob: Boolean;
  Field: TField;
begin
  _Into := '';
  _Values := '';
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
    end;

    Sep := True;
    _Into := _Into + S1;
    TmpStr := Field.AsString;
    FieldStr := Field.AsString;

    if OnLineConnection.UTF8Code = ccUTF8Encode then
    begin
      FieldStr := UTF8Decode(FieldStr);
      TmpStr := UTF8Decode(TmpStr);
    end;

    if OnLineConnection.UTF8Code = ccUTF8Decode then
    begin
      FieldStr := UTF8Encode(FieldStr);
      TmpStr := UTF8Encode(TmpStr);
    end;

    TmpStr := FieldToSQLString(Field, TmpStr, FieldStr);

    if isBlob then
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := '''' + ' ' + '''';
    end;

    _Values := _Values + TmpStr;
  end;

  _SQL := 'insert into ' + FTableName + ' (' + _Into + ')' +
    ' ' + 'values (' + _Values + ')';

  FSQLDataStr.Text := _SQL;

  if OnLineConnection.UTF8Code = ccUTF8Encode then
    FSQLDataStr.Text := UTF8Decode(_SQL);
  if OnLineConnection.UTF8Code = ccUTF8Decode then
    FSQLDataStr.Text := UTF8Encode(_SQL);
end;

function TOnlineQuery.GenerateUpdateData: Boolean;
var
  _From, _Values, S, S1, TmpStr, FieldStr: AnsiNetProcString;
  Sep: Boolean;
  Field: TField;
  IsChanged: Boolean;
  ChangeCount: integer;
begin
  Result := False;
  _From := '';
  _Values := '';
  Sep := False;
  S := FEditFields;
  ChangeCount := 0;
  while S <> '' do
  begin
    S1 := RetrieveStr(S, ';');
    Field := FindField(S1);
    if Field = nil then
      raise Exception.Create('Invalid EditField=' + S1);

    if Field.AsString <> VarToStr(Field.OldValue) then
    begin
      IsChanged := True;
      ChangeCount := ChangeCount + 1;
    end
    else
      IsChanged := False;

    if Sep and IsChanged then
    begin
      _Values := _Values + ', ';
      _From := _From + ', ';
    end;

    if IsChanged then
    begin
      TmpStr := Field.AsString;
      FieldStr := Field.AsString;

      if OnLineConnection.UTF8Code = ccUTF8Encode then
      begin
        FieldStr := UTF8Decode(FieldStr);
        TmpStr := UTF8Decode(TmpStr);
      end;

      if OnLineConnection.UTF8Code = ccUTF8Decode then
      begin
        FieldStr := UTF8Encode(FieldStr);
        TmpStr := UTF8Encode(TmpStr);
      end;

      TmpStr := FieldToSQLString(Field, TmpStr, FieldStr);
      Sep := True;
      _Values := _Values + S1 + '=' + TmpStr;
    end;
    _From := _From + S1;
  end;
  FSQLDataStr.Text :=
    'update ' + FTableName + ' set ' + _Values + ' ' + 'where ' +
    ComputePrimaryKeyForSQLData(True, False);

  if OnLineConnection.UTF8Code = ccUTF8Decode then
    FSQLDataStr.Text :=
      UTF8Encode('update ' + FTableName + ' set ' + _Values + ' ' +
      'where ' + ComputePrimaryKeyForSQLData(True, False));

  if OnLineConnection.UTF8Code = ccUTF8Encode then
    FSQLDataStr.Text :=
      UTF8Decode('update ' + FTableName + ' set ' + _Values + ' ' +
      'where ' + ComputePrimaryKeyForSQLData(True, False));

  if ChangeCount > 0 then
    Result := True
  else
    FSQLDataStr.Text := '';
end;

procedure TOnlineQuery.GenerateDeleteData;
var
  PrimKeySQL: string;
begin
  PrimKeySQL := ComputePrimaryKeyForSQLData(True, False);
  if OnLineConnection.UTF8Code = ccUTF8Decode then
    PrimKeySQL := UTF8Encode(PrimKeySQL);
  if OnLineConnection.UTF8Code = ccUTF8Encode then
    PrimKeySQL := UTF8Decode(PrimKeySQL);
  FSQLDataStr.Text := 'Delete from ' + FTableName + ' where ' + PrimKeySQL;
end;

function TOnlineQuery.ApplyCacheUpdate: Boolean;
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
    CacheCount := FOnlineConnection.Buffer.CacheExecSQL(Self, CacheList.Text);
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

function TOnlineQuery.OnlineRequest(IsSQLOpen: Boolean; xInstruc: integer;
  xParam: AnsiNetProcString): integer;
begin
  Result := 0;
  try
    Result := FOnlineConnection.Buffer.DoSpecialSQL(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineScript(xInstruc: integer; xParam:
  AnsiNetProcString): integer;
begin
  Result := 0;
  try
    Result := FOnlineConnection.Buffer.DoSQLScript(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineStoredProc(xInstruc: integer;
  xParam: AnsiNetProcString): integer;
begin
  Result := 0;
  try
    Result := FOnlineConnection.Buffer.DoStoredProc(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineClientProcess(xInstruc: integer;
  xParam: AnsiNetProcString): AnsiNetProcString;
begin
  RetValue := '';
  Result := '';
  try
    Result := FOnlineConnection.Buffer.ProcessDynamicCustProc(Self, xInstruc,
      xParam);
    RetValue := FOnlineConnection.Buffer.ReturnStr;
  except
  end;
end;

function TOnlineQuery.InternalSQL(SubInstruc: integer; xParam:
  AnsiNetProcString): integer;
begin
  Result := 0;
  try
    Result := FOnlineConnection.Buffer.DoInternalSpecialSQL(Self, SubInstruc,
      xParam);
  except
  end;
end;

function TOnlineQuery.InternalProcess(xInstruc: integer;
  xParam: AnsiNetProcString): AnsiNetProcString;
begin
  RetValue := '';
  try
    Result := FOnlineConnection.Buffer.ProcessInternalCustProc(Self, xInstruc,
      xParam);
    RetValue := FOnlineConnection.Buffer.ReturnStr;
  except
  end;
end;

procedure TOnlineQuery.OnLinePepare;
var
  I: integer;
  S: AnsiNetProcString;
begin
  case SQLInstruction of
    IstSQLExec:
      begin
        if FCachedUpdate then
          CacheList.Add(SQL)
        else
          FOnlineConnection.Buffer.ExecSQL(Self, SQL);
      end;
    IstSQLOpen: FOnlineConnection.Buffer.OpenSQL(Self, SQL, FReplaceFields,
        TmpR);
    IstSQLFieldDefs: FOnlineConnection.Buffer.UpdateFieldDefs(Self, SQL);
  end;

  SQL := UpperCase(SQL);
  for I := 0 to FieldCount - 1 do
  begin
    S := uppercase(Fields[I].FieldName);
    if pos(':' + S, SQL) > 0 then
    begin
      FOnlineConnection.Buffer.AddSQLParam(Fields[I].FieldName,
        Fields[I].DataType,
        Fields[I].Value);
    end;
    if pos(':OLD_' + S, SQL) > 0 then
    begin
      FOnlineConnection.Buffer.AddSQLParam('Old_' + Fields[I].FieldName,
        Fields[I].DataType, Fields[I].OldValue);
    end;
  end;
end;

function TOnlineQuery.OnLineRun;
begin
  Result := FOnlineConnection.Buffer.RunSQL;
end;

procedure TOnlineQuery.SetIndexFieldNames(const Value: AnsiNetProcString);
begin
  FIndexFieldNames := Value;
end;

procedure TOnlineQuery.SetTableName(const Value: AnsiNetProcString);
begin
  CheckInactive;
  FTableName := Value;
end;

procedure TOnlineQuery.SetPrimaryKey(const Value: AnsiNetProcString);
begin
  CheckInactive;
  FPrimaryKey := Value;
end;

procedure TOnlineQuery.SetSync(Value: Boolean);
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
        raise
          Exception.Create('TableName can''t be null, please check the SQL script');
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
      if GenerateUpdateData then
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

procedure TOnlineQuery.ExecSQL;
var
  TempSQL: string;
begin
  TempSQL := FSQL.Text;
  if OnLineConnection.UTF8Code = ccUTF8Encode then
    TempSQL := UTF8Decode(FSQL.Text);

  if OnLineConnection.UTF8Code = ccUTF8Decode then
    TempSQL := UTF8Encode(FSQL.Text);

  FRowsAffected := FOnlineConnection.Buffer.ExecSQL(Self,
    TempSQL);
end;

procedure TOnlineQuery.ExecScript;
begin
  FRowsAffected := FOnlineConnection.Buffer.DoSQLScript(Self, FSubInstrucNum,
    FSQL.Text);
end;

function TOnlineQuery.RowsAffected: longint;
begin
  Result := FRowsAffected;
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

function TOnlineQuery.GetServerTime: AnsiNetProcString;
begin
  Result := FOnlineConnection.Buffer.GetServerTime;
end;

end.

