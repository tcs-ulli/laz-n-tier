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
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DB,
  SynaCSockets, ClientProc, DataProcUtils;

type
  TOnlineQuery = class(TMemDB)
  private
    FOnlineConn: TOnlineConnection;
    FTableName, FPrimaryKey, FIndexFieldNames, FEditFields: TNetProcString;
    FSync, FLoading: Boolean;
    FSQL, FSQLDataStr: TStringList;
    FReplaceFields: Boolean;
    TmpR, FThinQuery: Boolean;
    FCachedUpdate: Boolean;
    FInstrucNum: TInstruction;
    FSubInstrucNum: Byte;
    FClientParam: TNetProcString;
    procedure DestroyQuerys;
    procedure OnLinePepare(SQL: TNetProcString; SQLInstruction: TSQLInstruction);
    function OnLineRun: integer;
    procedure SetIndexFieldNames(const Value: TNetProcString);
    procedure SetTableName(const Value: TNetProcString);
    procedure SetPrimaryKey(const Value: TNetProcString);
    procedure SetSync(Value: Boolean);
    function GetSQL: TStringList;
    procedure SetSQL(Value: TStringList);
    function GetActive: Boolean;
    Procedure SetActive(Value: Boolean); override;
  protected
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
  public
    CacheList: TStringList;
    RetValue: TNetProcString;
    function ApplyCacheUpdate: Boolean;
    function OnlineRequest(IsSQLOpen: Boolean; xInstruc: integer; xParam: TNetProcString): integer;
    function InternalSQL(SubInstruc: integer; xParam: TNetProcString): integer;
    function OnlineScript(xInstruc: integer; xParam: TNetProcString): integer;
    function OnlineStoredProc(xInstruc: integer; xParam: TNetProcString): integer;
    function OnlineClientProcess(xInstruc: integer; xParam: TNetProcString): TNetProcString;
    function InternalProcess(xInstruc: integer; xParam: TNetProcString): TNetProcString;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Sync: Boolean read FSync write SetSync;
    procedure GenerateInsertData;
    procedure GenerateUpdateData;
    procedure GenerateDeleteData;
    function ComputePrimaryKeyForSQLData(OldValues, NullValues: Boolean): TNetProcString;
    procedure ReOpenTable;
    function GetServerTime: TNetProcString;
    procedure Open;
  published
    property InstrucNum: TInstruction read FInstrucNum write FInstrucNum;
    property InstrucSubNum: Byte read FSubInstrucNum write FSubInstrucNum;
    property ThinQuery: Boolean read FThinQuery write FThinQuery;
    property ClientParam: TNetProcString read FClientParam write FClientParam;
    property CachedUpdate: Boolean read FCachedUpdate write FCachedUpdate;
    property OnlineConn: TOnlineConnection read FOnlineConn write FOnlineConn;
    property TableName: TNetProcString read FTableName write SetTableName;
    property PrimaryKey: TNetProcString read FPrimaryKey write SetPrimaryKey;
    property SQL: TStringList read GetSQL write SetSQL;
    property GetFields: Boolean read FReplaceFields write FReplaceFields;
    property Active: Boolean read GetActive write SetActive;
    property Filtered;
    property Filter;
    property Capacity;
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
    property ThinQuery;
    property ClientParam;
    property CachedUpdate;
    property OnlineConn;
    property TableName;
    property PrimaryKey;
    property SQL;
    property GetFields;
    property Active;
    property Filtered;
    property Filter;
    property Capacity;
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

function TOnlineQuery.GetActive: boolean;
begin
  result := (State <> dsInactive) and (State <> dsOpening);
end;

Procedure TOnlineQuery.SetActive(Value: Boolean);
begin
  inherited;
  if Active then
    First;
end;

procedure TOnlineQuery.Open;
begin
  Active := True;
end;

constructor TOnlineQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FSQLDataStr := TStringList.Create;
  FSync := True;
  FLoading := False;
  FReplaceFields := False;
  TmpR := False;
  FCachedUpdate := False;
  CacheList := TStringList.Create;
  FThinQuery := False;
  FInstrucNum := IstInternalOpen;
  FSubInstrucNum := 0;
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

procedure TOnlineQuery.GenerateInsertData;
var
  _SQL, _Into, _Values, _Where, S, S1, TmpStr: TNetProcString;
  Sep, IsBlob: Boolean;
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
    TmpStr := Field.AsTNetProcString;
    {$ELSE}
    TmpStr := Field.AsString;
    {$ENDIF}

    case Field.DataType of
      ftUnknown, ftString, ftFixedChar, ftWideString, ftMemo, ftVariant, ftBlob,
        ftFmtMemo:
        begin
          if Length(trim(TmpStr)) = 0 then
            TmpStr := '''' + ' ' + ''''
          else
            {$IFDEF OverDelphi2007}
            TmpStr := '''' + Field.AsTNetProcString + '''';
            {$ELSE}
            TmpStr := '''' + Field.AsString + '''';
            {$ENDIF}
        end;
      ftTypedBinary:
        begin
          if Length(trim(TmpStr)) = 0 then
            TmpStr := 'NULL'
          else
            {$IFDEF OverDelphi2007}
            TmpStr := Field.AsTNetProcString;
            {$ELSE}
            TmpStr := Field.AsString;
            {$ENDIF}
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
            TmpStr := '''' + '2009-11-01 11:11:11' + '''';
        end;
    else
      begin
        if Length(trim(TmpStr)) = 0 then
          TmpStr := '''' + ' ' + ''''
        else
          {$IFDEF OverDelphi2007}
          TmpStr := '''' + Field.AsTNetProcString + '''';
          {$ELSE}
          TmpStr := '''' + Field.AsString + '''';
          {$ENDIF}
      end;
    end;

    if isBlob then
    begin
      if Length(trim(TmpStr)) = 0 then
        TmpStr := '''' + ' ' + '''';
    end;

    _Values := _Values + TmpStr;
    if not isBlob then
      _Where := _Where + S1 + '=:' + S1;
  end;

  _SQL := _SQL + 'insert into ' + FTableName + ' (' + _Into +
    ')' + ' ' + 'values (' + _Values + ')';
  FSQLDataStr.Clear;
{$IFDEF FPC}
  FSQLDataStr.Text := UTF8Decode(_SQL);
{$ELSE}
  FSQLDataStr.Text := _SQL;
{$ENDIF}
end;

function TOnlineQuery.ComputePrimaryKeyForSQLData(OldValues, NullValues:
  Boolean): TNetProcString;
var
  S, S1, TmpStr: TNetProcString;
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

      case Field.DataType of
        ftUnknown, ftString, ftFixedChar, ftWideString, ftMemo, ftVariant, ftBlob,
          ftFmtMemo:
          begin
            if Length(trim(TmpStr)) = 0 then
              TmpStr := '''' + ' ' + ''''
            else
              TmpStr := '''' + VarToStr(Field.OldValue) + '''';
          end;
        ftTypedBinary:
          begin
            if Length(trim(TmpStr)) = 0 then
              TmpStr := 'NULL'
            else
              TmpStr := '''' + VarToStr(Field.OldValue) + '''';
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
              TmpStr := '''' + '2009-11-01 11:11:11' + '''';
          end;
      else
        begin
          if Length(trim(TmpStr)) = 0 then
            TmpStr := '''' + ' ' + ''''
          else
            TmpStr := '''' + VarToStr(Field.OldValue) + '''';
        end;
      end;
      Result := Result + '=' + TmpStr;
    end;
  end;
end;

procedure TOnlineQuery.GenerateUpdateData;
var
  _From, _Values, S, S1, TmpStr: TNetProcString;
  Sep: Boolean;
  Field: TField;
  IsChanged: Boolean;
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
    if Field.AsTNetProcString <> VarToStr(Field.OldValue) then
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
      TmpStr := Field.AsTNetProcString;
      {$ELSE}
      TmpStr := Field.AsString;
      {$ENDIF}

      case Field.DataType of
        ftUnknown, ftString, ftFixedChar, ftWideString, ftMemo, ftVariant, ftBlob,
          ftFmtMemo:
          begin
            if Length(trim(TmpStr)) = 0 then
              TmpStr := '''' + ' ' + ''''
            else
              {$IFDEF OverDelphi2007}
              TmpStr := '''' + Field.AsTNetProcString + '''';
              {$ELSE}
              TmpStr := '''' + Field.AsString + '''';
              {$ENDIF}
          end;
        ftTypedBinary:
          begin
            if Length(trim(TmpStr)) = 0 then
              TmpStr := 'NULL'
            else
              {$IFDEF OverDelphi2007}
              TmpStr := Field.AsTNetProcString;
              {$ELSE}
              TmpStr := Field.AsString;
              {$ENDIF}
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
              TmpStr := '''' + '2009-11-01 11:11:11' + '''';
          end;
      else
        begin
          if Length(trim(TmpStr)) = 0 then
            TmpStr := '''' + ' ' + ''''
          else
            {$IFDEF OverDelphi2007}
            TmpStr := '''' + Field.AsTNetProcString + '''';
            {$ELSE}
            TmpStr := '''' + Field.AsString + '''';
            {$ENDIF}
        end;
      end;
      Sep := True;
      _Values := _Values + S1 + '=' + TmpStr;
    end;
    _From := _From + S1;
  end;

  FSQLDataStr.Clear;

{$IFDEF FPC}
  FSQLDataStr.Text :=
    UTF8Decode('update ' + FTableName + ' set ' + _Values + ' ' +
    'where ' + ComputePrimaryKeyForSQLData(
    True, False));
{$ELSE}
  FSQLDataStr.Text :=
    'update ' + FTableName + ' set ' + _Values + ' ' +
    'where ' + ComputePrimaryKeyForSQLData(
    True, False);
{$ENDIF}
end;

procedure TOnlineQuery.GenerateDeleteData;
begin
  FSQLDataStr.Clear;
  FSQLDataStr.Text := 'Delete from ' + FTableName + ' where ' +
{$IFDEF FPC}
  UTF8Decode(ComputePrimaryKeyForSQLData(True, False));
{$ELSE}
  ComputePrimaryKeyForSQLData(True, False);
{$ENDIF}
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

function TOnlineQuery.OnlineRequest(IsSQLOpen: Boolean; xInstruc: integer; xParam: TNetProcString): integer;
begin
  Result := 0;
  try
    result := FOnlineConn.Buffer.DoSpecialSQL(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineScript(xInstruc: integer; xParam: TNetProcString): integer;
begin
  Result := 0;
  try
    result := FOnlineConn.Buffer.DoSQLScript(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineStoredProc(xInstruc: integer; xParam: TNetProcString): integer;
begin
  Result := 0;
  try
    result := FOnlineConn.Buffer.DoStoredProc(Self, xInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.OnlineClientProcess(xInstruc: integer; xParam: TNetProcString): TNetProcString;
begin
  RetValue := '';
  Result := '';
  try
    result := FOnlineConn.Buffer.ProcessDynamicCustProc(Self, xInstruc, xParam);
    RetValue := FOnlineConn.Buffer.ReturnStr;
  except
  end;
end;

function TOnlineQuery.InternalSQL(SubInstruc: integer; xParam: TNetProcString): integer;
begin
  Result := 0;
  try
    result := FOnlineConn.Buffer.DoInternalSpecialSQL(Self, SubInstruc, xParam);
  except
  end;
end;

function TOnlineQuery.InternalProcess(xInstruc: integer; xParam: TNetProcString): TNetProcString;
begin
  RetValue := '';
  try
    result := FOnlineConn.Buffer.ProcessInternalCustProc(Self, xInstruc, xParam);
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

procedure TOnlineQuery.SetSync(Value: Boolean);
begin
  CheckBrowseMode;
  if State = dsBrowse then
    FSync := Value;
end;

function TOnlineQuery.GetSQL: TStringList;
begin
  Result := FSQL;
end;

procedure TOnlineQuery.SetSQL(Value: TStringList);
begin
  Close;
  DestroyQuerys;
  FSQL.Assign(Value);
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
          OnlineRequest(True, Byte(FInstrucNum), FClientParam);

        SetIndexFieldNames(FIndexFieldNames);
        FSync := True;
      finally
        FLoading := False;
      end;
      if FieldDefs.Count > 0 then
      begin
        TotalKeyCount := Min(3, FieldDefs.Count);
        FPrimaryKey := '';
        for Keyi := 0 to TotalKeyCount - 1 do
          if Keyi = TotalKeyCount - 1 then
            FPrimaryKey := FPrimaryKey + FieldDefs[Keyi].Name
          else
            FPrimaryKey := FPrimaryKey + FieldDefs[Keyi].Name + ';';
      end;
      if FEditFields = '' then
      begin
        FEditFields := '';
        for i := 0 to FieldDefs.Count - 1 do
        begin
          if i > 0 then
            FEditFields := FEditFields + ';';
          FEditFields := FEditFields + FieldDefs[I].Name;
        end;
      end;
    end;
    exit;
  end;

  if FTableName = '' then
    FTableName := GetTableNameFromSQL(FSQL.Text);
  if FLoading then
    inherited InternalOpen
  else
  begin
    if FTableName = '' then
      raise Exception.Create('TableName can''t be null');
    FLoading := True;
    try
      FSync := False;
      OnLinePepare(FSQL.Text, IstSQLOpen);
      try
        OnLineRun;
        SetIndexFieldNames(FIndexFieldNames);
      finally
      end;
      FSync := True;
    finally
      FLoading := False;
    end;
    if FieldDefs.Count > 0 then
      if FPrimaryKey = '' then
      begin
        TotalKeyCount := Min(3, FieldDefs.Count);
        for Keyi := 0 to TotalKeyCount - 1 do
          if Keyi = TotalKeyCount - 1 then
            FPrimaryKey := FPrimaryKey + FieldDefs[Keyi].Name
          else
            FPrimaryKey := FPrimaryKey + FieldDefs[Keyi].Name + ';';
      end;
    if FPrimaryKey = '' then
      raise Exception.Create('PrimaryKey can''t be null');
    if FEditFields = '' then
    begin
      FEditFields := '';
      for i := 0 to FieldDefs.Count - 1 do
      begin
        if i > 0 then
          FEditFields := FEditFields + ';';
        FEditFields := FEditFields + FieldDefs[I].Name;
      end;
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
  result := FOnlineConn.Buffer.GetServerTime;
end;

end.

