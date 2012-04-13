{ ******************************************************* }

{ Delphi VCL Extensions (RX) }

{ Copyright (c) 1995, 1996 AO ROSNO }
{ Copyright (c) 1997, 1998 Master-Bank }

{ ******************************************************* }

unit MemDBUtils;

interface

uses
  //{$IFDEF FPC} LCLType, LCLProc, LCLIntf, {$ENDIF}
  Registry, Classes, SysUtils, DB, IniFiles;

const
  IntegerDataTypes = [ftSmallint, ftInteger, ftWord, ftLargeint];
  NumericDataTypes = IntegerDataTypes + [ftFloat, ftCurrency, ftBCD];
  TimeDataTypes = [ftTime, ftDateTime, ftTimeStamp];

  { The minimal VCL's used string ID is 61440. The custom IDs must be
    less that above. }
  MaxExtStrID = 61300;
  Null = 0;

  { DBLists }
  SLocalDatabase = MaxExtStrID - 86;

  { DBUtils }
  SRetryLogin = MaxExtStrID - 87;

  { DBFilter }
  SExprNotBoolean = MaxExtStrID - 88;
  SExprBadNullTest = MaxExtStrID - 89;
  SExprBadField = MaxExtStrID - 90;
  SCaptureFilter = MaxExtStrID - 91;
  SNotCaptureFilter = MaxExtStrID - 92;

  { RxDBCtrl }
  SInactiveData = MaxExtStrID - 93;
  SBrowseData = MaxExtStrID - 94;
  SEditData = MaxExtStrID - 95;
  SInsertData = MaxExtStrID - 96;
  SSetKeyData = MaxExtStrID - 97;
  SCalcFieldsData = MaxExtStrID - 98;

  { LoginDlg }
  SRegistration = MaxExtStrID - 99;
  SAppTitleLabel = MaxExtStrID - 100;
  SHintLabel = MaxExtStrID - 101;
  SUserNameLabel = MaxExtStrID - 102;
  SPasswordLabel = MaxExtStrID - 103;
  SInvalidUserName = MaxExtStrID - 104;

  { ChPswDlg }
  SChangePassword = MaxExtStrID - 105;
  SOldPasswordLabel = MaxExtStrID - 106;
  SNewPasswordLabel = MaxExtStrID - 107;
  SConfirmPasswordLabel = MaxExtStrID - 108;
  SPasswordChanged = MaxExtStrID - 109;
  SPasswordNotChanged = MaxExtStrID - 110;
  SPasswordsMismatch = MaxExtStrID - 111;

  { DBExcpt }
  SDBExceptCaption = MaxExtStrID - 112;
  SBDEErrorLabel = MaxExtStrID - 113;
  SServerErrorLabel = MaxExtStrID - 114;
  SErrorMsgLabel = MaxExtStrID - 115;
  SNextButton = MaxExtStrID - 116;
  SPrevButton = MaxExtStrID - 117;

  { DBFilter expression parser }
  SExprIncorrect = MaxExtStrID - 118;
  SExprTermination = MaxExtStrID - 119;
  SExprNameError = MaxExtStrID - 120;
  SExprStringError = MaxExtStrID - 121;
  SExprInvalidChar = MaxExtStrID - 122;
  SExprNoRParen = MaxExtStrID - 123;
  SExprExpected = MaxExtStrID - 124;
  SExprBadCompare = MaxExtStrID - 125;

  { DBUtils }
  SConfirmSave = MaxExtStrID - 126;
  SDatabaseName = MaxExtStrID - 127;

  { LoginDlg }
  SUnlockCaption = MaxExtStrID - 128;
  SUnlockHint = MaxExtStrID - 129;

  { RxDBCtrl }
  SDeleteMultipleRecords = MaxExtStrID - 130;

  SPropDefByLookup = 'PropDefByLookup';
  SDataSourceFixed = 'SDataSourceFixed';
  SCircularDataLink = 'SCircularDataLink';

  SDeleteRecordQuestion = 'Delete record?';
  SFieldTypeMismatch =
    'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SInvalidDate = 'Invalid Date';
  SFieldRequired = 'Field ''%s'' must have a value';
  SNotEditing = 'Dataset not in edit or insert mode';
  SUnknownFieldType = 'SUnknownFieldType %s';
  SFieldReadOnly = 'SFieldReadOnly %s';
{$I MemDBRADv.inc}

type
{$IFDEF OverRad2k7}
  TBookmarkType = TBookmark;
  TBookmarkPointerType = Pointer;
  TBuffer = TRecordBuffer;
{$ELSE}
  {$IF FPC_RELEASE >= 5}
  TBookmarkType = TBookmark;
  {$ELSE}
  TBookmarkType = TBookmarkStr;
  {$ENDIF}
  TBookmarkPointerType = TBookmark;
  TBuffer = PChar;
{$ENDIF}
  { TLocateObject }

  TLocateObject = class(TObject)
  private
    FDataSet: TDataSet;
    FLookupField: TField;
    FLookupValue: ansistring;
    FLookupExact, FCaseSensitive: boolean;
    FBookmark: TBookmark;
    FIndexSwitch: boolean;
    procedure SetDataSet(Value: TDataSet);
  protected
    function MatchesLookup(Field: TField): boolean;
    procedure CheckFieldType(Field: TField); virtual;
    procedure ActiveChanged; virtual;
    function LocateFilter: boolean; virtual;
    function LocateKey: boolean; virtual;
    function LocateFull: boolean; virtual;
    function UseKey: boolean; virtual;
    function FilterApplicable: boolean; virtual;
    property LookupField: TField read FLookupField;
    property LookupValue: ansistring read FLookupValue;
    property LookupExact: boolean read FLookupExact;
    property CaseSensitive: boolean read FCaseSensitive;
    property Bookmark: TBookmark read FBookmark write FBookmark;
  public
    function Locate(const KeyField, KeyValue: ansistring;
      Exact, ACaseSensitive: boolean): boolean;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property IndexSwitch: boolean read FIndexSwitch write FIndexSwitch;
  end;

type
  TCreateLocateObject = function: TLocateObject;

const
  CreateLocateObject: TCreateLocateObject = nil;

function CreateLocate(DataSet: TDataSet): TLocateObject;

{ Utility routines }

function IsDataSetEmpty(DataSet: TDataSet): boolean;
procedure RefreshQuery(Query: TDataSet);
function DataSetSortedSearch(DataSet: TDataSet; const Value, FieldName: ansistring;
  CaseInsensitive: boolean): boolean;
function DataSetSectionName(DataSet: TDataSet): ansistring;

procedure InternalSaveFields(DataSet: TDataSet; IniFile: TObject;
  const Section: ansistring);
procedure InternalRestoreFields(DataSet: TDataSet; IniFile: TObject;
  const Section: ansistring; RestoreVisible: boolean);

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: ansistring;
  const KeyValues: variant; Options: TLocateOptions): boolean;
procedure SaveFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile);
procedure RestoreFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile;
  RestoreVisible: boolean);
procedure SaveFields(DataSet: TDataSet; IniFile: TIniFile);
procedure RestoreFields(DataSet: TDataSet; IniFile: TIniFile; RestoreVisible: boolean);
procedure AssignRecord(Source, Dest: TDataSet; ByName: boolean);
function ConfirmDelete: boolean;
procedure ConfirmDataSetCancel(DataSet: TDataSet);
procedure CheckRequiredField(Field: TField);
procedure CheckRequiredFields(const Fields: array of TField);
function ExtractFieldName(const Fields: ansistring; var Pos: integer): ansistring;
procedure FillValueForField(const Field: TField; Value: variant);

{ SQL expressions }

function DateToSQL(Value: TDateTime): ansistring;
function FormatSQLDateRange(Date1, Date2: TDateTime;
  const FieldName: ansistring): ansistring;
function FormatSQLDateRangeEx(Date1, Date2: TDateTime;
  const FieldName: ansistring): ansistring;
function FormatSQLNumericRange(const FieldName: ansistring;
  LowValue, HighValue, LowEmpty, HighEmpty: double; Inclusive: boolean): ansistring;
function StrMaskSQL(const Value: ansistring): ansistring;
function FormatSQLCondition(const FieldName, AOperator, Value: ansistring;
  FieldType: TFieldType; Exact: boolean): ansistring;
function FormatAnsiSQLCondition(const FieldName, AOperator, Value: ansistring;
  FieldType: TFieldType; Exact: boolean): ansistring;

const
  TrueExpr = '0=0';

const
  { Server Date formats }
  sdfStandard16 = '''"''mm''/''dd''/''yyyy''"'''; { "mm/dd/yyyy" }
  sdfStandard32 = '''''''dd/mm/yyyy'''''''; { 'dd/mm/yyyy' }
  sdfOracle = '"TO_DATE(''"dd/mm/yyyy"'', ''DD/MM/YYYY'')"';
  sdfInterbase = '"CAST(''"mm"/"dd"/"yyyy"'' AS DATE)"';
  sdfMSSQL = '"CONVERT(datetime, ''"mm"/"dd"/"yyyy"'', 103)"';

const
  ServerDateFmt: string[50] = sdfStandard16;

var
  NullDate: TDateTime = { -693594 } 0;

procedure _DBError(const Msg: ansistring);
function GetDefaultSection(Component: TComponent): ansistring;
procedure IniWriteString(IniFile: TObject; const Section, Ident, Value: ansistring);
function IncDay(ADate: TDateTime; Delta: integer): TDateTime;
function ReplaceStr(const S, Srch, Replace: ansistring): ansistring;

implementation

uses Forms, Controls, Dialogs, Math;

{ Utility routines }

function ReplaceStr(const S, Srch, Replace: ansistring): ansistring;
var
  I: integer;
  Source: ansistring;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then
    begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end
    else
      Result := Result + Source;
  until I <= 0;
end;

function IncDay(ADate: TDateTime; Delta: integer): TDateTime;
begin
  Result := ADate + Delta;
end;

procedure IniWriteString(IniFile: TObject; const Section, Ident, Value: ansistring);
var
  S: ansistring;
begin
{$IFDEF WIN32}
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteString(Section, Ident, Value)
  else
  begin
{$ENDIF}
    S := Value;
    if S <> '' then
    begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or
        ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteString(Section, Ident, S);
{$IFDEF WIN32}
  end;
{$ENDIF}
end;

function GetDefaultSection(Component: TComponent): ansistring;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then
  begin
    if Component is TCustomForm then
      Result := Component.ClassName
    else
    begin
      Result := Component.Name;
      if Component is TControl then
      begin
        F := GetParentForm(TControl(Component));
        if F <> nil then
          Result := F.ClassName + Result
        else
        begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end
      else
      begin
        Owner := Component.Owner;
        if Owner is TForm then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end
  else
    Result := '';
end;

procedure _DBError(const Msg: ansistring);
begin
  DatabaseError(Msg);
end;

function ConfirmDelete: boolean;
begin
  Screen.Cursor := crDefault;
  Result := MessageDlg(SDeleteRecordQuestion, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes;
end;

procedure ConfirmDataSetCancel(DataSet: TDataSet);
begin
  if DataSet.State in [dsEdit, dsInsert] then
  begin
    DataSet.UpdateRecord;
    if DataSet.Modified then
    begin
    end
    else
      DataSet.Cancel;
  end;
end;

function SetToBookmark(ADataSet: TDataSet; ABookmark: TBookmark): boolean;
begin
  Result := False;
  with ADataSet do
    if Active and (ABookmark <> nil) and not (Bof and EOF) and
      BookmarkValid(ABookmark) then
      try
        ADataSet.GotoBookmark(ABookmark);
        Result := True;
      except
      end;
end;

{ Refresh Query procedure }

procedure RefreshQuery(Query: TDataSet);
var
  BookMk: TBookmark;
begin
  with Query do
  begin
    DisableControls;
    try
      if Active then
        BookMk := GetBookmark
      else
        BookMk := nil;
      try
        Close;
        Open;
        SetToBookmark(Query, BookMk);
      finally
        if BookMk <> nil then
          FreeBookmark(BookMk);
      end;
    finally
      EnableControls;
    end;
  end;
end;

{ TLocateObject }

procedure TLocateObject.SetDataSet(Value: TDataSet);
begin
  ActiveChanged;
  FDataSet := Value;
end;

function TLocateObject.LocateFull: boolean;
begin
  Result := False;
  with DataSet do
  begin
    First;
    while not EOF do
    begin
      if MatchesLookup(FLookupField) then
      begin
        Result := True;
        Break;
      end;
      Next;
    end;
  end;
end;

function TLocateObject.LocateKey: boolean;
begin
  Result := False;
end;

function TLocateObject.FilterApplicable: boolean;
begin
  Result := FLookupField.FieldKind in [fkData, fkInternalCalc];
end;

function TLocateObject.LocateFilter: boolean;
var
  Options: TLocateOptions;
  Value: variant;
begin
  try
    Options := [];
    if not FCaseSensitive then
      Include(Options, loCaseInsensitive);
    if not FLookupExact then
      Include(Options, loPartialKey);
    if (FLookupValue = '') then
      Value := Null
    else
      Value := FLookupValue;
    Result := DataSet.Locate(FLookupField.FieldName, Value, Options);
  finally
  end;
end;

procedure TLocateObject.CheckFieldType(Field: TField);
begin
end;

function TLocateObject.Locate(const KeyField, KeyValue: ansistring;
  Exact, ACaseSensitive: boolean): boolean;
var
  LookupKey: TField;
begin
  if DataSet = nil then
  begin
    Result := False;
    Exit;
  end;
  DataSet.CheckBrowseMode;
  LookupKey := DataSet.FieldByName(KeyField);
  DataSet.CursorPosChanged;
  FLookupField := LookupKey;
  FLookupValue := KeyValue;
  FLookupExact := Exact;
  FCaseSensitive := ACaseSensitive;
  if FLookupField.DataType <> ftString then
  begin
    FCaseSensitive := True;
    try
      CheckFieldType(FLookupField);
    except
      Result := False;
      Exit;
    end;
  end;
  FBookmark := DataSet.GetBookmark;
  try
    DataSet.DisableControls;
    try
      Result := MatchesLookup(FLookupField);
      if not Result then
      begin
        if UseKey then
          Result := LocateKey
        else
          Result := LocateFull;
        if not Result then
          SetToBookmark(DataSet, FBookmark);
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    FLookupValue := EmptyStr;
    FLookupField := nil;
    DataSet.FreeBookmark(FBookmark);
    FBookmark := nil;
  end;
end;

function TLocateObject.UseKey: boolean;
begin
  Result := False;
end;

procedure TLocateObject.ActiveChanged;
begin
end;

function TLocateObject.MatchesLookup(Field: TField): boolean;
var
  Temp: ansistring;
begin
  Temp := Field.AsString;
  if not FLookupExact then
    SetLength(Temp, Min(Length(FLookupValue), Length(Temp)));
  if FCaseSensitive then
    Result := AnsiCompareStr(Temp, FLookupValue) = 0
  else
    Result := AnsiCompareText(Temp, FLookupValue) = 0;
end;

function CreateLocate(DataSet: TDataSet): TLocateObject;
begin
  if Assigned(CreateLocateObject) then
    Result := CreateLocateObject()
  else
    Result := TLocateObject.Create;
  if (Result <> nil) and (DataSet <> nil) then
    Result.DataSet := DataSet;
end;

{ DataSet locate routines }

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: ansistring;
  const KeyValues: variant; Options: TLocateOptions): boolean;
var
  FieldCount: integer;
  Fields: TList;
  ABookmark: TBookmark;

  function CompareField(Field: TField; Value: variant): boolean;
  var
    S, S1: ansistring;

  begin
    if Field.DataType = ftString then
    begin
      S := Field.AsString;
      S1 := Value;
      if (loPartialKey in Options) then
        Delete(S, Length(S1) + 1, MaxInt);

      if (loCaseInsensitive in Options) then
        Result := AnsiCompareText(S, S1) = 0
      else
        Result := AnsiCompareStr(S, S1) = 0;
    end
    else
      Result := (Field.Value = Value);
  end;

  function CompareRecord: boolean;
  var
    I: integer;
  begin
    if FieldCount = 1 then
      Result := CompareField(TField(Fields.First), KeyValues)
    else
    begin
      Result := True;
      for I := 0 to FieldCount - 1 do
        Result := Result and CompareField(TField(Fields[I]), KeyValues[I]);
    end;
  end;

begin
  Result := False;
  with DataSet do
  begin
    CheckBrowseMode;
    if Bof and EOF then
      Exit;
  end;
  Fields := TList.Create;
  try
    DataSet.GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Result := CompareRecord;
    if Result then
      Exit;
    DataSet.DisableControls;
    try
      ABookmark := Pointer(DataSet.Bookmark);
      try
        with DataSet do
        begin
          First;
          while not EOF do
          begin
            Result := CompareRecord;
            if Result then
              Break;
            Next;
          end;
        end;
      finally
        if not Result and DataSet.BookmarkValid(ABookmark) then
{$IFDEF OverRad2k7}
          DataSet.Bookmark := ABookmark;
{$ELSE}
        DataSet.Bookmark := PAnsiChar(ABookmark);
{$ENDIF}
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    Fields.Free;
  end;
end;

procedure SaveFields(DataSet: TDataSet; IniFile: TIniFile);
begin
  InternalSaveFields(DataSet, IniFile, DataSetSectionName(DataSet));
end;

procedure RestoreFields(DataSet: TDataSet; IniFile: TIniFile; RestoreVisible: boolean);
begin
  InternalRestoreFields(DataSet, IniFile, DataSetSectionName(DataSet),
    RestoreVisible);
end;

procedure SaveFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile);
begin
  InternalSaveFields(DataSet, IniFile, DataSetSectionName(DataSet));
end;

procedure RestoreFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile;
  RestoreVisible: boolean);
begin
  InternalRestoreFields(DataSet, IniFile, DataSetSectionName(DataSet),
    RestoreVisible);
end;

{ DataSetSortedSearch. Navigate on sorted DataSet routine. }

function DataSetSortedSearch(DataSet: TDataSet; const Value, FieldName: ansistring;
  CaseInsensitive: boolean): boolean;
var
  L, H, I: longint;
  CurrentPos: longint;
  CurrentValue: ansistring;
  BookMk: TBookmark;
  Field: TField;

  function UpStr(const Value: ansistring): ansistring;
  begin
    if CaseInsensitive then
      Result := AnsiUpperCase(Value)
    else
      Result := Value;
  end;

  function GetCurrentStr: ansistring;
  begin
    Result := Field.AsString;
    if Length(Result) > Length(Value) then
      SetLength(Result, Length(Value));
    Result := UpStr(Result);
  end;

begin
  Result := False;
  if DataSet = nil then
    Exit;
  Field := DataSet.FindField(FieldName);
  if Field = nil then
    Exit;
  if Field.DataType = ftString then
  begin
    DataSet.DisableControls;
    BookMk := DataSet.GetBookmark;
    try
      L := 0;
      DataSet.First;
      CurrentPos := 0;
      H := DataSet.RecordCount - 1;
      if Value <> '' then
      begin
        while L <= H do
        begin
          I := (L + H) shr 1;
          if I <> CurrentPos then
            DataSet.MoveBy(I - CurrentPos);
          CurrentPos := I;
          CurrentValue := GetCurrentStr;
          if (UpStr(Value) > CurrentValue) then
            L := I + 1
          else
          begin
            H := I - 1;
            if (UpStr(Value) = CurrentValue) then
              Result := True;
          end;
        end; { while }
        if Result then
        begin
          if (L <> CurrentPos) then
            DataSet.MoveBy(L - CurrentPos);
          while (L < DataSet.RecordCount) and (UpStr(Value) <> GetCurrentStr) do
          begin
            Inc(L);
            DataSet.MoveBy(1);
          end;
        end;
      end
      else
        Result := True;
      if not Result then
        SetToBookmark(DataSet, BookMk);
    finally
      DataSet.FreeBookmark(BookMk);
      DataSet.EnableControls;
    end;
  end
  else
    DatabaseErrorFmt(SFieldTypeMismatch, [Field.DisplayName]);
end;

{ Save and restore DataSet Fields layout }

function DataSetSectionName(DataSet: TDataSet): ansistring;
begin
  with DataSet do
    if (Owner <> nil) and (Owner is TCustomForm) then
      Result := GetDefaultSection(Owner as TCustomForm)
    else
      Result := Name;
end;

function CheckSection(DataSet: TDataSet; const Section: ansistring): ansistring;
begin
  Result := Section;
  if Result = '' then
    Result := DataSetSectionName(DataSet);
end;

procedure InternalSaveFields(DataSet: TDataSet; IniFile: TObject;
  const Section: ansistring);
var
  I: integer;
begin
  with DataSet do
  begin
    for I := 0 to FieldCount - 1 do
    begin
      IniWriteString(IniFile, CheckSection(DataSet, Section),
        Name + Fields[I].FieldName,
        Format('%d,%d,%d', [Fields[I].Index, Fields[I].DisplayWidth,
        integer(Fields[I].Visible)]));
    end;
  end;
end;

procedure InternalRestoreFields(DataSet: TDataSet; IniFile: TObject;
  const Section: ansistring; RestoreVisible: boolean);
type
  TFieldInfo = packed record
    Field: TField;
    EndIndex: integer;
  end;

  PFieldArray = ^TFieldArray;
  TFieldArray = array [0 .. (65528 div SizeOf(TFieldInfo)) - 1] of TFieldInfo;
const
  Delims = [' ', ','];
begin

end;

function IsDataSetEmpty(DataSet: TDataSet): boolean;
begin
  with DataSet do
    Result := (not Active) or (EOF and Bof);
end;

{ SQL expressions }

function DateToSQL(Value: TDateTime): ansistring;
begin
  Result := IntToStr(Trunc(Value));
end;

function FormatSQLDateRange(Date1, Date2: TDateTime;
  const FieldName: ansistring): ansistring;
begin
  Result := TrueExpr;
  if (Date1 = Date2) and (Date1 <> NullDate) then
  begin
    Result := Format('%s = %s', [FieldName, FormatDateTime(ServerDateFmt, Date1)]);
  end
  else if (Date1 <> NullDate) or (Date2 <> NullDate) then
  begin
    if Date1 = NullDate then
      Result := Format('%s < %s', [FieldName,
        FormatDateTime(ServerDateFmt, IncDay(Date2, 1))])
    else if Date2 = NullDate then
      Result := Format('%s > %s', [FieldName,
        FormatDateTime(ServerDateFmt, IncDay(Date1, -1))])
    else
      Result := Format('(%s < %s) AND (%s > %s)', [FieldName,
        FormatDateTime(ServerDateFmt, IncDay(Date2, 1)), FieldName,
        FormatDateTime(ServerDateFmt, IncDay(Date1, -1))]);
  end;
end;

function FormatSQLDateRangeEx(Date1, Date2: TDateTime;
  const FieldName: ansistring): ansistring;
begin
  Result := TrueExpr;
  if (Date1 <> NullDate) or (Date2 <> NullDate) then
  begin
    if Date1 = NullDate then
      Result := Format('%s < %s', [FieldName,
        FormatDateTime(ServerDateFmt, IncDay(Date2, 1))])
    else if Date2 = NullDate then
      Result := Format('%s >= %s', [FieldName,
        FormatDateTime(ServerDateFmt, Date1)])
    else
      Result := Format('(%s < %s) AND (%s >= %s)',
        [FieldName, FormatDateTime(ServerDateFmt, IncDay(Date2, 1)),
        FieldName, FormatDateTime(ServerDateFmt, Date1)]);
  end;
end;

function FormatSQLNumericRange(const FieldName: ansistring;
  LowValue, HighValue, LowEmpty, HighEmpty: double; Inclusive: boolean): ansistring;
const
  Operators: array [boolean, 1 .. 2] of string[2] = (('>', '<'), ('>=', '<='));
begin
  Result := TrueExpr;
  if (LowValue = HighValue) and (LowValue <> LowEmpty) then
  begin
    Result := Format('%s = %g', [FieldName, LowValue]);
  end
  else if (LowValue <> LowEmpty) or (HighValue <> HighEmpty) then
  begin
    if LowValue = LowEmpty then
      Result := Format('%s %s %g', [FieldName, Operators[Inclusive, 2],
        HighValue])
    else if HighValue = HighEmpty then
      Result := Format('%s %s %g', [FieldName, Operators[Inclusive, 1], LowValue])
    else
    begin
      Result := Format('(%s %s %g) AND (%s %s %g)',
        [FieldName, Operators[Inclusive, 2], HighValue, FieldName,
        Operators[Inclusive, 1], LowValue]);
    end;
  end;
end;

function StrMaskSQL(const Value: ansistring): ansistring;
begin
  if (Pos('*', Value) = 0) and (Pos('?', Value) = 0) and (Value <> '') then
    Result := '*' + Value + '*'
  else
    Result := Value;
end;

function FormatSQLCondition(const FieldName, AOperator, Value: ansistring;
  FieldType: TFieldType; Exact: boolean): ansistring;
var
  EmptyValue: boolean;
  FieldValue: ansistring;
  DateValue: TDateTime;
  LogicOperator: ansistring;
begin
  FieldValue := '';
  DateValue := NullDate;
  Exact := Exact or not (FieldType in [ftString, ftDate, ftTime, ftDateTime]);
  if FieldType in [ftDate, ftTime, ftDateTime] then
  begin
    DateValue := StrToDateDef(Value, NullDate);
    EmptyValue := (DateValue = NullDate);
    FieldValue := FormatDateTime(ServerDateFmt, DateValue);
  end
  else
  begin
    FieldValue := Value;
    EmptyValue := FieldValue = '';
    if not (Exact or EmptyValue) then
      FieldValue := ReplaceStr(ReplaceStr(StrMaskSQL(FieldValue), '*', '%'),
        '?', '_');
    if FieldType = ftString then
      FieldValue := '''' + FieldValue + '''';
  end;
  LogicOperator := AOperator;
  if LogicOperator = '' then
  begin
    if Exact then
      LogicOperator := '='
    else
    begin
      if FieldType = ftString then
        LogicOperator := 'LIKE'
      else
        LogicOperator := '>=';
    end;
  end;
  if EmptyValue then
    Result := TrueExpr
  else if (FieldType = ftDateTime) and Exact then
  begin
    DateValue := IncDay(DateValue, 1);
    Result := Format('(%s >= %s) and (%s < %s)', [FieldName,
      FieldValue, FieldName, FormatDateTime(ServerDateFmt, DateValue)]);

  end
  else
    Result := Format('%s %s %s', [FieldName, LogicOperator, FieldValue]);
end;

function FormatAnsiSQLCondition(const FieldName, AOperator, Value: ansistring;
  FieldType: TFieldType; Exact: boolean): ansistring;
var
  S, Esc: ansistring;
begin
  Esc := '';
  if not Exact and (FieldType = ftString) then
  begin
    S := ReplaceStr(ReplaceStr(ReplaceStr(Value, '/', '//'), '_', '/_'), '%', '/%');
    if S <> Value then
      Esc := ' ESCAPE''/''';
  end
  else
    S := Value;
  Result := FormatSQLCondition(FieldName, AOperator, S, FieldType, Exact) + Esc;
end;

procedure CheckRequiredField(Field: TField);
begin
  with Field do
    if not ReadOnly and not Calculated and IsNull then
    begin
      FocusControl;
      DatabaseErrorFmt(SFieldRequired, [DisplayName]);
    end;
end;

procedure CheckRequiredFields(const Fields: array of TField);
var
  I: integer;
begin
  for I := Low(Fields) to High(Fields) do
    CheckRequiredField(Fields[I]);
end;

procedure AssignRecord(Source, Dest: TDataSet; ByName: boolean);
var
  I: integer;
  F, FSrc: TField;
begin
  if not (Dest.State in dsEditModes) then
    _DBError(SNotEditing);
  if ByName then
  begin
    for I := 0 to Source.FieldCount - 1 do
    begin
      F := Dest.FindField(Source.Fields[I].FieldName);
      if F <> nil then
      begin
        if (F.DataType = Source.Fields[I].DataType) and
          (F.DataSize = Source.Fields[I].DataSize) then
          F.Assign(Source.Fields[I])
        else
          F.AsString := Source.Fields[I].AsString;
      end;
    end;
  end
  else
  begin
    for I := 0 to Min(Source.FieldDefs.Count - 1, Dest.FieldDefs.Count - 1) do
    begin
      F := Dest.FindField(Dest.FieldDefs[I].Name);
      FSrc := Source.FindField(Source.FieldDefs[I].Name);
      if (F <> nil) and (FSrc <> nil) then
      begin
        if F.DataType = FSrc.DataType then
          F.Assign(FSrc)
        else
          F.AsString := FSrc.AsString;
      end;
    end;
  end;
end;

function ExtractFieldName(const Fields: ansistring; var Pos: integer): ansistring;
var
  I: integer;
begin
  I := Pos;
  while (I <= Length(Fields)) and (Fields[I] <> ';') do
    Inc(I);
  Result := Trim(Copy(Fields, Pos, I - Pos));
  if (I <= Length(Fields)) and (Fields[I] = ';') then
    Inc(I);
  Pos := I;
end;

procedure FillValueForField(const Field: TField; Value: variant);
var
  DS: TDataSet;
  P: TBookmark;
begin
  DS := Field.DataSet;
  DS.DisableControls;
  P := Pointer(DS.Bookmark);
  try
    DS.First;
    while not DS.EOF do
    begin
      DS.Edit;
      Field.Value := Value;
      DS.Post;
      DS.Next;
    end;
  finally
{$IFDEF OverRad2k7}
    DS.Bookmark := P;
{$ELSE}
    DS.Bookmark := PAnsiChar(P);
{$ENDIF}
    DS.EnableControls;
  end;
end;

end.
