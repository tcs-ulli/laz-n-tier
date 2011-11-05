{*******************************************************}

{         Delphi VCL Extensions (RX)                    }

{         Copyright (c) 1998 Master-Bank                }

{*******************************************************}

unit MemDataBase;

interface

uses SysUtils, Classes, Controls, DB, MemDBUtils;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

type
  TMemBlobData = AnsiString;
  TMemBlobArray = array[0..0] of TMemBlobData;
  TBlobDataArrayType = ^TMemBlobArray;
  TBlobDataType = TMemBlobData;
  PMemBlobArray = ^TMemBlobArray;
  TMemoryRecord = class;
  TLoadMode = (lmCopy, lmAppend);
  TCompareRecords = function(Item1, Item2: TMemoryRecord): integer of object;
  TBookmarkData = integer;
  PMemBookmarkInfo = ^TMemBookmarkInfo;

  TMemBookmarkInfo = packed record
    BookmarkData: TBookmarkData;
    BookmarkFlag: TBookmarkFlag;
  end;

  TMemDB = class(TDataSet)
  private
    FOldBuf: TBuffer;
    FRecordPos: integer;
    FRecordSize: integer;
    FBookmarkOfs: integer;
    FBlobOfs: integer;
    FRecBufSize: integer;
    FOffsets: PWordArray;
    FLastID: integer;
    FAutoInc: LongInt;
    FActive: Boolean;
    FRecords: TList;
    FIndexList: TList;
    FCaseInsensitiveSort: Boolean;
    FDescendingSort: Boolean;
    function AddRecord: TMemoryRecord;
    function InsertRecord(Index: integer): TMemoryRecord;
    function FindRecordID(ID: integer): TMemoryRecord;
    procedure CreateIndexList(const FieldNames: AnsiString);
    procedure FreeIndexList;
    procedure QuickSort(L, R: integer; Compare: TCompareRecords);
    procedure Sort;
    function CalcRecordSize: integer;
    function FindFieldData(Buffer: Pointer; Field: TField): Pointer;
    function GetMemoryRecord(Index: integer): TMemoryRecord;
    function GetCapacity: integer;
    function RecordFilter: Boolean;
    procedure SetCapacity(Value: integer);
    procedure ClearRecords;
    procedure InitBufferPointers(GetProps: Boolean);
  protected
    procedure AssignMemoryRecord(Rec: TMemoryRecord; Buffer: TBuffer);
    function GetActiveRecBuf(var RecBuf: TBuffer): Boolean; virtual;
    procedure InitFieldDefsFromFields;
    procedure RecordToBuffer(Rec: TMemoryRecord; Buffer: TBuffer);
    procedure SetMemoryRecordData(Buffer: TBuffer; Pos: integer); virtual;
    procedure SetAutoIncFields(Buffer: TBuffer); virtual;
    function CompareRecords(Item1, Item2: TMemoryRecord): integer; virtual;
    function GetBlobData(Field: TField; Buffer: TBuffer): TBlobDataType;
    procedure SetBlobData(Field: TField; Buffer: TBuffer; Value: TBlobDataType); 
    function AllocRecordBuffer: TBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TBuffer); override;
    function BCDToCurr(BCD: Pointer; var Curr: currency): Boolean;
    function CurrToBCD(const Curr: currency; BCD: Pointer;
      Precision, Decimals: integer): Boolean;
    procedure InternalInitRecord(Buffer: TBuffer); override;
    procedure ClearCalcFields(Buffer: TBuffer); override;
    function GetRecord(Buffer: TBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: word; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure CloseBlob(Field: TField); override;
    procedure GetBookmarkData(Buffer: TBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TBuffer): TBookmarkFlag; override;
    procedure InternalGotoBookmark(aBookmark: TBookmarkPointerType); override;
    procedure InternalSetToRecord(Buffer: TBuffer);override;
    procedure SetBookmarkFlag(Buffer: TBuffer; Value: TBookmarkFlag);override;
    procedure SetBookmarkData(Buffer: TBuffer; Data: Pointer); override;
    function GetIsIndexField(Field: TField): Boolean; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalEdit; override;
    procedure InitRecord(Buffer: TBuffer);
    procedure InternalAddRecord(Buffer: Pointer; aAppend: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalCancel; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    function IsCursorOpen: Boolean; override;
    function GetRecordCount: integer; override;
    function GetRecNo: integer; override;
    procedure SetRecNo(Value: integer); override;
    property Records[Index: integer]: TMemoryRecord read GetMemoryRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(aBookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): integer;
      override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
      override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetCurrentRecord(Buffer: TBuffer): Boolean;
    function IsSequenced: Boolean;
    function Locate(const KeyFields: string; const KeyValues: variant;
      Options: TLocateOptions): Boolean; override;
    procedure SortOnFields(const FieldNames: AnsiString;
      CaseInsensitive: Boolean = True; Descending: Boolean = False);
    procedure EmptyTable;
    procedure CopyStructure(Source: TDataSet);
    function LoadFromDataSet(Source: TDataSet; aRecordCount: integer;
      Mode: TLoadMode): integer;
    function SaveToDataSet(Dest: TDataSet; aRecordCount: integer): integer;
    property Capacity: integer read GetCapacity write SetCapacity default 0;
  published
    property Active;
    property AutoCalcFields;
    property Filtered;
    property FieldDefs;
{$IFNDEF FPC}
    property ObjectView default False;
{$ENDIF}
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

  { TMemBlobStream }

  TMemBlobStream = class(TStream)
  private
    FField: TBlobField;
    FDataSet: TMemDB;
    FBuffer: TBuffer;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: LongInt;
    FCached: Boolean;
    function GetBlobSize: LongInt;
    function GetBlobFromRecord(Field: TField): TMemBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: word): LongInt; override;
    procedure Truncate;
  end;

  { TMemoryRecord }

  TMemoryRecord = class(TPersistent)
  private
    FMemoryData: TMemDB;
    FID: integer;
    FData: Pointer;
    FBlobs: Pointer;
    function GetIndex: integer;
    procedure SetMemoryData(Value: TMemDB; UpdateParent: Boolean);
  protected
    procedure SetIndex(Value: integer); virtual;
  public
    constructor Create(MemoryData: TMemDB); virtual;
    constructor CreateEx(MemoryData: TMemDB; UpdateParent: Boolean); virtual;
    destructor Destroy; override;
    property MemoryData: TMemDB read FMemoryData;
    property ID: integer read FID write FID;
    property Index: integer read GetIndex write SetIndex;
    property Data: Pointer read FData;
  end;

implementation

//uses Forms, Variants;

{$I MemDBRADv.inc}

resourcestring
  SMemNoRecords = 'No data found';
  SInvalidFields = 'No fields defined';

const
  ftBlobTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
    ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, ftLargeint,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant,
    ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftFixedWideChar,
    ftWideMemo];

  ftSupported = [ftString, ftSmallint, ftInteger, ftWord, ftBoolean,
    ftFloat, ftCurrency, ftDate, ftTime, ftDateTime, ftAutoInc, ftBCD,
    ftBytes, ftVarBytes, ftADT, ftFixedChar, ftWideString, ftLargeint,
    ftVariant, ftGuid] + ftBlobTypes;

  fkStoredFields = [fkData];

  GuidSize = 38;

  { Utility routines }

procedure FinalizeBlobFields(BlobArray: PMemBlobArray; BlobFieldCount: integer);
var
  i: integer;
begin
  for i := 0 to BlobFieldCount - 1 do
    BlobArray^[i] := '';
end;

function CompareFields(Data1, Data2: Pointer; FieldType: TFieldType;
  CaseInsensitive: Boolean): integer;
begin
  Result := 0;
  case FieldType of
    ftString:
      if CaseInsensitive then
        Result := AnsiCompareText(PAnsiChar(Data1), PAnsiChar(Data2))
      else
        Result := AnsiCompareStr(PAnsiChar(Data1), PAnsiChar(Data2));
    ftSmallint:
      if smallint(Data1^) > smallint(Data2^) then
        Result := 1
      else if smallint(Data1^) < smallint(Data2^) then
        Result := -1;
    ftInteger, ftDate, ftTime, ftAutoInc:
      if LongInt(Data1^) > LongInt(Data2^) then
        Result := 1
      else if LongInt(Data1^) < LongInt(Data2^) then
        Result := -1;
    ftWord:
      if word(Data1^) > word(Data2^) then
        Result := 1
      else if word(Data1^) < word(Data2^) then
        Result := -1;
    ftBoolean:
      if wordBool(Data1^) and not wordBool(Data2^) then
        Result := 1
      else if not wordBool(Data1^) and wordBool(Data2^) then
        Result := -1;
    ftFloat, ftCurrency:
      if double(Data1^) > double(Data2^) then
        Result := 1
      else if double(Data1^) < double(Data2^) then
        Result := -1;
    ftDateTime:
      if TDateTime(Data1^) > TDateTime(Data2^) then
        Result := 1
      else if TDateTime(Data1^) < TDateTime(Data2^) then
        Result := -1;
    ftFixedChar:
      if CaseInsensitive then
        Result := AnsiCompareText(PAnsiChar(Data1), PAnsiChar(Data2))
      else
        Result := AnsiCompareStr(PAnsiChar(Data1), PAnsiChar(Data2));
    ftWideString:
      if CaseInsensitive then
        Result := AnsiCompareText(WideCharToString(PWideChar(Data1)),
          WideCharToString(PWideChar(Data2)))
      else
        Result := AnsiCompareStr(WideCharToString(PWideChar(Data1)),
          WideCharToString(PWideChar(Data2)));
    ftLargeint:
      if int64(Data1^) > int64(Data2^) then
        Result := 1
      else if int64(Data1^) < int64(Data2^) then
        Result := -1;
    ftVariant:
      Result := 0;
    ftGuid:
      Result := AnsiCompareText(PAnsiChar(Data1), PAnsiChar(Data2));
  end;
end;

function CalcFieldLen(FieldType: TFieldType; Size: word): word;
begin
  if not (FieldType in ftSupported) then
    Result := 0
  else if (FieldType in ftBlobTypes) then
    Result := SizeOf(LongInt)
  else
  begin
    Result := Size;
    case FieldType of
      ftString: Inc(Result);
      ftSmallint: Result := SizeOf(smallint);
      ftInteger: Result := SizeOf(LongInt);
      ftWord: Result := SizeOf(word);
      ftBoolean: Result := SizeOf(wordBool);
      ftFloat: Result := SizeOf(double);
      ftCurrency: Result := SizeOf(double);
      ftBCD: Result := 34;
      ftDate, ftTime: Result := SizeOf(LongInt);
      ftDateTime: Result := SizeOf(TDateTime);
      ftBytes: Result := Size;
      ftVarBytes: Result := Size + 2;
      ftAutoInc: Result := SizeOf(LongInt);
      ftADT: Result := 0;
      ftFixedChar: Inc(Result);
      ftWideString: Result := (Result + 1) * 2;
      ftLargeint: Result := SizeOf(int64);
      ftVariant: Result := SizeOf(variant);
      ftGuid: Result := GuidSize + 1;
    end;
  end;
end;

procedure CalcDataSize(FieldDef: TFieldDef; var DataSize: integer);
begin
  with FieldDef do
  begin
    if (DataType in ftSupported - ftBlobTypes) then
      Inc(DataSize, CalcFieldLen(DataType, Size) + 1);
{$IFDEF ENABLE_Child_Defs}
    for I := 0 to ChildDefs.Count - 1 do
      CalcDataSize(ChildDefs[I], DataSize);
{$ENDIF}
  end;
end;

procedure Error(const Msg: AnsiString);
begin
  DatabaseError(Msg);
end;

procedure ErrorFmt(const Msg: AnsiString; const Args: array of const);
begin
  DatabaseErrorFmt(Msg, Args);
end;

{ TMemoryRecord }

constructor TMemoryRecord.Create(MemoryData: TMemDB);
begin
  CreateEx(MemoryData, True);
end;

constructor TMemoryRecord.CreateEx(MemoryData: TMemDB; UpdateParent: Boolean);
begin
  inherited Create;
  SetMemoryData(MemoryData, UpdateParent);
end;

destructor TMemoryRecord.Destroy;
begin
  SetMemoryData(nil, True);
  inherited Destroy;
end;

function TMemoryRecord.GetIndex: integer;
begin
  if FMemoryData <> nil then
    Result := FMemoryData.FRecords.IndexOf(Self)
  else
    Result := -1;
end;

procedure TMemoryRecord.SetMemoryData(Value: TMemDB; UpdateParent: Boolean);
var
  I: integer;
  DataSize: integer;
begin
  if FMemoryData <> Value then
  begin
    if FMemoryData <> nil then
    begin
      FMemoryData.FRecords.Remove(Self);
      if FMemoryData.BlobFieldCount > 0 then
{$IFDEF FPC}
        Finalize(PMemBlobArray(FBlobs)^[0], FMemoryData.BlobFieldCount);
{$ELSE}
        Finalize(PMemBlobArray(FBlobs)[0], FMemoryData.BlobFieldCount);
{$ENDIF}
      ReallocMem(FBlobs, 0);
      ReallocMem(FData, 0);
      FMemoryData := nil;
    end;
    if Value <> nil then
    begin
      if UpdateParent then
      begin
        Value.FRecords.Add(Self);
        Inc(Value.FLastID);
        FID := Value.FLastID;
      end;
      FMemoryData := Value;
      if Value.BlobFieldCount > 0 then
      begin
        ReallocMem(FBlobs, Value.BlobFieldCount * SizeOf(Pointer));
{$IFDEF FPC}
        Initialize(PMemBlobArray(FBlobs)^[0]);//, Value.BlobFieldCount);  Maybe work on Win64 2011-11-01
{$ELSE}
        Initialize(PMemBlobArray(FBlobs)[0], Value.BlobFieldCount);
{$ENDIF}
      end;
      DataSize := 0;
      for I := 0 to Value.FieldDefs.Count - 1 do
        CalcDataSize(Value.FieldDefs[I], DataSize);
      ReallocMem(FData, DataSize);
    end;
  end;
end;

procedure TMemoryRecord.SetIndex(Value: integer);
var
  CurIndex: integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    FMemoryData.FRecords.Move(CurIndex, Value);
end;

{ TMemDB }

constructor TMemDB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecordPos := -1;
  FLastID := Low(integer);
  FAutoInc := 1;
  FRecords := TList.Create;
  FOldBuf := nil;
end;

destructor TMemDB.Destroy;
begin
  inherited Destroy;
  FreeIndexList;
  ClearRecords;
  FRecords.Free;
  ReallocMem(FOffsets, 0);
end;

{ Records Management }

function TMemDB.GetCapacity: integer;
begin
  if FRecords <> nil then
    Result := FRecords.Capacity
  else
    Result := 0;
end;

procedure TMemDB.SetCapacity(Value: integer);
begin
  if FRecords <> nil then
    FRecords.Capacity := Value;
end;

function TMemDB.AddRecord: TMemoryRecord;
begin
  Result := TMemoryRecord.Create(Self);
end;

function TMemDB.FindRecordID(ID: integer): TMemoryRecord;
var
  I: integer;
begin
  for I := 0 to FRecords.Count - 1 do
  begin
    Result := TMemoryRecord(FRecords[I]);
    if Result.ID = ID then
      Exit;
  end;
  Result := nil;
end;

function TMemDB.InsertRecord(Index: integer): TMemoryRecord;
begin
  Result := AddRecord;
  Result.Index := Index;
end;

procedure TMemDB.InternalEdit;
begin
  inherited InternalEdit;
  GetMem(FOldBuf, FRecBufSize);
  Move(ActiveBuffer^, FOldBuf^, FRecBufSize);
end;

function TMemDB.GetMemoryRecord(Index: integer): TMemoryRecord;
begin
  Result := TMemoryRecord(FRecords[Index]);
end;

{ Field Management }

function TMemDB.BCDToCurr(BCD: Pointer; var Curr: currency): Boolean;
begin
  Move(BCD^, Curr, SizeOf(currency));
  Result := True;
end;

function TMemDB.CurrToBCD(const Curr: currency; BCD: Pointer;
  Precision, Decimals: integer): Boolean;
begin
  Move(Curr, BCD^, SizeOf(currency));
  Result := True;
end;

procedure TMemDB.InitFieldDefsFromFields;
var
  I: Integer;
  Offset: Word;
  FD: TFieldDef;
begin
  if FieldDefs.Count = 0 then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      with Fields[I] do
        if (FieldKind in fkStoredFields) and not (DataType in ftSupported) then
          ErrorFmt(SUnknownFieldType, [DisplayName]);
    end;
    FreeIndexList;
  end;
  Offset := 0;
  if FieldDefs.Count = 0 then
    for I := 0 to FieldCount - 1 do
    begin
      FD := FieldDefs.AddFieldDef;
      FD.Name := Fields[I].FieldName;
      FD.Size := Fields[I].Size;
      FD.DataType := Fields[I].DataType;
      if Fields[I].Required then
        FD.Attributes := FD.Attributes + [faRequired];
      if Fields[I] is TFloatField then
        FD.Precision := TFloatField(Fields[I]).Precision;
    end;
  { Calculate fields offsets }
  ReallocMem(FOffsets, FieldDefs.Count * SizeOf(Word));
  for I := 0 to FieldDefs.Count - 1 do
  begin
    FOffsets^[I] := Offset;
    with FieldDefs[I] do
    begin
      if (DataType in ftSupported - ftBlobTypes) then
        Inc(Offset, CalcFieldLen(DataType, Size) + 1);
    end;
  end;
end;

function TMemDB.FindFieldData(Buffer: Pointer; Field: TField): Pointer;
var
  Index: integer;
begin
{$IFNDEF FPC}
  Index := FieldDefList.IndexOf(Field.FullName);
  if (Index >= 0) and (Buffer <> nil) and (FieldDefList[Index].DataType in
    ftSupported - ftBlobTypes) then
    Result := (PAnsiChar(Buffer) + FOffsets[Index])
  else
    Result := nil;
{$ELSE}
  Index := FieldDefs.IndexOf(Field.FieldName);
  if (Index >= 0) and (Buffer <> nil) and (FieldDefs[Index].DataType in
    ftSupported - ftBlobTypes) then
    Result := (PAnsiChar(Buffer) + FOffsets^[Index])
  else
    Result := nil;
{$ENDIF}
end;

{ Buffer Manipulation }

function TMemDB.CalcRecordSize: integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to FieldDefs.Count - 1 do
    CalcDataSize(FieldDefs[I], Result);
end;

procedure TMemDB.InitBufferPointers(GetProps: Boolean);
begin
  if GetProps then
    FRecordSize := CalcRecordSize;
  FBookmarkOfs := FRecordSize + CalcFieldsSize;
  FBlobOfs := FBookmarkOfs + SizeOf(TMemBookmarkInfo);
  FRecBufSize := FBlobOfs + BlobFieldCount * SizeOf(Pointer);
end;

procedure TMemDB.ClearRecords;
begin
  while FRecords.Count > 0 do
    TObject(FRecords.Last).Free;
  FLastID := Low(integer);
  FRecordPos := -1;
end;

function TMemDB.AllocRecordBuffer: TBuffer;
begin
{$IFDEF OverRad2k7}
  Result := AllocMem(FRecBufSize);
{$ELSE}
  Result := StrAlloc(FRecBufSize);
  if BlobFieldCount > 0 then
{$IFDEF FPC}
    Initialize(PMemBlobArray(Result + FBlobOfs)^[0]); //, BlobFieldCount);  Maybe work on Win64 2011-11-01
{$ELSE}
    Initialize(PMemBlobArray(Result + FBlobOfs)[0], BlobFieldCount);
{$ENDIF}
{$ENDIF}
end;

procedure TMemDB.FreeRecordBuffer(var Buffer: TBuffer);
begin
{$IFDEF OverRad2k7}
  FreeMem(Buffer);
{$ELSE}
  if BlobFieldCount > 0 then
{$IFDEF FPC}
    Finalize(PMemBlobArray(Buffer + FBlobOfs)^[0], BlobFieldCount);
{$ELSE}
    Finalize(PMemBlobArray(Buffer + FBlobOfs)[0], BlobFieldCount);
{$ENDIF}
  StrDispose(Buffer);
{$ENDIF}
  Buffer := nil;
end;

procedure TMemDB.ClearCalcFields(Buffer: TBuffer);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

procedure TMemDB.InternalInitRecord(Buffer: TBuffer);
var
  I: integer;
begin
{$IFDEF FPC}
  FillChar(Buffer^, FBlobOfs, 0);
  for I := 0 to BlobFieldCount - 1 do
    PMemBlobArray(Buffer + FBlobOfs)^[I] := '';
{$ELSE}
  FillChar(Buffer^, FBlobOfs, 0);
  for I := 0 to BlobFieldCount - 1 do
    PMemBlobArray(Buffer + FBlobOfs)[I] := '';
{$ENDIF}
end;

procedure TMemDB.InitRecord(Buffer: TBuffer);
begin
  InternalInitRecord(Buffer);
  ClearCalcFields(Buffer);
  SetBookmarkFlag(Buffer, bfInserted);
  with PMemBookmarkInfo(Buffer + FBookmarkOfs)^ do
  begin
    BookmarkData := Low(integer);
    BookmarkFlag := bfInserted;
  end;
end;

function TMemDB.GetCurrentRecord(Buffer: TBuffer): Boolean;
begin
  Result := False;
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      Move(Records[FRecordPos].Data^, Buffer^, FRecordSize);
      Result := True;
    end;
  end;
end;

procedure TMemDB.RecordToBuffer(Rec: TMemoryRecord; Buffer: TBuffer);
var
  I: integer;
begin
  Move(Rec.Data^, Buffer^, FRecordSize);
  with PMemBookmarkInfo(Buffer + FBookmarkOfs)^ do
  begin
    BookmarkData := Rec.ID;
    BookmarkFlag := bfCurrent;
  end;
  for I := 0 to BlobFieldCount - 1 do
{$IFDEF FPC}
    PMemBlobArray(Buffer + FBlobOfs)^[I] := PMemBlobArray(Rec.FBlobs)^[I];
{$ELSE}
    PMemBlobArray(Buffer + FBlobOfs)[I] := PMemBlobArray(Rec.FBlobs)[I];
{$ENDIF}
  GetCalcFields(Buffer);
end;

function TMemDB.GetRecord(Buffer: TBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
begin
  Result := grOk;
  Accept := True;
  case GetMode of
    gmPrior:
      if FRecordPos <= 0 then
      begin
        Result := grBOF;
        FRecordPos := -1;
      end
      else
      begin
        repeat
          Dec(FRecordPos);
          if Filtered then
            Accept := RecordFilter;
        until Accept or (FRecordPos < 0);
        if not Accept then
        begin
          Result := grBOF;
          FRecordPos := -1;
        end;
      end;
    gmCurrent:
      if (FRecordPos < 0) or (FRecordPos >= RecordCount) then
        Result := grError
      else if Filtered then
      begin
        if not RecordFilter then
          Result := grError;
      end;
    gmNext:
      if FRecordPos >= RecordCount - 1 then
        Result := grEOF
      else
      begin
        repeat
          Inc(FRecordPos);
          if Filtered then
            Accept := RecordFilter;
        until Accept or (FRecordPos > RecordCount - 1);
        if not Accept then
        begin
          Result := grEOF;
          FRecordPos := RecordCount - 1;
        end;
      end;
  end;
  if Result = grOk then
    RecordToBuffer(Records[FRecordPos], Buffer)
  else if (Result = grError) and DoCheck then
    Error(SMemNoRecords);
end;

function TMemDB.GetRecordSize: word;
begin
  Result := FRecordSize;
end;

function TMemDB.GetActiveRecBuf(var RecBuf: TBuffer): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := ActiveBuffer;
    dsEdit, dsInsert: RecBuf := ActiveBuffer;
    dsCalcFields: RecBuf := CalcBuffer;
    dsOldValue:
      begin
        if FOldBuf = nil then
          RecBuf := ActiveBuffer
        else
          RecBuf := FOldBuf;
      end;
    dsFilter: RecBuf := TempBuffer;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TMemDB.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf, Data: TBuffer;
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then
    Exit;
  if Field.FieldNo > 0 then
  begin
    Data := FindFieldData(RecBuf, Field);
    if Data <> nil then
    begin
      Result := Boolean(Data[0]);
      Inc(Data);
      if Field.DataType in [ftString, ftFixedChar, ftWideString, ftGuid] then
        Result := Result and {$IFDEF OverRad2k7}(Data <> nil)
{$ELSE}(StrLen(Data) > 0){$ENDIF};
      if Result and (Buffer <> nil) then
        Move(Data^, Buffer^, CalcFieldLen(Field.DataType, Field.Size));
    end;
  end
  else
  begin
    if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      Inc(RecBuf, FRecordSize + Field.Offset);
      Result := Boolean(RecBuf[0]);
      if Result and (Buffer <> nil) then
        Move(RecBuf[1], Buffer^, Field.DataSize);
    end;
  end;
end;

procedure TMemDB.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf, Data: TBuffer;
begin
  if not (State in dsWriteModes) then
    Error(SNotEditing);
  GetActiveRecBuf(RecBuf);
  with Field do
  begin
    if FieldNo > 0 then
    begin
      if State in [dsCalcFields, dsFilter] then
        Error(SNotEditing);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        ErrorFmt(SFieldReadOnly, [DisplayName]);
      Validate(Buffer);
      if FieldKind <> fkInternalCalc then
      begin
        Data := FindFieldData(RecBuf, Field);
        if Data <> nil then
        begin
          begin
            Boolean(Data[0]) := LongBool(Buffer);
            Inc(Data);
            if longBool(Buffer) then
              Move(Buffer^, Data^, CalcFieldLen(DataType, Size))
            else
              FillChar(Data^, CalcFieldLen(DataType, Size), 0);
          end;
        end;
      end;
    end
    else {fkCalculated, fkLookup}
    begin
      Inc(RecBuf, FRecordSize + Offset);
      Boolean(RecBuf[0]) := LongBool(Buffer);
      if Boolean(RecBuf[0]) then
        Move(Buffer^, RecBuf[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, LongInt(Field));
  end;
end;

{ Filter }

procedure TMemDB.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      inherited SetFiltered(Value);
      First;
    end;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TMemDB.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active then
  begin
    CheckBrowseMode;
    inherited SetOnFilterRecord(Value);
    if Filtered then
      First;
  end
  else
    inherited SetOnFilterRecord(Value);
end;

function TMemDB.RecordFilter: Boolean;
var
  SaveState: TDataSetState;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
  begin
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      SaveState := SetTempState(dsFilter);
      try
        RecordToBuffer(Records[FRecordPos], TempBuffer);
        OnFilterRecord(Self, Result);
      except
 //       Application.HandleException(Self);
      end;
      RestoreState(SaveState);
    end
    else
      Result := False;
  end;
end;

{ Blobs }

function TMemDB.GetBlobData(Field: TField; Buffer: TBuffer): TBlobDataType;
begin
{$IFDEF FPC}
  Result := PMemBlobArray(Buffer + FBlobOfs)^[Field.Offset];
{$ELSE}
  Result := PMemBlobArray(Buffer + FBlobOfs)[Field.Offset];
{$ENDIF}
end;

procedure TMemDB.SetBlobData(Field: TField; Buffer: TBuffer; Value:
  TBlobDataType);
begin
  if (Buffer = ActiveBuffer) then
  begin
    if State = dsFilter then
      Error(SNotEditing);
{$IFDEF FPC}
    PMemBlobArray(Buffer + FBlobOfs)^[Field.Offset] := Value;
{$ELSE}
    PMemBlobArray(Buffer + FBlobOfs)[Field.Offset] := Value;
{$ENDIF}
  end;
end;

procedure TMemDB.CloseBlob(Field: TField);
begin
{$IFDEF FPC}
  if (FRecordPos >= 0) and (FRecordPos < FRecords.Count) and
    (State = dsEdit) then
    PMemBlobArray(ActiveBuffer + FBlobOfs)^[Field.Offset] :=
      PMemBlobArray(Records[FRecordPos].FBlobs)^[Field.Offset]
  else
    PMemBlobArray(ActiveBuffer + FBlobOfs)^[Field.Offset] := '';
{$ELSE}
  if (FRecordPos >= 0) and (FRecordPos < FRecords.Count) and (State = dsEdit)
    then
    PMemBlobArray(ActiveBuffer + FBlobOfs)[Field.Offset] :=
      PMemBlobArray(Records[FRecordPos].FBlobs)[Field.Offset]
  else
    PMemBlobArray(ActiveBuffer + FBlobOfs)[Field.Offset] := '';
{$ENDIF}
end;

function TMemDB.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TMemBlobStream.Create(Field as TBlobField, Mode);
end;

{ Bookmarks }

function TMemDB.BookmarkValid(aBookmark: TBookmark): Boolean;
begin
{$IFDEF OverRad2k7}
  Result := FActive and (PInteger(Bookmark)^ > Low(Integer)) and
    (PInteger(Bookmark)^ <= FLastID);
{$ELSE}
  Result := FActive and (TBookmarkData(aBookmark^) > Low(integer)) and
    (TBookmarkData(aBookmark^) <= FLastID);
{$ENDIF}
end;

function TMemDB.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): integer;
begin
  if (Bookmark1 = nil) and (Bookmark2 = nil) then
    Result := 0
  else if (Bookmark1 <> nil) and (Bookmark2 = nil) then
    Result := 1
  else if (Bookmark1 = nil) and (Bookmark2 <> nil) then
    Result := -1
  else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
    Result := 1
  else if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
    Result := -1
  else
    Result := 0;
end;

procedure TMemDB.GetBookmarkData(Buffer: TBuffer; Data: Pointer);
begin
  Move(PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, Data^,
    SizeOf(TBookmarkData));
end;

procedure TMemDB.SetBookmarkData(Buffer: TBuffer; Data: Pointer);
begin
  Move(Data^, PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData,
    SizeOf(TBookmarkData));
end;

function TMemDB.GetBookmarkFlag(Buffer: TBuffer): TBookmarkFlag;
begin
  Result := PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag;
end;

procedure TMemDB.SetBookmarkFlag(Buffer: TBuffer; Value: TBookmarkFlag);
begin
  PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag := Value;
end;

procedure TMemDB.InternalGotoBookmark(aBookmark: TBookmarkPointerType);
var
  Rec: TMemoryRecord;
  SavePos: integer;
  Accept: Boolean;
begin
  Rec := FindRecordID(TBookmarkData(aBookmark^));
  if Rec <> nil then
  begin
    Accept := True;
    SavePos := FRecordPos;
    try
      FRecordPos := Rec.Index;
      if Filtered then
        Accept := RecordFilter;
    finally
      if not Accept then
        FRecordPos := SavePos;
    end;
  end;
end;

{ Navigation }

procedure TMemDB.InternalSetToRecord(Buffer: TBuffer);
begin
  InternalGotoBookmark(@PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData);
end;

procedure TMemDB.InternalFirst;
begin
  FRecordPos := -1;
end;

procedure TMemDB.InternalLast;
begin
  FRecordPos := FRecords.Count;
end;

{ Data Manipulation }

procedure TMemDB.AssignMemoryRecord(Rec: TMemoryRecord; Buffer: TBuffer);
var
  I: integer;
begin
  Move(Buffer^, Rec.Data^, FRecordSize);
  for I := 0 to BlobFieldCount - 1 do
{$IFDEF FPC}
    PMemBlobArray(Rec.FBlobs)^[I] := PMemBlobArray(Buffer + FBlobOfs)^[I];
{$ELSE}
    PMemBlobArray(Rec.FBlobs)[I] := PMemBlobArray(Buffer + FBlobOfs)[I];
{$ENDIF}
end;

procedure TMemDB.SetMemoryRecordData(Buffer: TBuffer; Pos: integer);
var
  Rec: TMemoryRecord;
begin
  if State = dsFilter then
    Error(SNotEditing);
  Rec := Records[Pos];
  AssignMemoryRecord(Rec, Buffer);
end;

procedure TMemDB.SetAutoIncFields(Buffer: TBuffer);
var
  I, Count: Integer;
  Data: TBuffer;
begin
  Count := 0;
  for I := 0 to FieldCount - 1 do
    if (Fields[I].FieldKind in fkStoredFields) and (Fields[I].DataType =
      ftAutoInc) then
    begin
      Data := FindFieldData(Buffer, Fields[I]);
      if Data <> nil then
      begin
        Boolean(Data[0]) := True;
        Inc(Data);
        Move(FAutoInc, Data^, SizeOf(LongInt));
        Inc(Count);
      end;
    end;
  if Count > 0 then
    Inc(FAutoInc);
end;

procedure TMemDB.InternalAddRecord(Buffer: Pointer; aAppend: Boolean);
var
  RecPos: integer;
  Rec: TMemoryRecord;
begin
  if aAppend then
  begin
    Rec := AddRecord;
    FRecordPos := FRecords.Count - 1;
  end
  else
  begin
    if FRecordPos = -1 then
      RecPos := 0
    else
      RecPos := FRecordPos;
    Rec := InsertRecord(RecPos);
    FRecordPos := RecPos;
  end;
  SetAutoIncFields(Buffer);
  SetMemoryRecordData(Buffer, Rec.Index);

  GetMem(FOldBuf, FRecBufSize);
  Move(ActiveBuffer^, FOldBuf^, FRecBufSize);
end;

procedure TMemDB.InternalDelete;
var
  Accept: Boolean;
begin
  Records[FRecordPos].Free;
  if FRecordPos >= FRecords.Count then
    Dec(FRecordPos);
  Accept := True;
  repeat
    if Filtered then
      Accept := RecordFilter;
    if not Accept then
      Dec(FRecordPos);
  until Accept or (FRecordPos < 0);
  if FRecords.Count = 0 then
    FLastID := Low(integer);
end;

procedure TMemDB.InternalPost;
var
  RecPos: integer;
begin
  if State = dsEdit then
    SetMemoryRecordData(ActiveBuffer, FRecordPos)
  else
  begin
    if State in [dsInsert] then
      SetAutoIncFields(ActiveBuffer);
    if FRecordPos >= FRecords.Count then
    begin
      SetMemoryRecordData(ActiveBuffer, AddRecord.Index);
      FRecordPos := FRecords.Count - 1;
    end
    else
    begin
      if FRecordPos = -1 then
        RecPos := 0
      else
        RecPos := FRecordPos;
      SetMemoryRecordData(ActiveBuffer, InsertRecord(RecPos).Index);
      FRecordPos := RecPos;
    end;
  end;
  FreeMem(FOldBuf);
  FOldBuf := nil;
end;

procedure TMemDB.InternalCancel;
begin
  FreeMem(FOldBuf);
  FOldBuf := nil;
  inherited InternalCancel;
end;

procedure TMemDB.OpenCursor(InfoQuery: Boolean);
begin
  if not InfoQuery then
  begin
    if FieldCount > 0 then
      FieldDefs.Clear;
    InitFieldDefsFromFields;
  end;
  FActive := True;
  inherited OpenCursor(InfoQuery);
end;

procedure TMemDB.InternalOpen;
begin
  BookmarkSize := SizeOf(TBookmarkData);
  if DefaultFields then
    CreateFields;
  BindFields(True);
  InitBufferPointers(True);
  InternalFirst;
end;

procedure TMemDB.InternalClose;
begin
  ClearRecords;
  FAutoInc := 1;
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  FreeIndexList;
  FActive := False;
end;

procedure TMemDB.InternalHandleException;
begin
  //Application.HandleException(Self);
end;

procedure TMemDB.InternalInitFieldDefs;
begin
end;

function TMemDB.IsCursorOpen: Boolean;
begin
  Result := FActive;
end;

{ Informational }

function TMemDB.GetRecordCount: integer;
begin
  Result := FRecords.Count;
end;

function TMemDB.GetRecNo: integer;
begin
  CheckActive;
  UpdateCursorPos;
  if (FRecordPos = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FRecordPos + 1;
end;

procedure TMemDB.SetRecNo(Value: integer);
begin
  if (Value > 0) and (Value <= FRecords.Count) then
  begin
    FRecordPos := Value - 1;
    Resync([]);
  end;
end;

function TMemDB.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;

function TMemDB.Locate(const KeyFields: string; const KeyValues: variant;
  Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then
  begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

{ Table Manipulation }

procedure TMemDB.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    ClearRecords;
    ClearBuffers;
    DataEvent(deDataSetChange, 0);
  end;
end;

procedure TMemDB.CopyStructure(Source: TDataSet);

  procedure CheckDataTypes(FieldDefs: TFieldDefs);
  var
    I: integer;
  begin
    for I := FieldDefs.Count - 1 downto 0 do
    begin
      if not (FieldDefs.Items[I].DataType in ftSupported) then
        FieldDefs.Items[I].Free
      else
{$IFDEF ENABLE_Child_Defs}
        CheckDataTypes(FieldDefs[I].ChildDefs);
{$ENDIF}
    end;
  end;

var
  I: integer;
begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do
    Fields[I].Free;
  if (Source = nil) then
    Exit;
  Source.FieldDefs.Update;
  FieldDefs := Source.FieldDefs;
  CheckDataTypes(FieldDefs);
  CreateFields;
end;

function TMemDB.LoadFromDataSet(Source: TDataSet; aRecordCount: integer;
  Mode: TLoadMode): integer;
var
  SourceActive: Boolean;
  MovedCount: integer;
begin
  Result := 0;
  if Source = Self then
    Exit;
  SourceActive := Source.Active;
  Source.DisableControls;
  try
    DisableControls;
    try
      Filtered := False;
      with Source do
      begin
        Open;
        CheckBrowseMode;
        UpdateCursorPos;
      end;
      if Mode = lmCopy then
      begin
        Close;
        CopyStructure(Source);
      end;
      FreeIndexList;
      if not Active then
        Open;
      CheckBrowseMode;
      if aRecordCount > 0 then
        MovedCount := aRecordCount
      else
      begin
        Source.First;
        MovedCount := MaxInt;
      end;
      try
        while not Source.EOF do
        begin
          Append;
          AssignRecord(Source, Self, True);
          Post;
          Inc(Result);
          if Result >= MovedCount then
            Break;
          Source.Next;
        end;
      finally
        First;
      end;
    finally
      EnableControls;
    end;
  finally
    if not SourceActive then
      Source.Close;
    Source.EnableControls;
  end;
end;

function TMemDB.SaveToDataSet(Dest: TDataSet; aRecordCount: integer): integer;
var
  MovedCount: integer;
begin
  Result := 0;
  if Dest = Self then
    Exit;
  CheckBrowseMode;
  UpdateCursorPos;
  Dest.DisableControls;
  try
    DisableControls;
    try
      if not Dest.Active then
        Dest.Open
      else
        Dest.CheckBrowseMode;
      if aRecordCount > 0 then
        MovedCount := aRecordCount
      else
      begin
        First;
        MovedCount := MaxInt;
      end;
      try
        while not EOF do
        begin
          Dest.Append;
          AssignRecord(Self, Dest, True);
          Dest.Post;
          Inc(Result);
          if Result >= MovedCount then
            Break;
          Next;
        end;
      finally
        Dest.First;
      end;
    finally
      EnableControls;
    end;
  finally
    Dest.EnableControls;
  end;
end;

{ Index Related }

procedure TMemDB.SortOnFields(const FieldNames: AnsiString;
  CaseInsensitive: Boolean = True; Descending: Boolean = False);
begin
  CreateIndexList(FieldNames);
  FCaseInsensitiveSort := CaseInsensitive;
  FDescendingSort := Descending;
  try
    Sort;
  except
    FreeIndexList;
    raise;
  end;
end;

procedure TMemDB.Sort;
var
  Pos: TBookmarkType;
begin
  if Active and (FRecords <> nil) and (FRecords.Count > 0) then
  begin
    Pos := Bookmark;
    try
{$IFDEF FPC}
      QuickSort(0, FRecords.Count - 1, @CompareRecords);
{$ELSE}
      QuickSort(0, FRecords.Count - 1, CompareRecords);
{$ENDIF}
      SetBufListSize(0);
      InitBufferPointers(False);
      try
        SetBufListSize(BufferCount + 1);
      except
        SetState(dsInactive);
        CloseCursor;
        raise;
      end;
    finally
      Bookmark := Pos;
    end;
    Resync([]);
  end;
end;

procedure TMemDB.QuickSort(L, R: integer; Compare: TCompareRecords);
var
  I, J: integer;
  P: TMemoryRecord;
begin
  repeat
    I := L;
    J := R;
    P := Records[(L + R) shr 1];
    repeat
      while Compare(Records[I], P) < 0 do
        Inc(I);
      while Compare(Records[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        FRecords.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

function TMemDB.CompareRecords(Item1, Item2: TMemoryRecord): integer;
var
  Data1, Data2: PAnsiChar;
  F: TField;
  I: integer;
begin
  Result := 0;
  if FIndexList <> nil then
  begin
    for I := 0 to FIndexList.Count - 1 do
    begin
      F := TField(FIndexList[I]);
      Data1 := FindFieldData(Item1.Data, F);
      if Data1 <> nil then
      begin
        Data2 := FindFieldData(Item2.Data, F);
        if Data2 <> nil then
        begin
          if Boolean(Data1[0]) and Boolean(Data2[0]) then
          begin
            Inc(Data1);
            Inc(Data2);
            Result := CompareFields(Data1, Data2, F.DataType,
              FCaseInsensitiveSort);
          end
          else if Boolean(Data1[0]) then
            Result := 1
          else if Boolean(Data2[0]) then
            Result := -1;
          if FDescendingSort then
            Result := -Result;
        end;
      end;
      if Result <> 0 then
        Exit;
    end;
  end;
  if (Result = 0) then
  begin
    if Item1.ID > Item2.ID then
      Result := 1
    else if Item1.ID < Item2.ID then
      Result := -1;
    if FDescendingSort then
      Result := -Result;
  end;
end;

function TMemDB.GetIsIndexField(Field: TField): Boolean;
begin
  if FIndexList <> nil then
    Result := FIndexList.IndexOf(Field) >= 0
  else
    Result := False;
end;

procedure TMemDB.CreateIndexList(const FieldNames: AnsiString);
var
  Pos: integer;
  F: TField;
begin
  if FIndexList = nil then
    FIndexList := TList.Create
  else
    FIndexList.Clear;
  Pos := 1;
  while Pos <= Length(FieldNames) do
  begin
    F := FieldByName(ExtractFieldName(FieldNames, Pos));
    if (F.FieldKind = fkData) and (F.DataType in ftSupported - ftBlobTypes) then
      FIndexList.Add(F)
    else
      ErrorFmt(SFieldTypeMismatch, [F.DisplayName]);
  end;
end;

procedure TMemDB.FreeIndexList;
begin
  FIndexList.Free;
  FIndexList := nil;
end;

{ TMemBlobStream }

constructor TMemBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TMemDB;
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
  if not FField.Modified and (Mode <> bmRead) then
  begin
    if FField.ReadOnly then
      ErrorFmt(SFieldReadOnly, [FField.DisplayName]);
    if not (FDataSet.State in [dsEdit, dsInsert]) then
      Error(SNotEditing);
    FCached := True;
  end
  else
    FCached := (FBuffer = FDataSet.ActiveBuffer);
  FOpened := True;
  if Mode = bmWrite then
    Truncate;
end;

destructor TMemBlobStream.Destroy;
begin
  if FOpened and FModified then
    FField.Modified := True;
  if FModified then
  try
    FDataSet.DataEvent(deFieldChange, LongInt(FField));
  except
    //Application.HandleException(Self);
  end;
end;

function TMemBlobStream.GetBlobFromRecord(Field: TField): TMemBlobData;
var
  Rec: TMemoryRecord;
  Pos: integer;
begin
  Result := '';
  Pos := FDataSet.FRecordPos;
  if (Pos < 0) and (FDataSet.RecordCount > 0) then
    Pos := 0
  else if Pos >= FDataSet.RecordCount then
    Pos := FDataSet.RecordCount - 1;
  if (Pos >= 0) and (Pos < FDataSet.RecordCount) then
  begin
    Rec := FDataSet.Records[Pos];
    if Rec <> nil then
{$IFDEF FPC}
      Result := PMemBlobArray(Rec.FBlobs)^[FField.Offset];
{$ELSE}
      Result := PMemBlobArray(Rec.FBlobs)[FField.Offset];
{$ENDIF}
  end;
end;

function TMemBlobStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := 0;
  if FOpened then
  begin
    if Count > Size - FPosition then
      Result := Size - FPosition
    else
      Result := Count;
    if Result > 0 then
    begin
      if FCached then
      begin
        Move(PAnsiChar(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer,
          Result);
        Inc(FPosition, Result);
      end
      else
      begin
        Move(PAnsiChar(GetBlobFromRecord(FField))[FPosition], Buffer,
          Result);
        Inc(FPosition, Result);
      end;
    end;
  end;
end;

function TMemBlobStream.Write(const Buffer; Count: LongInt): LongInt;
var
  Temp: TBlobDataType;
begin
  Result := 0;
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    Temp := FDataSet.GetBlobData(FField, FBuffer);
    if Length(Temp) < FPosition + Count then
      SetLength(Temp, FPosition + Count);
    Move(Buffer, PAnsiChar(Temp)[FPosition], Count);
    FDataSet.SetBlobData(FField, FBuffer, Temp);
    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
  end;
end;

function TMemBlobStream.Seek(Offset: LongInt; Origin: word): LongInt;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TMemBlobStream.Truncate;
begin
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    FDataSet.SetBlobData(FField, FBuffer, '');
    FModified := True;
  end;
end;

function TMemBlobStream.GetBlobSize: LongInt;
begin
  Result := 0;
  if FOpened then
    if FCached then
      Result := Length(FDataSet.GetBlobData(FField, FBuffer))
    else
      Result := Length(GetBlobFromRecord(FField));
end;

end.

