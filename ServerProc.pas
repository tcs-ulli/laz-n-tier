{ ==============================================================================|
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
  |============================================================================== }

unit ServerProc;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses DataProcUtils, SynaSSockets, SysUtils, Classes, DB, SyncObjs,
{$IFDEF MSWINDOWS} Windows, {$ELSE} DynLibs, {$ENDIF} MD5, StrUtils;

//{$I Instructions.inc}

type
  TServerConnBuffer = class;
  SQLStyle = (SelectSQL, ExecuteSQL);

  TCustomServerSockQuery = class(TComponent)
  public
    Params: TParams;
    Script: TStringList;
    SQL: TStringList;
    FNetData: TDataset;
    NetComponent: TComponent;
    RowsAffected: integer;
    ORGIDStyle: integer;
    ORGID1, ORGID2: TNetProcString;
    DataOwner: TServerConnBuffer;
    StoredProcName: string;
    procedure Reconnect; virtual;
    function ExecSQL: integer; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(ParamStr: TNetProcString): TParam; virtual;
    function Open: boolean; virtual;
    function SuperOpen: boolean; virtual;
    procedure ClearParam; virtual;
    procedure CreateDBParam(DataType: TFieldType; ParamName: string;
      ParamType: TParamType); virtual;
    procedure Close; virtual;
    function SuperExecSQL: integer; virtual;
    function ExecScript: integer; virtual;
    function ExecStoredProc: string; virtual;
    procedure StartTransaction; virtual;
    procedure Commit; virtual;
    procedure RollBack; virtual;
    procedure InitializeStoredProc; virtual;
    property NetData: TDataset read FNetData write FNetData;
  end;

  TServerSockQuery = class(TCustomServerSockQuery)
  public
    procedure Reconnect; override;
    function ExecSQL: integer; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(ParamStr: TNetProcString): TParam; override;
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
  end;

  TOnlineDataSource = class(TDataSource)
  private
    FOnlineDataSet: TServerSockQuery;
    procedure SetOnlineDataSet(Value: TServerSockQuery);
  public
  published
    property OnlineDataSet: TServerSockQuery read FOnlineDataSet write SetOnlineDataSet;
  end;

  TOnCustInternalCall = function(CustInstruc, CustSubInstruc: byte;
    CliParam: PAnsiChar; DataQuery: TServerSockQuery; DataSQLProc: TServerSockQuery;
    DataStoredProc: TServerSockQuery;
    User, SubFunctions: TNetProcString): TNetProcString of object;

  TOnUserLogonCall = function(UserName, Password: TNetProcString): TLogonStyle of object;

  TServerConnBuffer = class(TOnlineDataBuffer)
  private
    FOnCustInternalCall: TOnCustInternalCall;
    FOnUserLogonCall: TOnUserLogonCall;
  protected
    procedure ProcessData; override;
  public
    MustAuthenticate: boolean;
    NetQuery: TServerSockQuery;
    NetSQLRun: TServerSockQuery;
    NetStoredProc: TServerSockQuery;
    NetSQLProc: TServerSockQuery;
    SessionName, DatabaseName: TNetProcString;
    TempClientFunctions, TempClientReadTables, TempClientPermTables,
    TempORGID1, TempORGID2, TempSubFuncs: TNetProcString;
    TempOrgStyle: integer;
    LogonStyle: boolean;
    FUSR: TNetProcString;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Log(const Msg: TNetProcString);
    procedure ExecFuncSQL;
    procedure ExecFuncSQLCacheUpdate;
    procedure ExecFuncLoadFile;
    procedure ExecFuncError(const Msg: TNetProcString);
    procedure ExecFuncLogin;
    function DoSQLScript(FDataQuery: TServerSockQuery;
      FUser, FSubFuncs: TNetProcString): boolean;
    function DoStoredProc(FDataQuery: TServerSockQuery;
      FUser, FSubFuncs: TNetProcString): boolean;
    function DoSpecialSQL(FDataQuery: TServerSockQuery;
      FUser, FSubFuncs: TNetProcString): boolean;
    function DoInternalSpecialSQL(CustSubInstrucs: byte; FUser: string;
      ASQLStyle: SQLStyle; SQLText: TNetProcString): boolean;
    function DoDynamicCustProc(FDataQuery: TServerSockQuery;
      NetSQLProc: TServerSockQuery; FZStProc: TServerSockQuery;
      FUser, FSubFuncs: TNetProcString): boolean;
    function DoInternalCustProc(FDataQuery: TServerSockQuery;
      NetSQLProc: TServerSockQuery; FZStProc: TServerSockQuery;
      FUser, FSubFuncs: TNetProcString): boolean;
    function ServerCheckLogon(UserCode, UcPSW: TNetProcString): TLogonStyle;
    procedure ExecFuncChangPSW;
    function ServerSQLProc(CustInstrucV: byte; SQLVS: SQLStyle;
      FUser, SQLValue: TNetProcString): byte;
    function ServerStoredProc(CustInstrucV, SQLVS: byte;
      FUser, SQLValue: TNetProcString; ParamNum, InNum, RetNum: integer): byte;
    function ServerScriptProc(CustInstrucV, SQLVS: byte;
      FUser, SQLValue: TNetProcString): byte;
    function ServerCustBefore(CustInstrucV, SQLVS: byte;
      FUser, ScriptValue, SQLValueE, SQLValueOBefore: TNetProcString)
      : TNetProcString;
    function ServerCustProc(CustInstrucV, SQLVS: byte;
      FUser, ScriptValue, SQLValueE, SQLValueOBefore, SQLValueOAfter:
      TNetProcString): TNetProcString;
    function ServerCustAfter(CustInstrucV, SQLVS: byte;
      FUser, ScriptValue, SQLValueE, SQLValueOBefore, SQLValueOAfter:
      TNetProcString): TNetProcString;
    function GetCustInstruc: byte;
    function GetCustParam: TNetProcString;
  published
    property OnCustInternalCall: TOnCustInternalCall
      read FOnCustInternalCall write FOnCustInternalCall;
    property OnUserLogonCall: TOnUserLogonCall
      read FOnUserLogonCall write FOnUserLogonCall;
  end;

  T_GetXValue = function(CustInstruc: byte): byte; cdecl;
  T_Do_S_Proc = function(CustInstruc: byte;
    FUser, FSubFuncs, CliParam: PAnsiChar): PAnsiChar; cdecl; // stdcall;
  T_Do_S_AfterProc = function(CustInstruc: byte;
    FUser, FSubFuncs, CliParam, StrBefore: PAnsiChar): PAnsiChar; cdecl;
  T_Do_S_ReturnProc = function(CustInstruc: byte;
    FUser, FSubFuncs, CliParam, StrBefore, StrProc, StrAfter: PAnsiChar): PAnsiChar;
    cdecl;

var
  ServerLog: TStrings = nil;
  DLibHandle: THandle;
  GetSQLStyle: T_GetXValue;
  GetStoredParamNum: T_GetXValue;
  GetStoredInNum: T_GetXValue;
  GetStoredOutNum: T_GetXValue;
  DoSSToredProc: T_Do_S_Proc;
  DoSScriptProc: T_Do_S_Proc;
  DoSCustProcScript: T_Do_S_AfterProc;
  DoSCustProcSQLExec: T_Do_S_AfterProc;
  DoSCustProcSQLBeforeOpen: T_Do_S_Proc;
  DoSCustProcSQLAfterOpen: T_Do_S_AfterProc;
  DoSSpecialQuery: T_Do_S_Proc;
  DoSReturnValue: T_Do_S_ReturnProc;

procedure Writelog(Value: TNetProcString);

implementation

{$I SQLProcUtils.inc}
{$I MemDBRADv.inc}

procedure Writelog(Value: TNetProcString);
var
  f: textFile;
  s: TNetProcString;
begin
  {$IFDEF EnableLOG}
  s := Value;
  s := ExtractFilePath(ParamStr(0)) + 'DBNetProc.log';
  assignfile(f, s);
  if fileexists(s) then
    append(f)
  else
    rewrite(f);
  try
    writeln(f, Value);
  finally
    Closefile(f);
  end;
  {$ENDIF}
end;

procedure TOnlineDataSource.SetOnlineDataSet(Value: TServerSockQuery);
begin
  FOnlineDataSet := Value;
  DataSet := Value.NetData;
end;

constructor TCustomServerSockQuery.Create(AOwner: TComponent);
begin
  inherited;
  SQL := TStringList.Create;
  Params := TParams.Create;
  Script := TStringList.Create;
  DataOwner := TServerConnBuffer(AOwner);
end;

destructor TCustomServerSockQuery.Destroy;
begin
  DataOwner := nil;
  SQL.Free;
  Params.Free;
  Script.Free;
  inherited;
end;

procedure TCustomServerSockQuery.CreateDBParam(DataType: TFieldType;
  ParamName: string; ParamType: TParamType);
begin

end;

procedure TCustomServerSockQuery.ClearParam;
begin

end;

function TCustomServerSockQuery.Open: boolean;
begin

end;

function TCustomServerSockQuery.SuperOpen: boolean;
begin

end;

procedure TCustomServerSockQuery.Close;
begin

end;

function TCustomServerSockQuery.ParamByName(ParamStr: TNetProcString): TParam;
begin

end;

procedure TCustomServerSockQuery.Reconnect;
begin

end;

function TCustomServerSockQuery.ExecScript: integer;
begin

end;

function TCustomServerSockQuery.ExecStoredProc: string;
begin

end;

procedure TCustomServerSockQuery.StartTransaction;
begin

end;

procedure TCustomServerSockQuery.Commit;
begin

end;

procedure TCustomServerSockQuery.RollBack;
begin

end;

function TCustomServerSockQuery.ExecSQL: integer;
begin

end;

function TCustomServerSockQuery.SuperExecSQL: integer;
begin

end;

procedure TCustomServerSockQuery.InitializeStoredProc;
begin

end;

{$WARNINGS OFF}

constructor TServerSockQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

{$WARNINGS ON}

destructor TServerSockQuery.Destroy;
begin
  inherited Destroy;
end;

function TServerSockQuery.Open: boolean;
var
  TName: TNetProcString;
begin
  Result := False;

  TName := GetTableNameFromSQL(Self.SQL.Text);

  if TName = '' then
    Exit;

  if (Uppercase(TName) <> 'OPERASINFO') and (Uppercase(TName) <> 'ORGIDDB') then
  else
    Exit;

  if DataOwner.MustAuthenticate then
  begin
    if (Pos(TName, DataOwner.TempClientReadTables) = 0) and
      (DataOwner.TempClientReadTables <> 'ALL') then
      Exit;

    ORGIDStyle := GetTableORGIDStyle2(Self, TName);

    if DataOwner.TempORGID1 = 'ALL' then
    begin
      if (ORGIDStyle = 1) or (ORGIDStyle = 3) then
        ORGIDStyle := ORGIDStyle - 1;
    end;

    if DataOwner.TempORGID2 = 'ALL' then
    begin
      if (ORGIDStyle = 2) or (ORGIDStyle = 3) then
        ORGIDStyle := ORGIDStyle - 2;
    end;

    DataOwner.TempOrgStyle := ORGIDStyle;

    if ORGIDStyle > 0 then
      if Pos('WHERE', Uppercase(Self.SQL[0])) > 0 then
      begin
        if ORGIDStyle = 3 then
          Self.SQL[0] := Self.SQL[0] + ' and ORGID1 = ' +
            QuotedStr(DataOwner.TempORGID1) + ' and ORGID2 = ' +
            QuotedStr(DataOwner.TempORGID2);
        if ORGIDStyle = 2 then
          Self.SQL[0] := Self.SQL[0] + ' and ORGID2 = ' +
            QuotedStr(DataOwner.TempORGID2);
        if ORGIDStyle = 1 then
          Self.SQL[0] := Self.SQL[0] + ' and ORGID1 = ' +
            QuotedStr(DataOwner.TempORGID1);
      end
      else
      begin
        if ORGIDStyle = 3 then
          Self.SQL[0] := Self.SQL[0] + ' where ORGID1 = ' +
            QuotedStr(DataOwner.TempORGID1) + ' and ORGID2 = ' +
            QuotedStr(DataOwner.TempORGID2);
        if ORGIDStyle = 2 then
          Self.SQL[0] := Self.SQL[0] + ' where ORGID2 = ' +
            QuotedStr(DataOwner.TempORGID2);
        if ORGIDStyle = 1 then
          Self.SQL[0] := Self.SQL[0] + ' where ORGID1 = ' +
            QuotedStr(DataOwner.TempORGID1);
      end;
  end;
  Result := True;
  Writelog(SQL.Text);
end;

function TServerSockQuery.SuperOpen: boolean;
begin
  Result := False;
end;

procedure TServerSockQuery.Close;
begin

end;

function TServerSockQuery.ParamByName(ParamStr: TNetProcString): TParam;
begin
  Result := nil;
end;

procedure TServerSockQuery.Reconnect;
begin

end;

function TServerSockQuery.ExecScript: integer;
begin

end;

function TServerSockQuery.ExecStoredProc: string;
begin

end;

procedure TServerSockQuery.StartTransaction;
begin

end;

procedure TServerSockQuery.Commit;
begin

end;

procedure TServerSockQuery.RollBack;
begin

end;

function TServerSockQuery.ExecSQL: integer;
var
  TName: TNetProcString;
begin
  Result := 0;
  TName := GetTableNameFromSQL(Self.SQL.Text);
  if TName = '' then
    Exit;
  if Uppercase(TName) = 'ORGIDDB' then
    Exit;
  if Uppercase(TName) = 'OPERASINFO' then
    Exit;

  Self.SQL.Text := Trim(Self.SQL.Text);

  if DataOwner.MustAuthenticate then
  begin
    if (Pos(TName, DataOwner.TempClientPermTables) = 0) and
      (DataOwner.TempClientPermTables <> 'ALL') then
      Exit;
    if Pos('INSERT', Uppercase(Self.SQL.Text)) = 1 then
      Self.SQL.Text := UpdateInsFunc(Self.SQL.Text, DataOwner.TempORGID1,
        DataOwner.TempORGID2, DataOwner.TempOrgStyle);
    if Pos('UPDATE', Uppercase(Self.SQL.Text)) = 1 then
      Self.SQL.Text := UpdateUPDFunc(Self.SQL.Text, DataOwner.TempORGID1,
        DataOwner.TempORGID2, DataOwner.TempOrgStyle);
  end;
  Writelog(SQL.Text);
end;

function TServerSockQuery.SuperExecSQL: integer;
var
  TName: TNetProcString;
begin
  Result := 0;
  TName := GetTableNameFromSQL(Self.SQL.Text);
  if TName = '' then
    Exit;
  Self.SQL.Text := Trim(Self.SQL.Text);
  Result := RowsAffected;
  Writelog(SQL.Text);
end;

procedure TServerSockQuery.InitializeStoredProc;
begin

end;

constructor TServerConnBuffer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MustAuthenticate := True;
  LogonStyle := False;
  DateSeparator := '-';
  ShortDateFormat := 'yyMMdd';
  LongDateFormat := 'yyyy-MM-dd';
  DecimalSeparator := '.';
  ThousandSeparator := ',';
  ShortTimeFormat := 'H:mm';
  LongTimeFormat := 'H:mm:ss';
end;

destructor TServerConnBuffer.Destroy;
begin
  inherited Destroy;
end;

procedure TServerConnBuffer.Log(const Msg: TNetProcString);
begin
{$IFNDEF FPC}
  if ServerLog <> nil then
    ServerLog.add(Msg);
{$ENDIF}
  Writelog(Msg);
end;

procedure TServerConnBuffer.ProcessData;
begin
  Log('Instruction: ' + InstructionNames[Instruction].Name);
  case Instruction of
    IstLogin:
      ExecFuncLogin;
    IstTime:
    begin
      WriteStr(FormatDateTime('yyyyMMdd hh:mm:ss', Now));
      ProcessSendData;
    end;
    IstSQL:
      ExecFuncSQL;
    IstSQLCacheExec:
      ExecFuncSQLCacheUpdate;
    IstLoadFile:
      ExecFuncLoadFile;
    IstDynamicCustProc:
      DoDynamicCustProc(NetQuery, NetSQLProc, NetStoredProc, FUSR,
        TempSubFuncs);
    IstInternalCustProc:
      DoInternalCustProc(NetQuery, NetSQLProc, NetStoredProc, FUSR,
        TempSubFuncs);
    IstSpecialSQL:
      DoSpecialSQL(NetQuery, FUSR, TempSubFuncs);
    IstSQLScript:
      DoSQLScript(NetSQLProc, FUSR, TempSubFuncs);
    IstStoredProc:
      DoStoredProc(NetStoredProc, FUSR, TempSubFuncs);
    IstChangePSW:
      ExecFuncChangPSW;
    else
      ExecFuncError('Unknown Instruction');
  end;
end;

function TServerConnBuffer.ServerCheckLogon(UserCode, UcPSW: TNetProcString): TLogonStyle;
var
  FPSW: TNetProcString;
  FTempPermMode: integer;
begin
  FUSR := '';
  FPSW := '';
  TempClientFunctions := '';
  TempClientPermTables := '';
  TempClientReadTables := '';
  TempORGID1 := '';
  TempORGID2 := '';
  TempSubFuncs := '';
  LogonStyle := False;
  FTempPermMode := 0;
  Result := UserNameError;

  if (UserCode = SuperUsr) and (UcPSW = SuperPsw) then
  begin
    Result := LogedOnServer;
    TempClientFunctions := 'ALL';
    TempClientPermTables := 'ALL';
    TempClientReadTables := 'ALL';
    TempORGID1 := 'ALL';
    TempORGID2 := 'ALL';
    LogonStyle := True;
    TempSubFuncs := 'ALL';
    FTempPermMode := 1;
    Exit;
  end;

  try
    NetQuery.Close;
    NetQuery.SQL.Clear;
    NetQuery.SQL.Text := 'Select * from OPERASINFO where USR=' +
      QuotedStr(UserCode);

    if NetQuery.SuperOpen then
      Log('Authenticate opened')
    else
      Log('Authenticate not opened');

    if NetQuery.NetData.RecordCount = 0 then
    begin
      NetQuery.Close;
      Result := UserNameError;
      Exit;
    end
    else
    begin
{$IFDEF OverRad2k7}
      FUSR := Trim(NetQuery.NetData.FieldByName('USR').AsAnsiString);
      FPSW := StrMD5(FUSR + Trim(NetQuery.NetData.FieldByName('PSW')
        .AsAnsiString) + FormatDateTime('yyyymmdd', Now));
      TempClientFunctions := Trim(NetQuery.NetData.FieldByName('FUNCS')
        .AsAnsiString);
      TempClientPermTables := Trim(NetQuery.NetData.FieldByName('PermTB')
        .AsAnsiString);
      TempClientReadTables := Trim(NetQuery.NetData.FieldByName('ReadTB')
        .AsAnsiString);
      TempORGID1 := Trim(NetQuery.NetData.FieldByName('ORGID1').AsAnsiString);
      TempORGID2 := Trim(NetQuery.NetData.FieldByName('ORGID2').AsAnsiString);
      TempSubFuncs := Trim(NetQuery.NetData.FieldByName('SubFuncs')
        .AsAnsiString);
{$ELSE}
      FUSR := Trim(NetQuery.NetData.FieldByName('USR').AsString);
      FPSW := StrMD5(FUSR + Trim(NetQuery.NetData.FieldByName('PSW').AsString) +
        FormatDateTime('yyyymmdd', Now));
      TempClientFunctions := Trim(NetQuery.NetData.FieldByName('FUNCS')
        .AsString);
      TempClientPermTables := Trim(NetQuery.NetData.FieldByName('PermTB')
        .AsString);
      TempClientReadTables := Trim(NetQuery.NetData.FieldByName('ReadTB')
        .AsString);
      TempORGID1 := Trim(NetQuery.NetData.FieldByName('ORGID1').AsString);
      TempORGID2 := Trim(NetQuery.NetData.FieldByName('ORGID2').AsString);
      TempSubFuncs := Trim(NetQuery.NetData.FieldByName('SubFuncs').AsString);
{$ENDIF}
      FTempPermMode := NetQuery.NetData.FieldByName('PermMode').AsInteger;

      if FPSW = UcPSW then
      begin
        Result := LogedOnServer;
        LogonStyle := True;
      end
      else
      begin
        Result := UserNameError;
        LogonStyle := False;
      end;

      if FTempPermMode <> 1 then
      begin
        Result := PermDenied;
        LogonStyle := False;
      end;

      if FTempPermMode = 2 then
      begin
        Result := PermModeII;
        LogonStyle := False;
      end;

      if FTempPermMode = 3 then
      begin
        Result := PermModeIII;
        LogonStyle := False;
      end;

    end;
    NetQuery.Close;
  except
  end;
end;

procedure TServerConnBuffer.ExecFuncSQL;
var
  SQLIsttion: TSQLInstruction;
  S1, TempStr, TempSQL, TempField: TNetProcString;
  I, RecCont: integer;
begin
  try
    NetQuery.Close;
    NetSQLRun.Close;
    NetQuery.SQL.Clear;
    NetSQLRun.SQL.Clear;
    SQLIsttion := TSQLInstruction(ReadByte);
    TempSQL := ReadStr;
{$IFDEF FPC}
    NetQuery.SQL.Text := TempSQL;
{$ELSE}
    NetQuery.SQL.Text := UTF8Decode(TempSQL);
{$ENDIF}
    NetSQLRun.SQL.Text := NetQuery.SQL.Text;
    while ReadByte = 0 do
    begin
      S1 := ReadStr;
      with NetQuery.ParamByName(RetrieveStr(S1, ';')) do
      begin
        DataType := TFieldType(StrToInt(S1));
        Value := ReadStr;
      end;
    end;

    if SQLIsttion = IstSQLExec then
    begin
      NetSQLRun.ExecSQL;
      WriteByte(byte(SQLIsttion));
      WriteInt(NetQuery.RowsAffected);
      NetSQLRun.SQL.Clear;
      ProcessSendData;
      Exit;
    end;

    if (SQLIsttion = IstSQLWithFields) or (SQLIsttion = IstSQLOpen) then
    begin
      WriteByte(byte(SQLIsttion));

      RecCont := 0;

      if NetQuery.Open then
      begin
        if SQLIsttion = IstSQLWithFields then
        begin
          WriteInt(NetQuery.NetData.FieldCount);

          for I := 0 to NetQuery.NetData.FieldCount - 1 do
          begin
{$IFNDEF FPC}
            TempField := UTF8Encode(NetQuery.NetData.Fields[I].FieldName) + ';';
{$ELSE}
            TempField := NetQuery.NetData.Fields[I].FieldName + ';';
{$ENDIF}
            WriteStr(TempField + IntToStr(
              integer(NetQuery.NetData.Fields[I].DataType)) + ';' +
              IntToStr(NetQuery.NetData.Fields[I].Size) + ';' +
              IntToStr(integer(NetQuery.NetData.Fields[I].Required)));
          end;
        end;

        RecCont := 0;
        if (SQLIsttion = IstSQLOpen) or (SQLIsttion = IstSQLWithFields) then
        begin
          while not NetQuery.NetData.EOF do
          begin
            WriteByte(0);
            for I := 0 to NetQuery.NetData.FieldCount - 1 do
            begin
              if NetQuery.NetData.Fields[I].DataType in [ftDate, ftTime, ftDateTime] then
                TempStr := FloatToDotStr(NetQuery.NetData.Fields[I].AsDateTime)
              else
              if NetQuery.NetData.Fields[I].DataType in [ftFloat, ftCurrency] then
                TempStr := FloatToDotStr(NetQuery.NetData.Fields[I].AsFloat)
              else
              begin
{$IFDEF OverRad2k7}
                TempStr := NetQuery.NetData.Fields[I].AsAnsiString;
{$ELSE}
                TempStr := NetQuery.NetData.Fields[I].AsString;
{$ENDIF}
{$IFNDEF FPC}
                if NetQuery.NetData.Fields[I].DataType in
                  [ftString, ftFixedChar] then
                  TempStr := UTF8Encode(TempStr);
{$ENDIF}
              end;
              WriteStr(TempStr);
            end;
            Inc(RecCont);
            NetQuery.NetData.Next;
          end;
        end;
      end;

      WriteByte(1);
      WriteInt(RecCont);
      WriteByte(1);
      NetQuery.SQL.Clear;
      ProcessSendData;
    end;

  except
    on E: Exception do
    begin
      NetQuery.Reconnect;
      ExecFuncError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

procedure TServerConnBuffer.ExecFuncSQLCacheUpdate;
var
  S1: TNetProcString;
  ParamLen, I: integer;
begin
  try
    ParamLen := ReadInt;
    Log('Get Cache Count: ' + IntToStr(ParamLen));
    NetSQLRun.Close;
    I := 0;
    while ReadByte = 0 do
    begin
      try
        S1 := ReadStr;
        if S1 <> '' then
        begin
          NetSQLRun.SQL.Clear;
{$IFDEF FPC}
          NetSQLRun.SQL.Text := S1;
{$ELSE}
          NetSQLRun.SQL.Text := UTF8Decode(S1);
{$ENDIF}
          Log('CachedUpdate: ' + NetSQLRun.SQL.Text);
          try
            NetSQLRun.ExecSQL;
          except
            NetQuery.Reconnect;
          end;
        end;
        Inc(I);
      except
        NetQuery.Reconnect;
      end;
    end;
    NetQuery.SQL.Clear;

    WriteByte(1);
    WriteInt(I);
    WriteByte(1);
    Log('Return Cache Count: ' + IntToStr(I));
    ProcessSendData;

  except
    on E: Exception do
    begin
      ExecFuncError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

procedure TServerConnBuffer.ExecFuncLoadFile;
begin
end;

procedure TServerConnBuffer.ExecFuncError(const Msg: TNetProcString);
begin
  SetInstruction(IstError);
  WriteStr(Msg);
  ProcessSendData;
  WriteLog(Msg);
end;

procedure TServerConnBuffer.ExecFuncLogin();
var
  UsrName, UsrPsw: TNetProcString;
  RetByte: byte;
  HasLoggedOn: TLogonStyle;
begin
  UsrName := ReadStr;
  UsrPsw := ReadStr;
  {$IFDEF DBSTOREDLOGON}
  HasLoggedOn := ServerCheckLogon(UsrName, UsrPsw);
  {$ELSE}
  if Assigned(FOnUserLogonCall) then
  begin
    HasLoggedOn := FOnUserLogonCall(UsrName, UsrPsw);
    TempClientFunctions := 'ALL';
    TempClientPermTables := 'ALL';
    TempClientReadTables := 'ALL';
    TempORGID1 := 'ALL';
    TempORGID2 := 'ALL';
    LogonStyle := True;
    TempSubFuncs := 'ALL';
  end;
  {$ENDIF}
  if (UsrName = SuperUsr) and (UsrPsw = SuperPsw) then
    HasLoggedOn := LogedOnServer;
  if HasLoggedOn = LogedOnServer then
    LogonStyle := True
  else
    LogonStyle := False;
  RetByte := Ord(HasLoggedOn);
  SetInstruction(IstLogin);
  WriteByte(RetByte);
  ProcessSendData;
  if LogonStyle then
    Log('Logon success')
  else
    Log('Logon failed');
end;

function TServerConnBuffer.DoStoredProc(FDataQuery: TServerSockQuery;
  FUser, FSubFuncs: TNetProcString): boolean;
var
  CustIsts, SQLx: byte;
  ReslSQLV, ClientParam: TNetProcString;
  StoredParamNum, xInNum, xOutNum: integer;
begin
  Result := False;
  try
    if (Pos('Stored', TempClientFunctions) > 0) or
      (TempClientFunctions = 'ALL') then
    begin
      CustIsts := GetCustInstruc;
      SQLx := GetSQLStyle(CustIsts);
      ClientParam := GetCustParam;
      StoredParamNum := GetStoredParamNum(CustIsts);
      xInNum := GetStoredInNum(CustIsts);
      xOutNum := GetStoredOutNum(CustIsts);

      ReslSQLV := DoSSToredProc(CustIsts, PAnsiChar(FSubFuncs),
        PAnsiChar(FUser), PAnsiChar(ClientParam));

      Log('CustMessage: ' + IntToStr(integer(CustIsts)) + '*' +
        IntToStr(SQLx) + '*' + ReslSQLV);
      SetInstruction(IstStoredProc);
      WriteByte(SQLx);
      ServerStoredProc(CustIsts, SQLx, FUser, ReslSQLV, StoredParamNum,
        xInNum, xOutNum); // 'select * from bbb');
    end
    else
    begin
      Log('Not permitted Stored Proc');
      SetInstruction(IstStoredProc);
      WriteByte(254);
      ProcessSendData;
    end;
  finally
  end;
end;

function TServerConnBuffer.ServerCustBefore(CustInstrucV, SQLVS: byte;
  FUser, ScriptValue, SQLValueE, SQLValueOBefore: TNetProcString): TNetProcString;
var
  TempStr, RetStr: TNetProcString;
  I: integer;
begin
  RetStr := '';
  if SQLValueOBefore <> '' then
  begin
    try
      NetQuery.SQL.Clear;
{$IFNDEF FPC}
      SQLValueOBefore := UTF8Decode(SQLValueOBefore);
{$ELSE}
      SQLValueOBefore := SQLValueOBefore;
{$ENDIF}
      NetQuery.SQL.Text := SQLValueOBefore;
      Log('CustProcBeforeSQL: ' + SQLValueOBefore);
      NetQuery.SuperOpen;
      if NetQuery.NetData.RecordCount > 0 then
        while not NetQuery.NetData.EOF do
        begin
          for I := 0 to NetQuery.NetData.FieldCount - 1 do
          begin
{$IFDEF OverRad2k7}
            TempStr := NetQuery.NetData.Fields[I].AsAnsiString;
{$ELSE}
            TempStr := NetQuery.NetData.Fields[I].AsString;
{$ENDIF}
{$IFNDEF FPC}
            if NetQuery.NetData.Fields[I].DataType in
              [ftString, ftFixedChar] then
              TempStr := UTF8Encode(TempStr);
{$ENDIF}
            if (I = 0) and (NetQuery.NetData.RecNo = 1) then
              RetStr := TempStr
            else
              RetStr := RetStr + ';' + TempStr;
          end;
          NetQuery.NetData.Next;
        end;
      NetQuery.Close;
      NetQuery.SQL.Clear;
    except
      on E: Exception do
      begin
        NetQuery.Reconnect;
        ExecFuncError(E.ClassName + ': ' + E.Message);
        Exit;
      end;
    end;
  end;
  Result := RetStr;
end;

function TServerConnBuffer.ServerCustProc(CustInstrucV, SQLVS: byte;
  FUser, ScriptValue, SQLValueE, SQLValueOBefore, SQLValueOAfter:
  TNetProcString): TNetProcString;
var
  RetStr: TNetProcString;
  ScriptCount, RunCount: integer;
begin
  RetStr := '';

  ScriptCount := 0;
  RunCount := 0;

  if (NetSQLProc <> nil) and (ScriptValue <> '') then
    try
      NetSQLProc.Script.Clear;
{$IFNDEF FPC}
      NetSQLProc.Script.Text := UTF8Decode(ScriptValue);
{$ELSE}
      NetSQLProc.Script.Text := ScriptValue;
{$ENDIF}
      Log('ClientScript: ' + #13#10 + NetSQLProc.Script.Text);

      NetSQLProc.StartTransaction;
      try
        NetSQLProc.ExecScript;
      except
        NetSQLProc.RollBack;
      end;
      NetSQLProc.Commit;

      ScriptCount := NetSQLProc.RowsAffected;
      NetSQLProc.Script.Clear;
    except
      on E: Exception do
      begin
        NetSQLProc.Reconnect;
        ExecFuncError(E.ClassName + ': ' + E.Message);
        Exit;
      end;
    end;

  if SQLValueE <> '' then
  begin
    try
      NetSQLRun.SQL.Clear;
      NetSQLRun.SQL.Text := SQLValueE;
{$IFNDEF FPC}
      NetSQLRun.SQL.Text := UTF8Decode(SQLValueE);
{$ENDIF}
      NetSQLRun.SuperExecSQL;
      RunCount := NetSQLRun.RowsAffected;
      NetSQLRun.SQL.Clear;
    except
      on E: Exception do
      begin
        NetSQLProc.Reconnect;
        ExecFuncError(E.ClassName + ': ' + E.Message);
        RetStr := 'ERR';
        Result := 'ERR';
        Exit;
      end;
    end;
  end;

  WriteInt(ScriptCount + RunCount);
  RetStr := 'OK' + ';' + IntToStr(ScriptCount) + ';' + IntToStr(RunCount);
  Result := RetStr;
  Log(RetStr + ' ^ ' + IntToStr(ScriptCount) + ' ^ ' + IntToStr(RunCount));
end;

function TServerConnBuffer.ServerCustAfter(CustInstrucV, SQLVS: byte;
  FUser, ScriptValue, SQLValueE, SQLValueOBefore, SQLValueOAfter:
  TNetProcString): TNetProcString;
var
  TempStr, RetStr: TNetProcString;
  I: integer;
begin
  RetStr := '';
  if SQLValueOAfter <> '' then
  begin
    try
      NetQuery.SQL.Clear;
{$IFNDEF FPC}
      SQLValueOAfter := UTF8Decode(SQLValueOAfter);
{$ELSE}
      SQLValueOAfter := SQLValueOAfter;
{$ENDIF}
      NetQuery.SQL.Text := SQLValueOAfter;
      Log('CustProcAfterSQL: ' + SQLValueOAfter);
      NetQuery.SuperOpen;
      RetStr := '';
      if NetQuery.NetData.RecordCount > 0 then
        while not NetQuery.NetData.EOF do
        begin
          for I := 0 to NetQuery.NetData.FieldCount - 1 do
          begin
{$IFDEF OverRad2k7}
            TempStr := NetQuery.NetData.Fields[I].AsAnsiString;
{$ELSE}
            TempStr := NetQuery.NetData.Fields[I].AsString;
{$ENDIF}
{$IFNDEF FPC}
            if NetQuery.NetData.Fields[I].DataType in
              [ftString, ftFixedChar] then
              TempStr := UTF8Encode(TempStr);
{$ENDIF}
            if (I = 0) and (NetQuery.NetData.RecNo = 1) then
              RetStr := TempStr
            else
              RetStr := RetStr + ';' + TempStr;
          end;
          NetQuery.NetData.Next;
        end;

      NetQuery.Close;
      NetQuery.SQL.Clear;
    except
      on E: Exception do
      begin
        NetQuery.Reconnect;
        ExecFuncError(E.ClassName + ': ' + E.Message);
        Exit;
      end;
    end;
  end;
  Result := RetStr;
end;

function TServerConnBuffer.DoDynamicCustProc(FDataQuery: TServerSockQuery;
  NetSQLProc: TServerSockQuery; FZStProc: TServerSockQuery;
  FUser, FSubFuncs: TNetProcString): boolean;
var
  CustIsts, SQLx: byte;
  ReslScriptV, ReslSQLVE, ReslSQLVOBefore, ReslSQLVOAfter, ClientParam,
  RetBefore, RetProc, RetAfter, RetVL: TNetProcString;
begin
  Result := False;
  try
    if (Pos('ClDLL', TempClientFunctions) > 0) or (TempClientFunctions = 'ALL') then
    begin
      CustIsts := GetCustInstruc;
      SQLx := GetSQLStyle(CustIsts);
      ClientParam := GetCustParam;
      ReslSQLVOBefore := DoSCustProcSQLBeforeOpen(CustIsts,
        PAnsiChar(FUser), PAnsiChar(FSubFuncs), PAnsiChar(ClientParam));
      Log('CustProcMessage: ' + IntToStr(integer(CustIsts)) + '^' +
        IntToStr(SQLx) + '^' + ClientParam);
      // ReslSQLVE); // + '^' + ReslSQLVOBefore);
      SetInstruction(IstDynamicCustProc);
      WriteByte(SQLx);
      WriteByte(CustIsts);
      RetBefore := ServerCustBefore(CustIsts, SQLx, FUser, ReslScriptV,
        ReslSQLVE, ReslSQLVOBefore);
      ReslScriptV := DoSCustProcScript(CustIsts, PAnsiChar(FUser),
        PAnsiChar(FSubFuncs), PAnsiChar(ClientParam), PAnsiChar(RetBefore));
      ReslSQLVE := DoSCustProcSQLExec(CustIsts, PAnsiChar(FUser),
        PAnsiChar(FSubFuncs), PAnsiChar(ClientParam), PAnsiChar(RetBefore));
      RetProc := ServerCustProc(CustIsts, SQLx, PAnsiChar(FUser),
        ReslScriptV, ReslSQLVE, ReslSQLVOBefore, ReslSQLVOAfter);
      ReslSQLVOAfter := DoSCustProcSQLAfterOpen(CustIsts, PAnsiChar(FUser),
        PAnsiChar(FSubFuncs), PAnsiChar(ClientParam), PAnsiChar(RetBefore));
      RetAfter := ServerCustAfter(CustIsts, SQLx, PAnsiChar(FUser),
        ReslScriptV, ReslSQLVE, ReslSQLVOBefore, ReslSQLVOAfter);
      RetVL := DoSReturnValue(CustIsts, PAnsiChar(FUser),
        PAnsiChar(FSubFuncs), PAnsiChar(ClientParam), PAnsiChar(RetBefore),
        PAnsiChar(RetProc), PAnsiChar(RetAfter));
      WriteStr(RetAfter);
      WriteStr(RetVL);
      ProcessSendData;
    end
    else
    begin
      Log('Not permitted ClDLL');
      SetInstruction(IstDynamicCustProc);
      WriteByte(254);
      ProcessSendData;
    end;
  finally
  end;
end;

function TServerConnBuffer.DoInternalSpecialSQL(CustSubInstrucs: byte;
  FUser: string; ASQLStyle: SQLStyle; SQLText: TNetProcString): boolean;
begin
  Result := False;
  try
    begin
      Log('Internal SQL: ' + IntToStr(CustSubInstrucs) + '^' + SQLText);
      ServerSQLProc(CustSubInstrucs, ASQLStyle, FUser, SQLText);
    end;
  finally
  end;
end;

function TServerConnBuffer.DoInternalCustProc(FDataQuery: TServerSockQuery;
  NetSQLProc: TServerSockQuery; FZStProc: TServerSockQuery;
  FUser, FSubFuncs: TNetProcString): boolean;
var
  CustIsts, CustSubIst: byte;
  ClientParam, RetVL: TNetProcString;
begin
  Result := False;
  try
    if (Pos('INNER', TempClientFunctions) > 0) or (TempClientFunctions = 'ALL') then
    begin
      CustIsts := GetCustInstruc;
      CustSubIst := 0;
      if CustIsts = byte(InternalSQLInstruciton) then
        CustSubIst := ReadByte;
      ClientParam := GetCustParam;
      Log('CustInternalProc: ' + IntToStr(integer(CustIsts)) + '^' + ClientParam);
      // ReslSQLVE); // + '^' + ReslSQLVOBefore);
      if Assigned(FOnCustInternalCall) then
        RetVL := FOnCustInternalCall(CustIsts, CustSubIst,
          PAnsiChar(ClientParam), FDataQuery, NetSQLProc, FZStProc,
          PAnsiChar(FUser), PAnsiChar(FSubFuncs))
      else
        RetVL := 'No service';
      Log(RetVL);
      SetInstruction(IstInternalCustProc);
      WriteByte(CustIsts);
      if CustIsts = byte(InternalSQLInstruciton) then
        DoInternalSpecialSQL(CustSubIst, FUser, SelectSQL, RetVL)
      else
        WriteStr(RetVL);
      ProcessSendData;
    end
    else
    begin
      Log('Not permitted INNER');
      SetInstruction(IstInternalCustProc);
      WriteByte(254);
      ProcessSendData;
    end;
  finally
  end;
end;

function TServerConnBuffer.DoSQLScript(FDataQuery: TServerSockQuery;
  FUser, FSubFuncs: TNetProcString): boolean;
var
  CustIsts, SQLx: byte;
  ReslSQLV, ClientParam: TNetProcString;
begin
  Result := False;
  try
    if (Pos('SCRIP', TempClientFunctions) > 0) or (TempClientFunctions = 'ALL') then
    begin
      CustIsts := GetCustInstruc;
      SQLx := GetSQLStyle(CustIsts);
      ClientParam := GetCustParam;
      ReslSQLV := DoSScriptProc(CustIsts, PAnsiChar(FUser),
        PAnsiChar(FSubFuncs), PAnsiChar(ClientParam));
      Log('CustMessage: ' + IntToStr(integer(CustIsts)) + '*' +
        IntToStr(SQLx) + '*' + ReslSQLV);
      SetInstruction(IstSQLScript);
      WriteByte(SQLx);
      ServerScriptProc(CustIsts, SQLx, FUser, ReslSQLV);
    end
    else
    begin
      Log('Not permitted SCRIPT');
      SetInstruction(IstSQLScript);
      WriteByte(254);
      ProcessSendData;
    end;
  finally
  end;
end;

function TServerConnBuffer.DoSpecialSQL(FDataQuery: TServerSockQuery;
  FUser, FSubFuncs: TNetProcString): boolean;
var
  CustIsts, SQLx: byte;
  ReslSQLV, ClientParam: TNetProcString;
begin
  Result := False;
  try
    if (Pos('SPSQL', TempClientFunctions) > 0) or (TempClientFunctions = 'ALL') then
    begin
      CustIsts := GetCustInstruc;
      SQLx := GetSQLStyle(CustIsts);
      ClientParam := GetCustParam;
      ReslSQLV := DoSSpecialQuery(CustIsts, PAnsiChar(FUser),
        PAnsiChar(FSubFuncs), PAnsiChar(ClientParam));
      Log('CustMessage: ' + IntToStr(integer(CustIsts)) + '*' +
        IntToStr(SQLx) + '*' + ReslSQLV);
      SetInstruction(IstSpecialSQL);
      WriteByte(SQLx);
      ServerSQLProc(CustIsts, SQLStyle(SQLx), FUser, ReslSQLV);
      // 'select * from bbb');
    end
    else
    begin
      Log('Not permitted SPSQL');
      SetInstruction(IstSpecialSQL);
      WriteByte(254);
      ProcessSendData;
    end;
  finally

  end;
end;

function TServerConnBuffer.GetCustInstruc: byte;
begin
  try
    Result := ReadByte;
  except
    Result := 254;
  end;
end;

function TServerConnBuffer.GetCustParam: TNetProcString;
begin
{$IFDEF FPC}
  Result := ReadStr;
{$ELSE}
  Result := UTF8Decode(ReadStr);
{$ENDIF}
end;

function TServerConnBuffer.ServerSQLProc(CustInstrucV: byte; SQLVS: SQLStyle;
  FUser, SQLValue: TNetProcString): byte;
var
  TempStr, TempField: TNetProcString;
  I, RecCont: integer;
begin
  Result := 0;
  if SQLValue = '' then
    Exit;

  if (SQLVS <> ExecuteSQL) and (SQLVS <> SelectSQL) then
    Exit;

  try
    NetQuery.Close;
    NetSQLRun.Close;
{$IFNDEF FPC}
    NetQuery.SQL.Text := UTF8Decode(SQLValue);
{$ELSE}
    NetQuery.SQL.Text := SQLValue;
{$ENDIF}
    NetSQLRun.SQL.Text := NetQuery.SQL.Text;

    Log('ClientQuery: ' + NetQuery.SQL.Text);

    if SQLVS = ExecuteSQL then
    begin
      NetSQLRun.SuperExecSQL;
      WriteByte(CustInstrucV);
      WriteInt(NetSQLRun.RowsAffected);
      NetSQLRun.SQL.Clear;

      ProcessSendData;
      Exit;
    end;

    if SQLVS = SelectSQL then
    begin
      WriteByte(CustInstrucV);
      RecCont := 0;
      if NetQuery.SuperOpen then
      begin
        begin
          WriteInt(NetQuery.NetData.FieldCount);

          for I := 0 to NetQuery.NetData.FieldCount - 1 do
          begin
{$IFDEF FPC}
            TempField := NetQuery.NetData.Fields[I].FieldName + ';';
{$ELSE}
            TempField := UTF8Encode(NetQuery.NetData.Fields[I].FieldName) + ';';
{$ENDIF}
            WriteStr(TempField + IntToStr(
              integer(NetQuery.NetData.Fields[I].DataType)) + ';' +
              IntToStr(NetQuery.NetData.Fields[I].Size) + ';' +
              IntToStr(integer(NetQuery.NetData.Fields[I].Required)));
          end;
        end;

        RecCont := 0;
        begin
          while not NetQuery.NetData.EOF do
          begin
            WriteByte(0);
            for I := 0 to NetQuery.NetData.FieldCount - 1 do
            begin
              if NetQuery.NetData.Fields[I].DataType in [ftDate, ftTime, ftDateTime] then
                TempStr := FloatToDotStr(NetQuery.NetData.Fields[I].AsDateTime)
              else
              if NetQuery.NetData.Fields[I].DataType in [ftFloat, ftCurrency] then
                TempStr := FloatToDotStr(NetQuery.NetData.Fields[I].AsFloat)
              else
              begin
{$IFDEF OverRad2k7}
                TempStr := NetQuery.NetData.Fields[I].AsAnsiString;
{$ELSE}
                TempStr := NetQuery.NetData.Fields[I].AsString;
{$ENDIF}
{$IFNDEF FPC}
                if NetQuery.NetData.Fields[I].DataType in
                  [ftString, ftFixedChar] then
                  TempStr := UTF8Encode(TempStr);
{$ENDIF}
                WriteStr(TempStr);
              end;
            end;
            Inc(RecCont);
            NetQuery.NetData.Next;
          end;
        end;
      end;

      WriteByte(1);
      WriteInt(RecCont);
      WriteByte(1);
      NetQuery.SQL.Clear;

      ProcessSendData;
    end;

  except
    on E: Exception do
    begin
      NetQuery.Reconnect;
      ExecFuncError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

function TServerConnBuffer.ServerStoredProc(CustInstrucV, SQLVS: byte;
  FUser, SQLValue: TNetProcString; ParamNum, InNum, RetNum: integer): byte;
var
  OutStr: TNetProcString;
  I, TmpI: integer;
  temur: StrArray;
begin
  Result := 0;
  if SQLValue = '' then
    Exit;

  if NetStoredProc = nil then
    Exit;

  try
    NetStoredProc.Close;
    Log('StoredQuery: ' + NetQuery.SQL.Text);
    NetStoredProc.Params.Clear;
    temur := GetBufDotStr(SQLValue, ';', ParamNum);
    NetStoredProc.StoredProcName := temur[1];
    NetStoredProc.Params.Clear;

    for I := 0 to ParamNum - 1 do
    begin
      if (I mod 3) = 0 then
        with NetStoredProc do
        begin
          Params.add;
          Params[I].ParamType := TParamType(StrToIntDef(temur[I + 2], 0));
          Params[I].DataType := TFieldType(StrToIntDef(temur[I + 3], 0));
          Params[I].Name := temur[I + 4];
        end;
    end;

    for I := 0 to InNum - 1 do
    begin
      if (I mod 2) = 0 then
      begin
        TmpI := StrToIntDef(temur[I + ParamNum + 2], 0);
        NetStoredProc.Params[TmpI].Value := temur[I + ParamNum + 3];
      end;
    end;

    NetStoredProc.ExecStoredProc;

    OutStr := '';

    for I := 0 to RetNum - 1 do
    begin
      if (I mod 2) = 0 then
      begin
        TmpI := StrToIntDef(temur[I + ParamNum + InNum + 2], 0);
        if I = 0 then
          OutStr := OutStr + NetStoredProc.Params[TmpI].Value
        else
          OutStr := OutStr + ';' + NetStoredProc.Params[TmpI].Value;
      end;
    end;
    WriteByte(CustInstrucV);
    WriteInt(NetStoredProc.RowsAffected);
    WriteStr(OutStr);
    ProcessSendData;
  except
    on E: Exception do
    begin
      NetStoredProc.Reconnect;
      WriteByte(CustInstrucV);
      WriteInt(40);
      ExecFuncError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

function TServerConnBuffer.ServerScriptProc(CustInstrucV, SQLVS: byte;
  FUser, SQLValue: TNetProcString): byte;
begin
  Result := 0;
  if SQLValue = '' then
    Exit;
  if NetSQLProc = nil then
    Exit;

  try
{$IFNDEF FPC}
    NetSQLProc.Script.Text := UTF8Decode(SQLValue);
{$ELSE}
    NetSQLProc.Script.Text := SQLValue;
{$ENDIF}
    NetSQLProc.Script.Clear;
    NetSQLProc.Script.Text := SQLValue;

    Log('ClientScript: ' + NetSQLProc.Script.Text);

    NetSQLProc.StartTransaction;
    try
      NetSQLProc.ExecScript;
    except
      NetSQLProc.RollBack;
    end;
    NetSQLProc.Commit;

    WriteByte(CustInstrucV);
    WriteInt(NetSQLProc.RowsAffected);
    NetSQLProc.Script.Clear;

    ProcessSendData;

  except
    on E: Exception do
    begin
      NetSQLProc.Reconnect;
      WriteByte(CustInstrucV);
      WriteInt(40);
      ExecFuncError(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

procedure TServerConnBuffer.ExecFuncChangPSW;
var
  TempPSW, PSW1, PSW2: TNetProcString;
  RetByte: byte;
begin
  RetByte := 40;

  try
    NetQuery.Close;
    NetQuery.SQL.Clear;
    NetQuery.SQL.add('select * from OPERASINFO where USR = ' + FUSR);
    NetQuery.SuperOpen;
{$IFDEF OverRad2k7}
    TempPSW := Uppercase(Trim(NetQuery.NetData.FieldByName('PSW').AsAnsiString));
{$ELSE}
    TempPSW := Uppercase(Trim(NetQuery.NetData.FieldByName('PSW').AsString));
{$ENDIF}
    PSW1 := ReadStr;
    PSW2 := ReadStr;
    Log(PSW1 + ' * ' + PSW2);

    if PSW1 = TempPSW then
    begin
      NetSQLRun.Close;
      NetSQLRun.SQL.Clear;
      NetSQLRun.SQL.add('update OPERASINFO set PSW = ' + QuotedStr(PSW2) +
        ' where USR = ' + FUSR);
      NetSQLRun.SuperExecSQL;
      RetByte := 10;
    end;

    NetQuery.Close;
    NetSQLRun.Close;
  except
    on E: Exception do
    begin
      NetQuery.Reconnect;
      ExecFuncError(E.ClassName + ': ' + E.Message);
    end;
  end;

  SetInstruction(IstChangePSW);
  WriteByte(RetByte);
  ProcessSendData;
end;

procedure LoadAllFunc;
begin
  @GetSQLStyle := GetProcAddress(DLibHandle, 'GetSQLStyle');
  @GetStoredParamNum := GetProcAddress(DLibHandle, 'GetStoredParamNum');
  @GetStoredInNum := GetProcAddress(DLibHandle, 'GetStoredInNum');
  @GetStoredOutNum := GetProcAddress(DLibHandle, 'GetStoredOutNum');
  @DoSSToredProc := GetProcAddress(DLibHandle, 'DoSSToredProc');
  @DoSScriptProc := GetProcAddress(DLibHandle, 'DoSScriptProc');
  @DoSCustProcScript := GetProcAddress(DLibHandle, 'DoSCustProcScript');
  @DoSCustProcSQLExec := GetProcAddress(DLibHandle, 'DoSCustProcSQLExec');
  @DoSCustProcSQLBeforeOpen := GetProcAddress(DLibHandle, 'DoSCustProcSQLBeforeOpen');
  @DoSCustProcSQLAfterOpen := GetProcAddress(DLibHandle, 'DoSCustProcSQLAfterOpen');
  @DoSSpecialQuery := GetProcAddress(DLibHandle, 'DoSSpecialQuery');
  @DoSReturnValue := GetProcAddress(DLibHandle, 'DoSReturnValue');
end;

initialization

  DLibHandle := 0;
{$IFDEF LINUX}
  DLibHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + 'DBOLSPlugin.so'));
{$ELSE}
  DLibHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + 'DBOLSPlugin.dll'));
{$ENDIF}
  try
    if DLibHandle <> 0 then
      LoadAllFunc;
  except
  end;

finalization

  if DLibHandle <> 0 then
    FreeLibrary(DLibHandle);

end.

