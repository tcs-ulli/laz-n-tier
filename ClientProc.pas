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

unit ClientProc;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes, DataProcUtils, SynaCSockets, SysUtils, DB, {$IFNDEF FPC}WideStrUtils,
  {$ENDIF}{$IFDEF VER180}Windows, {$ELSE}DynLibs, {$ENDIF}MD5;

type
  TClientConnBuffer = class(TOnlineDataBuffer)
  Protected
    FDataSet: TDataSet;
    procedure ProcessData; Override;
    procedure ProcessError(const Msg: TNetProcString);
  Public
    FSocket: TCSocket;
    ReturnStr: TNetProcString;
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; Override;
    procedure Log(const Msg: TNetProcString);
    procedure AddSQLParam(const ParamName: TNetProcString; ParamType: TFieldType;
      const Value: TNetProcString);
    function OpenSQL(DataSet: TDataset; const SQL: TNetProcString; HasFields, TmpRF:
      boolean): Integer;
    function UpdateFieldDefs(DataSet: TDataset; const SQL: TNetProcString): Integer;
    function ExecSQL(DataSet: TDataset; const SQL: TNetProcString): Integer;
    function CacheExecSQL(DataSet: TDataset; const SQL: TNetProcString): Integer;
    function RunSQL: Integer;
    function ClientRunSQL(SQLVx: Byte): Integer;
    function LogOnServer(UsrName, UsrPsw: TNetProcString): Boolean;
    function LogOnServer2(UsrName, UsrPsw: TNetProcString): TLogonStyle;
    function GetServerTime: TNetProcString;
    function ChangePassword(OldPSW, NewPSW: TNetProcString): Boolean;
    function DoSQLScript(DataSet: TDataset; CPInstruc: Byte; CliParam:
      TNetProcString): integer;
    function DoStoredProc(DataSet: TDataset; CPInstruc: Byte; CliParam:
      TNetProcString): integer;
    function DOSpecialSQL(DataSet: TDataset; CPInstruc: Byte; CliParam:
      TNetProcString): integer;
    function DoInternalSpecialSQL(DataSet: TDataset; SubInstruc: Byte; CliParam:
      TNetProcString): integer;
    function ProcessDynamicCustProc(DataSet: TDataset; CPInstruc: Byte;
      CliParam: TNetProcString): TNetProcString;
    function ProcessInternalCustProc(DataSet: TDataset; CPInstruc: Byte;
      CliParam: TNetProcString): TNetProcString;
  end;

var
  ClientLog: TStrings = nil;

implementation

{$I MemDBRADv.inc}

function GetFileName(str: string): string;
var
  i: Integer;
  x, y: string;
begin
  x := ExtractFileName(str);
  y := ExtractFileExt(str);
  i := Pos(y, x);
  if i <> 0 then
    Result := Copy(x, 0, i - 1)
  else
    Result := '';
end;

procedure Writelog(value: TNetProcString);
var
  f: textFile;
  s: TNetProcString;
begin
{$IFDEF EnableLOG}
  s := value;
  s := GetFileName(ParamStr(0)) + '.log';
  assignfile(f, s);
  if fileexists(s) then
    append(f)
  else
    rewrite(f);
  try
    writeln(f, value);
  finally
    Closefile(f);
  end;
{$ENDIF}
end;

constructor TClientConnBuffer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSet := nil;
  SetInstruction(IstNone);
  LoginTime := Now;
end;

destructor TClientConnBuffer.Destroy;
begin
  inherited Destroy;
end;

procedure TClientConnBuffer.Log(const Msg: TNetProcString);
begin
  if ClientLog <> nil then
    ClientLog.add(msg);
  Writelog(Msg);
end;

procedure TClientConnBuffer.ProcessData;
begin
  case Instruction of
    IstError:
      ProcessError(ReadStr);
  end;
end;

function TClientConnBuffer.OpenSQL;
begin
  Result := 0;
  Log(FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
  FDataSet := DataSet;
  SetInstruction(IstSQL);
  if HasFields or TmpRF then
    WriteByte(Byte(IstSQLWithFields))
  else
    WriteByte(Byte(IstSQLOpen));
{$IFDEF FPC}
  WriteStr(SQL);
{$ELSE}
  WriteStr(UTF8Encode(SQL));
{$ENDIF}
  Log(FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
end;

function TClientConnBuffer.UpdateFieldDefs;
begin
  Result := 0;
  FDataSet := DataSet;
  SetInstruction(IstSQL);
  WriteByte(Byte(IstSQLFieldDefs));
{$IFDEF FPC}
  WriteStr(SQL);
{$ELSE}
  WriteStr(UTF8Encode(SQL));
{$ENDIF}
end;

function TClientConnBuffer.ExecSQL;
begin
  Result := 0;
  FDataSet := DataSet;
  SetInstruction(IstSQL);
  WriteByte(Byte(IstSQLExec));
  WriteStr(UTF8Encode(SQL));
  WriteByte(1);
  ProcessSendData;
  Log('ExecSQL Send:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
  RecvBuffer := FSocket.ProcessData(SendBuffer);
  Log('ExecSQL Send:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
  Log('Received string length:' + IntToStr(Length(RecvBuffer)));
  ProcessReadData;
  if TSQLInstruction(ReadByte) = IstSQLExec then
    Result := ReadInt;
  FDataSet := nil;
end;

function TClientConnBuffer.LogOnServer(UsrName, UsrPsw: TNetProcString): Boolean;
begin
  SetInstruction(IstLogin);
  WriteStr(UsrName);
  WriteStr(StrMD5(UsrName + StrMD5(UsrPsw) + FormatDateTime('yyyymmdd', Now)));
  ProcessSendData;
  RecvBuffer := FSocket.ProcessData(SendBuffer);
  ProcessReadData;
  Result := ReadByte = Byte(Ord(LogedOnServer));
end;

function TClientConnBuffer.LogOnServer2(UsrName, UsrPsw: TNetProcString):
  TLogonStyle;
begin
  SetInstruction(IstLogin);
  WriteStr(UsrName);
  WriteStr(StrMD5(UsrName + StrMD5(UsrPsw) + FormatDateTime('yyyymmdd', Now)));
  ProcessSendData;
  RecvBuffer := FSocket.ProcessData(SendBuffer);
  ProcessReadData;
  Result := TLogonStyle(ReadByte);
end;

function TClientConnBuffer.GetServerTime: TNetProcString;
begin
  SetInstruction(IstTime);
  ProcessSendData;
  RecvBuffer := FSocket.ProcessData(SendBuffer);
  ProcessReadData;
  Result := ReadStr;
end;

function TClientConnBuffer.ChangePassword(OldPSW, NewPSW: TNetProcString): Boolean;
begin
  SetInstruction(IstChangePSW);
  WriteStr(StrMD5(OldPSW));
  WriteStr(StrMD5(NewPSW));
  ProcessSendData;
  RecvBuffer := FSocket.ProcessData(SendBuffer);
  ProcessReadData;
  Result := ReadByte = 10;
end;

function TClientConnBuffer.CacheExecSQL;
var
  TempCacheList: TStringList;
  I: integer;
  TempStr: TNetProcString;
begin
  TempCacheList := TStringList.Create;
  Result := 0;
  FDataSet := DataSet;
  SetInstruction(IstSQLCacheExec);
  TempCacheList.Clear;
  TempCacheList.Text := SQL;
  I := 0;
  while I < TempCacheList.Count do
  begin
    if Length(Trim(TempCacheList[I])) = 0 then
    begin
      TempCacheList.Delete(I);
    end
    else
      I := I + 1;
  end;
  Log('Send Cache: ' + IntToStr(TempCacheList.Count));
  WriteInt(TempCacheList.Count);
  for I := 0 to TempCacheList.Count - 1 do
  begin
    if Length(Trim(TempCacheList[I])) > 0 then
    begin
      WriteByte(0);
      TempStr := UTF8Encode(TNetProcString(TempCacheList[I]));
      WriteStr(TempStr);
    end;
    Log(TempStr);
  end;

  WriteByte(1);
  ProcessSendData;
  Log('ExecSQL Send:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
  if SendBuffer <> '' then
  begin
    RecvBuffer := FSocket.ProcessData(SendBuffer);
    Log('ExecSQL Send:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));

    Log('Received string length:' + IntToStr(Length(RecvBuffer)));
    ProcessReadData;
    if ReadByte = 1 then
      Result := ReadInt;
  end;
  Log('Get Cache Count: ' + IntToStr(Result));
  FDataSet := nil;
  TempCacheList.Free;
end;

procedure TClientConnBuffer.AddSQLParam(const ParamName: TNetProcString; ParamType:
  TFieldType; const Value: TNetProcString);
begin
  WriteByte(0);
  WriteStr(ParamName + ';' + IntToStr(Integer(ParamType)));
  WriteStr(Value);
end;

function TClientConnBuffer.RunSQL;
var
  I, _FieldCount: Integer;
  S, Name: TNetProcString;
  Datatype: TFieldType;
  Size: Integer;
  Required: Boolean;
  SQLIsttion: TSQLInstruction;
  RecNumInParam: integer;
  TempStr: TNetProcString;
begin
  Result := 0;
  WriteByte(1);
  ProcessSendData;
  Log('Send:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
  RecvBuffer := FSocket.ProcessData(SendBuffer);
  Log('Open:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
  ProcessReadData;
  try
    SQLIsttion := TSQLInstruction(ReadByte);
    if SQLIsttion = IstSQLExec then
    begin
      Result := ReadInt;
      FDataSet := nil;
    end
    else
    begin
      FDataSet.DisableControls;
      try
        FDataSet.Close;
        _FieldCount := FDataSet.FieldDefs.Count;
        if SQLIsttion = IstSQLWithFields then
        begin
          FDataSet.FieldDefs.Clear;
          _FieldCount := ReadInt;
          for I := 0 to _FieldCount - 1 do
          begin
            S := ReadStr;
            Name := RetrieveStr(S, ';');
            Datatype := TFieldType(StrToInt(RetrieveStr(S, ';')));
            Size := StrToInt(RetrieveStr(S, ';'));
            Required := Boolean(StrToInt(RetrieveStr(S, ';')));
            FDataSet.FieldDefs.Add(Name, DataType, Size, Required);
          end;
        end;
        if (SQLIsttion = IstSQLOpen) or (SQLIsttion =
          IstSQLWithFields) then
        begin
          FDataSet.Open;
          while ReadByte = 0 do
          begin
            FDataSet.Append;
            for I := 0 to _FieldCount - 1 do
            begin
              TempStr := ReadStr;
              if FDataSet.Fields[I].DataType in [ftDate, ftTime, ftDateTime] then
                FDataSet.Fields[I].AsDateTime := DotStrToFloat(TempStr)
              else
                if FDataSet.Fields[I].DataType in [ftFloat, ftCurrency] then
                  FDataSet.Fields[I].AsFloat := DotStrToFloat(TempStr)
                else
                begin
{$IFDEF FPC}
                  FDataSet.Fields[I].AsString := TempStr;
{$ELSE}
                  if FDataSet.Fields[I].DataType in [ftString, ftFixedChar]
                    then
{$IFDEF OverRad2k7}
                    FDataSet.Fields[I].AsAnsiString := UTF8Decode(TempStr)
{$ELSE}
                    FDataSet.Fields[I].AsString := UTF8Decode(TempStr)
{$ENDIF}
                  else
{$IFDEF OverRad2k7}
                    FDataSet.Fields[I].AsAnsiString := TempStr;
{$ELSE}
                    FDataSet.Fields[I].AsString := TempStr;
{$ENDIF}
{$ENDIF}
                end;
            end;
            FDataSet.Post;
            Inc(Result);
          end;
          FDataSet.First;
        end;

        RecNumInParam := ReadInt;
        if (RecNumInParam = 0) and (_FieldCount > 0) then
        begin
          FDataSet.Append;
          FDataSet.Fields[0].AsString := '1';
          FDataSet.Post;
          FDataSet.Delete;
        end;
        if Result <> RecNumInParam then
          Log('Error recnum. Recnum' + IntToStr(Result) + '*' +
            IntToStr(RecNumInParam));

      except
        on E: Exception do
        begin
          ProcessError(E.ClassName + ': ' + E.Message);
          if SQLIsttion <> IstSQLExec then
            FDataSet.Close;
        end;
      end;
    end;
  finally
    FDataSet.EnableControls;
    FDataSet := nil;
  end;
  Log('Open:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
end;

function TClientConnBuffer.ClientRunSQL(SQLVx: Byte): integer;
var
  I, _FieldCount: Integer;
  S, Name: TNetProcString;
  Datatype: TFieldType;
  Size: Integer;
  Required: Boolean;
  RecNumInParam: integer;
  TempStr: TNetProcString;
begin
  Result := 0;
  if (SQLVx > 2) or (SQLVX < 1) then
  begin
    Log('The function is not permitted.');
    Exit;
  end;
  try
    if SQLVx = 2 then
    begin
      Result := ReadInt;
      FDataSet := nil;
    end;
    if SQLVx = 1 then
    begin
      FDataSet.DisableControls;
      try
        FDataSet.Close;
        FDataSet.FieldDefs.Clear;
        _FieldCount := ReadInt;
        for I := 0 to _FieldCount - 1 do
        begin
          S := ReadStr;
          Name := RetrieveStr(S, ';');
          Datatype := TFieldType(StrToInt(RetrieveStr(S, ';')));
          Size := StrToInt(RetrieveStr(S, ';'));
          Required := Boolean(StrToInt(RetrieveStr(S, ';')));
          FDataSet.FieldDefs.Add(Name, DataType, Size, Required);
        end;
        FDataSet.Open;
        while ReadByte = 0 do
        begin
          FDataSet.Append;
          for I := 0 to _FieldCount - 1 do
          begin
            TempStr := ReadStr;
            if FDataSet.Fields[I].DataType in [ftDate, ftTime, ftDateTime] then
              FDataSet.Fields[I].AsDateTime := DotStrToFloat(TempStr)
            else
              if FDataSet.Fields[I].DataType in [ftFloat, ftCurrency] then
                FDataSet.Fields[I].AsFloat := DotStrToFloat(TempStr)
              else
              begin
{$IFDEF FPC}
                FDataSet.Fields[I].AsString := TempStr;
{$ELSE}
                if FDataSet.Fields[I].DataType in [ftString, ftFixedChar]
                  then
{$IFDEF OverRad2k7}
                  FDataSet.Fields[I].AsAnsiString := UTF8Decode(TempStr)
{$ELSE}
                  FDataSet.Fields[I].AsString := UTF8Decode(TempStr)
{$ENDIF}
                else
{$IFDEF OverRad2k7}
                  FDataSet.Fields[I].AsAnsiString := TempStr;
{$ELSE}
                  FDataSet.Fields[I].AsString := TempStr;
{$ENDIF}
{$ENDIF}
              end;
          end;
          FDataSet.Post;
          Inc(Result);
          FDataSet.First;
        end;
        RecNumInParam := ReadInt;
        if Result <> RecNumInParam then
          Log('Error recnum. Recnum - : ' + IntToStr(Result) + '*' +
            IntToStr(RecNumInParam));
      except
        on E: Exception do
        begin
          ProcessError(E.ClassName + ': ' + E.Message);
          if SQLVx = 1 then
            FDataSet.Close;
        end;
      end;
    end;
  finally
    FDataSet.EnableControls;
    FDataSet := nil;
  end;
  Log('Open:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
end;

procedure TClientConnBuffer.ProcessError(const Msg: TNetProcString);
var istErrorMsg: TNetProcString;
begin
  if Instruction = IstError then
  begin
    istErrorMsg := ReadStr + #13 + Msg;
    Log(istErrorMsg);
    if not (csDesigning in ComponentState) then
      raise exception.create(istErrorMsg);
  end
  else
    if not (csDesigning in ComponentState) then
      raise exception.create(Msg);
end;

function TClientConnBuffer.DoSQLScript(DataSet: TDataset; CPInstruc: Byte;
  CliParam: TNetProcString): integer;
var
  ReturnValue: Byte;
begin
  Result := 0;
  FDataSet := DataSet;
  SetInstruction(IstSQLScript);
  WriteByte(CPInstruc);
  try
    WriteStr(UTF8Encode(CliParam));
    WriteByte(1);
    ProcessSendData;
    RecvBuffer := FSocket.ProcessData(SendBuffer);
    ProcessReadData;
    ReturnValue := ReadByte;
    Result := ReadInt;
  finally
    FDataSet := nil;
  end;
end;

function TClientConnBuffer.DoStoredProc(DataSet: TDataset; CPInstruc: Byte;
  CliParam: TNetProcString): integer;
var
  ReturnValue: Byte;
begin
  Result := 0;
  FDataSet := DataSet;
  SetInstruction(IstStoredProc);
  WriteByte(CPInstruc);
  try
    WriteStr(UTF8Encode(CliParam));
    WriteByte(1);
    ProcessSendData;
    RecvBuffer := FSocket.ProcessData(SendBuffer);
    ProcessReadData;
    ReturnValue := ReadByte;
    ReturnValue := ReadByte;
    Result := ReadInt;
  finally
    FDataSet := nil;
  end;
end;

function TClientConnBuffer.DoSpecialSQL(DataSet: TDataset; CPInstruc: Byte;
  CliParam: TNetProcString): integer;
var
  ReturnValue: Byte;
begin
  Result := 0;
  FDataSet := DataSet;
  SetInstruction(IstSpecialSQL);
  WriteByte(CPInstruc);
  try
    WriteStr(UTF8Encode(CliParam));
    WriteByte(1);
    ProcessSendData;
    RecvBuffer := FSocket.ProcessData(SendBuffer);
    ProcessReadData;
    ReturnValue := ReadByte;
    Result := ReadByte;
    ClientRunSQL(ReturnValue);
  finally
  end;
end;

function TClientConnBuffer.DoInternalSpecialSQL(DataSet: TDataset; SubInstruc:
  Byte; CliParam: TNetProcString): integer;
var
  ReturnValue: Byte;
  _FieldCount, I, Size, RecNumInParam: integer;
  S, TempStr: TNetProcString;
  Datatype: TFieldType;
  Required: Boolean;
begin
  Result := 0;
  FDataSet := DataSet;
  SetInstruction(IstInternalCustProc);
  WriteByte(101);
  WriteByte(SubInstruc);
  WriteStr(UTF8Encode(CliParam));
  WriteByte(1);
  ProcessSendData;
  RecvBuffer := FSocket.ProcessData(SendBuffer);
  ProcessReadData;
  ReturnValue := ReadByte;
  Result := ReadByte;
  FDataSet.DisableControls;
  try
    Result := 0;
    FDataSet.Close;
    FDataSet.FieldDefs.Clear;
    _FieldCount := ReadInt;
    for I := 0 to _FieldCount - 1 do
    begin
      S := ReadStr;
      Name := RetrieveStr(S, ';');
      Datatype := TFieldType(StrToInt(RetrieveStr(S, ';')));
      Size := StrToInt(RetrieveStr(S, ';'));
      Required := Boolean(StrToInt(RetrieveStr(S, ';')));
      FDataSet.FieldDefs.Add(Name, DataType, Size, Required);
    end;
    FDataSet.Open;
    while ReadByte = 0 do
    begin
      FDataSet.Append;
      for I := 0 to _FieldCount - 1 do
      begin
        TempStr := ReadStr;
{$IFDEF FPC}
        FDataSet.Fields[I].AsString := TempStr;
{$ELSE}
        if FDataSet.Fields[I].DataType in [ftString, ftFixedChar] then
{$IFDEF OverRad2k7}
          FDataSet.Fields[I].AsAnsiString := UTF8Decode(TempStr)
{$ELSE}
          FDataSet.Fields[I].AsString := UTF8Decode(TempStr)
{$ENDIF}
        else
{$IFDEF OverRad2k7}
          FDataSet.Fields[I].AsAnsiString := TempStr;
{$ELSE}
          FDataSet.Fields[I].AsString := TempStr;
{$ENDIF}
{$ENDIF}
      end;
      FDataSet.Post;
      Inc(Result);
      FDataSet.First;
    end;
    RecNumInParam := ReadInt;
    if Result <> RecNumInParam then
      Log('Error recnum. Recnum - : ' + IntToStr(Result) + '*' +
        IntToStr(RecNumInParam));
  finally
    FDataSet.EnableControls;
    FDataSet := nil;
  end;
  Log('Open:' + FormatDateTime('yyyy-mm-dd hh:mm:ss:zzz', Now));
end;

function TClientConnBuffer.ProcessDynamicCustProc(DataSet: TDataset; CPInstruc:
  Byte;
  CliParam: TNetProcString): TNetProcString;
var
  ReturnValue: Byte;
begin
  Result := '';
  FDataSet := DataSet;

  SetInstruction(IstDynamicCustProc);
  WriteByte(CPInstruc);
  try
    WriteStr(UTF8Encode(CliParam));
    WriteByte(1);
    ProcessSendData;
    RecvBuffer := FSocket.ProcessData(SendBuffer);
    ProcessReadData;
    ReturnValue := ReadByte;
    ReturnValue := ReadByte;
    ReturnValue := ReadInt;
{$IFDEF FPC}
    Result := ReadStr;
    ReturnStr := ReadStr;
{$ELSE}
    Result := UTF8Decode(ReadStr);
    ReturnStr := UTF8Decode(ReadStr);
{$ENDIF}
  finally
    FDataSet := nil;
  end;
end;

function TClientConnBuffer.ProcessInternalCustProc(DataSet: TDataset; CPInstruc:
  Byte;
  CliParam: TNetProcString): TNetProcString;
var
  ReturnValue: Byte;
begin
  Result := '';
  FDataSet := DataSet;

  SetInstruction(IstInternalCustProc);
  WriteByte(CPInstruc);
  try
    WriteStr(UTF8Encode(CliParam));
    WriteByte(1);
    ProcessSendData;
    RecvBuffer := FSocket.ProcessData(SendBuffer);
    ProcessReadData;
    ReturnValue := ReadByte;
{$IFDEF FPC}
    Result := ReadStr;
{$ELSE}
    Result := UTF8Decode(ReadStr);
{$ENDIF}
    log(Result);
  finally
    FDataSet := nil;
  end;
end;

end.

