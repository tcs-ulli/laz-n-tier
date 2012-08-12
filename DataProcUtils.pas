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

unit DataProcUtils;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses

{$IFDEF FPC}
  //LCLIntf, LCLType, LMessages,
{$ELSE}
  Windows, Forms,
{$ENDIF}
  Classes, SysUtils;

type
  TInstruction = (istNone, istError, istLogin, istTime, istSQL, istLoadFile, istNoist,
    istSQLCacheExec, istChangePSW, istSpecialSQL, istSQLScript, istStoredProc,
    istDynamicCustProc, istInternalCustProc, istInternalOpen, InternalSQLInstruciton,
    istSysName, istUpgradeInfo, istUpgradeFile);
  TSQLInstruction = (IstSQLExec, IstSQLOpen, IstSQLFieldDefs, IstSQLWithFields);
  AnsiNetProcString = AnsiString;
  PNetProcString = ^AnsiNetProcString;
  TInstructionName = record
    Instruction: TInstruction;
    Name: AnsiNetProcString;
  end;
  
  StrArray = array of AnsiNetProcString;
  AnsiInt = LongWord;
  PAnsiInt = ^AnsiInt;
  PDouble = ^Double;

  TOnlineDataBuffer = class(TComponent)
  protected
    RecvPos: AnsiInt;
    TotalSize: AnsiInt;
    procedure ProcessData; virtual; abstract;
  public
    RecvBuffer: AnsiNetProcString;
    SendBuffer: AnsiNetProcString;
    LoginTime: TDateTime;
    LoginName: ShortString;
    Instruction: TInstruction;
    HeaderSize: AnsiInt;
    constructor Create(aOwner: TComponent); override;
    procedure SetInstruction(Instruc: TInstruction; const Value: AnsiNetProcString = '');
    function GetInstruction(Value: AnsiNetProcString): TInstruction;
    function ReadByte: Byte;
    function ReadInt: AnsiInt;
    function ReadStr: AnsiNetProcString;
    procedure WriteByte(Value: Byte);
    procedure WriteInt(Value: AnsiInt);
    procedure WriteStr(const Value: AnsiNetProcString);
    procedure ProcessReadData;
    procedure ProcessSendData;
  end;

{$I Instructions.inc}

function RetrieveStr(var txt: AnsiNetProcString; const Separador: AnsiNetProcString): AnsiNetProcString;
function GetBufDotStr(s: AnsiNetProcString; Dot: AnsiChar; xLen: integer): StrArray;
procedure PutIntegerToArray(s: PNetProcString; index, num: integer);
function PutArrayToInteger(s: AnsiNetProcString; index: integer): integer;
function FloatToDotStr(Value: Double): AnsiNetProcString;
function DotStrToFloat(Value: AnsiNetProcString): Double;
function SetAnsiDoubleStr(Value: AnsiNetProcString): AnsiNetProcString;

implementation

{$I DataProcUtils.inc}

constructor TOnlineDataBuffer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Instruction := IstNone;
  TotalSize := 0;
  LoginTime := Now;
end;

procedure TOnlineDataBuffer.SetInstruction(Instruc: TInstruction; const Value: AnsiNetProcString = '');
var SignatureLen: integer;
begin
  Instruction := Instruc;
  RecvPos := 1;
  SignatureLen := Length(OnlineDataSignature);
  SetLength(SendBuffer, 1 + SizeOf(AnsiInt) + SignatureLen);
  SendBuffer[1] := AnsiChar(Ord(Instruction));
  move(OnlineDataSignature[1], SendBuffer[2 + SizeOf(AnsiInt)], SignatureLen);
  HeaderSize := 1 + Length(OnlineDataSignature) + SizeOf(TotalSize);
  if Value <> '' then WriteStr(Value);
end;

function TOnlineDataBuffer.GetInstruction(Value: AnsiNetProcString): TInstruction;
begin
  if Length(Value) > 0 then
    Result := TInstruction(Ord(Value[1]))
  else
    Result := IstNoIst;
end;

function TOnlineDataBuffer.ReadByte: Byte;
begin
  Result := 255;
  if RecvPos <= Length(RecvBuffer) then begin
    Result := Byte(RecvBuffer[RecvPos]);
    Inc(RecvPos);
  end;
end;

function TOnlineDataBuffer.ReadInt: AnsiInt;
begin
  Result := $FFFFFFFF;
  if RecvPos <= Length(RecvBuffer) - SizeOf(Result) then begin
    Result := PAnsiInt(@RecvBuffer[RecvPos])^;
    Inc(RecvPos, SizeOf(Result));
  end;
end;

function TOnlineDataBuffer.ReadStr: AnsiNetProcString;
var
  Size: AnsiInt;
begin
  if RecvPos > Length(RecvBuffer) then
    Result := ''
  else begin
    Size := PAnsiInt(@RecvBuffer[RecvPos])^;
    Inc(RecvPos, SizeOf(Size));
    SetLength(Result, Size);
    if Size > 0 then
      move(RecvBuffer[RecvPos], Result[1], Size);
    Inc(RecvPos, Size);
  end;
end;

procedure TOnlineDataBuffer.WriteByte(Value: Byte);
begin
  SendBuffer := SendBuffer + Char(Value);
end;

procedure TOnlineDataBuffer.WriteInt(Value: AnsiInt);
var
  BufferLen: AnsiInt;
begin
  BufferLen := Length(SendBuffer);
  SetLength(SendBuffer, BufferLen + SizeOf(AnsiInt));
  move(Value, SendBuffer[BufferLen + 1], SizeOf(AnsiInt));
end;

procedure TOnlineDataBuffer.WriteStr(const Value: AnsiNetProcString);
var
  BufferLen, DataLen: AnsiInt;
begin
  BufferLen := Length(SendBuffer);
  DataLen := Length(Value);
  SetLength(SendBuffer, BufferLen + SizeOf(AnsiInt) + DataLen);
  move(DataLen, SendBuffer[BufferLen + 1], SizeOf(AnsiInt));
  if DataLen > 0 then
    move(Value[1], SendBuffer[BufferLen + SizeOf(AnsiInt) + 1], DataLen);
end;

procedure TOnlineDataBuffer.ProcessReadData;
var RecvSize: AnsiInt;
begin {$B-}
  RecvSize := Length(RecvBuffer);
  if ((RecvSize >= (1 + SizeOf(TotalSize) + Length(OnlineDataSignature)))
    and CompareMem(@RecvBuffer[2 + SizeOf(AnsiInt)], @OnlineDataSignature[1], Length(OnlineDataSignature))) then
  begin
    SetInstruction(TInstruction(Ord(RecvBuffer[1])));
    TotalSize := PAnsiInt(@RecvBuffer[2])^;
    RecvPos := HeaderSize + 1;
    ProcessData;
  end;
end;

procedure TOnlineDataBuffer.ProcessSendData;
var
  Size, StrSize: AnsiInt;
begin
  Size := Length(SendBuffer);
  StrSize := Size - SizeOf(TotalSize) - 1;
  move(StrSize, SendBuffer[2], SizeOf(AnsiInt));
end;

end.

