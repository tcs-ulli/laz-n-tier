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

unit SynaSockUtils;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SyncObjs, SysUtils, {$IFNDEF LINUX}Windows, {$ENDIF}
  Blcksock, Synsock, Synautil, {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem, {$ENDIF}{$ENDIF}//{$IFDEF FPC}LclIntf, {$ENDIF}
  DataProcUtils, SynaIP, SSL_OpenSSL;

type
  TCustomSocket = class(TTCPBlockSocket)
  public
    DisConnected: Boolean;
    procedure Init;
    function SendOnlineData(DataStr: AnsiString): boolean;
    function RecvOnlineData(FTimeOut: integer): AnsiString;
  end;

  JobContent = (doNone, doConn, doDisconn, doSend, doRecv);

  TJobs = record
    Job: JobContent;
    Data: AnsiString;
  end;

implementation

{==============================================================================}

procedure TCustomSocket.Init;
begin
  DisConnected := False;
end;

function TCustomSocket.SendOnlineData(DataStr: AnsiString): boolean;
var
  i, DLen, BLen, XLen: integer;
  TempData: AnsiString;
begin
  DLen := Length(DataStr);

  Result := False;

  if DLen > 8102 then
  begin
    XLen := DLen mod 8102;
    BLen := DLen div 8102;

    for i := 0 to BLen - 1 do
    begin
      if DisConnected then
      begin
        Exit;
      end;

      SetLength(TempData, 8102);
      move(DataStr[i * 8102 + 1], TempData[1], 8102);
      SendString(TempData);
      if LastError <> 0 then
        Exit;
    end;

    if XLen > 0 then
    begin
      SetLength(TempData, XLen);
      move(DataStr[BLen * 8102 + 1], TempData[1], XLen);
      SendString(TempData);
    end;
  end
  else
    SendString(DataStr);

  Result := LastError = 0;
end;

//It can work in hard enviroment and can receive the other message sent.
function TCustomSocket.RecvOnlineData(FTimeOut: integer): AnsiString;
var
  t, s: AnsiString;
  RPTTimeOut, FBias, i: integer;
  ti: longword;
begin
  try
    s := RecvPacket(FTimeOut);
    RPTTimeOut := 36;
    i := 0;
    repeat
      ti := GetTick;
      t := RecvPacket(RPTTimeOut);
      if RPTTimeOut < FTimeOut then
        if LastError = WSAETIMEDOUT then
        begin
          i := i + 1;
          FBias := 96;
          if i = 2 then
            FBias := 160;
          if i = 3 then
            FBias := 1600;
          if i = 4 then
          begin
            FBias := 3600;
            i := 0;
          end;
          RPTTimeOut := FBias + integer(TickDelta(ti, GetTick));
          ResetLastError;
        end;
      s := s + t;
    until (t = '') or DisConnected;
    Result := s;
  except
    Result := '';
  end;
end;             

end.

