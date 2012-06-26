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

unit OnlineQueryReg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SynaSSockets, SynaCSockets, MemDataBase, NetConnection, OnLineQuery,
  ServerProc, ZeosDataServer, ZeosProv, NtConnection; //, UIBProv, UIBDataServer;   //UniDaProv, UniDataServer,

procedure Register;
  
implementation

//{$R *.dcr}
{$R OnLineData.RES}

procedure Register;
begin
  RegisterComponents('DBNet Base', [TSSocketServer]);
  RegisterComponents('DBNet Base', [TCSocket]);
  RegisterComponents('DBNet Advanced', [TOnlineQuery]);
  {$IFDEF FPC}
  RegisterComponents('DBNet Advanced', [TClientDataset]);
  {$ENDIF}
  RegisterComponents('DBNet Advanced', [TOnlineConnection]);
  RegisterComponents('DBNet Advanced', [TLocalConnection]);
  RegisterComponents('DBNet Base', [TMemDB]);
  RegisterComponents('DBNet Advanced', [TZeosDataServer]);
//  RegisterComponents('DBNet Advanced', [TUniDataServer]);
//  RegisterComponents('DBNet Advanced', [TUIBDataServer]);
  RegisterComponents('DBNet Base', [TZeosSQLQuery]);
  RegisterComponents('DBNet Base', [TZeosScriptQuery]);
  RegisterComponents('DBNet Base', [TZeosStoredProcQuery]);
//  RegisterComponents('DBNet Base', [TUniDaSQLQuery]);
//  RegisterComponents('DBNet Base', [TUniDaScriptQuery]);
//  RegisterComponents('DBNet Base', [TUniDaStoredProcQuery]);
{  RegisterComponents('DBNet Base', [TUIBSQLQuery]);
  RegisterComponents('DBNet Base', [TUIBScriptQuery]);
  RegisterComponents('DBNet Base', [TUIBStoredProcQuery]);     }
//  RegisterComponents('DBNet Base', [TOnlineDataSource]);
//  RegisterComponents('DBNet Base', [TZeosConnection]);
end;

end.
