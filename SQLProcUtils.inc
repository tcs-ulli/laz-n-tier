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

function GetTableNameFromSQL(ASQL: TNetProcString): TNetProcString;
var
  PPOS: integer;
  TempSQL, USQL: TNetProcString;
begin
  USQL := UpperCase(Trim(ASQL));

  if Pos('SELECT', USQL) = 1 then
  begin
    if Pos('JOIN', USQL) > 0 then
    begin
      Result := '';
      Exit;
    end;
    PPOS := Pos('FROM', USQL);
    if PPOS > 0 then
    begin
      TempSQL := Trim(Copy(ASQL, PPOS + 5, Length(ASQL)));
      PPOS := Pos(' WHERE', UpperCase(TempSQL));
      if PPOS > 0 then
        SetLength(TempSQL, PPOS - 1)
      else
      begin
        PPOS := Pos(',', TempSQL);
        if PPOS > 0 then
          SetLength(TempSQL, 0);
      end;
      Result := TempSQL;
    end
    else
      Result := '';
    Exit;
  end;

  if Pos('INSERT', USQL) = 1 then
  begin
    PPOS := Pos('INTO', USQL);
    if PPOS > 0 then
    begin
      TempSQL := Trim(Copy(ASQL, PPOS + 5, Length(ASQL)));
      PPOS := Pos(' ', TempSQL);
      if PPOS > 0 then
        SetLength(TempSQL, PPOS - 1);
      Result := TempSQL;
    end
    else
      Result := '';
    Exit;
  end;

  PPOS := Pos('UPDATE', USQL);
  if PPOS = 1 then
  begin
    TempSQL := Trim(Copy(ASQL, PPOS + 7, Length(ASQL)));
    PPOS := Pos(' SET', UpperCase(TempSQL));
    if PPOS > 0 then
    begin
      SetLength(TempSQL, PPOS - 1);
      Result := Trim(TempSQL);
    end
    else
      Result := '';
    Exit;
  end;

  if Pos('DELETE', USQL) = 1 then
  begin
    PPOS := Pos('FROM', USQL);
    if PPOS > 0 then
    begin
      TempSQL := Trim(Copy(ASQL, PPOS + 5, Length(ASQL)));
      PPOS := Pos(' ', TempSQL);
      if PPOS > 0 then
        SetLength(TempSQL, PPOS - 1);
      Result := TempSQL;
    end
    else
      Result := '';
    Exit;
  end;

end;

function UpdateInsFunc(src0, corgid1, corgid2: TNetProcString; stlod: integer): TNetProcString;
var
  vi, fi, i, srclen: integer;
  s1, s2, t1, t2, src: TNetProcString;
  ts01, ts02: TStringList;
  haveid1, haveid2: boolean;
begin
  ts01 := TStringList.Create;
  ts02 := TStringList.Create;
  Result := '';
  src := UpperCase(trim(src0));
  srclen := length(src);
  vi := Pos('VALUES', src) + 8;
  s2 := Copy(src0, vi, srclen - 1);
  SetLength(s2, srclen - vi);
  fi := Pos('(', src) + 1;
  s1 := Copy(src0, fi, vi - 8 - 1);
  SetLength(s1, vi - 8 - fi - 2);
  t1 := StringReplace(s1, ',', #13, [rfReplaceAll]);
  t2 := StringReplace(s2, ',', #13, [rfReplaceAll]);

  ts01.Text := t1;
  ts02.Text := t2;

  haveid1 := False;
  haveid2 := False;

  for I := 0 to ts01.Count - 1 do
  begin
    if stlod = 3 then
    begin
      if Pos('ORGID1', UpperCase(trim(ts01.Strings[i]))) > 0 then
      begin
        ts02.Strings[i] := '''' + corgid1 + '''';
        haveid1 := True;
      end;
      if Pos('ORGID2', UpperCase(trim(ts01.Strings[i]))) > 0 then
      begin
        ts02.Strings[i] := '''' + corgid2 + '''';
        haveid2 := True;
      end;
    end;

    if stlod = 2 then
    begin
      if Pos('ORGID2', UpperCase(trim(ts01.Strings[i]))) > 0 then
      begin
        ts02.Strings[i] := '''' + corgid2 + '''';
        haveid2 := True;
      end;
    end;

    if stlod = 1 then
    begin
      if Pos('ORGID1', UpperCase(trim(ts01.Strings[i]))) > 0 then
      begin
        ts02.Strings[i] := '''' + corgid1 + '''';
        haveid1 := True;
      end;
    end;
  end;

  if stlod = 3 then
  begin
    if not haveid1 then
    begin
      ts01.Insert(1, 'ORGID1');
      ts02.Insert(1, QuotedStr(corgid1));
    end;
    if not haveid2 then
    begin
      ts01.Insert(1, 'ORGID2');
      ts02.Insert(1, QuotedStr(corgid2));
    end;
  end;

  if stlod = 2 then
  begin
    if not haveid2 then
    begin
      ts01.Insert(1, 'ORGID2');
      ts02.Insert(1, QuotedStr(corgid2));
    end;
  end;

  if stlod = 1 then
  begin
    if not haveid1 then
    begin
      ts01.Insert(1, 'ORGID1');
      ts02.Insert(1, QuotedStr(corgid1));
    end;
  end;

  t1 := '';
  t2 := '';
  for I := 0 to ts01.Count - 1 do
  begin
    if (ts01.Strings[i] <> '') then
    begin
      if i <> 0 then
      begin
        t1 := t1 + ', ' + ts01.Strings[i];
        t2 := t2 + ', ' + ts02.Strings[i];
      end
      else
      begin
        t1 := t1 + ts01.Strings[i];
        t2 := t2 + ts02.Strings[i];
      end;
    end;
  end;

  Result := Copy(src0, 1, fi - 2) + '(' + t1 + ')' + ' values (' + t2 + ')';
  ts01.Free;
  ts02.Free;
end;

function UpdateUPDFunc(src0, corgid1, corgid2: TNetProcString; stlod: integer): TNetProcString;
var
  vi, fi, i, srclen: integer;
  s1, s2, t1, t2, src: TNetProcString;
  ts01, ts02: TStringList;
  TsCount, TmpTS: integer;
begin
  ts01 := TStringList.Create;
  ts02 := TStringList.Create;
  Result := '';
  src := UpperCase(trim(src0));
  srclen := length(src);
  vi := Pos('WHERE', src) + 6;
  s2 := Copy(src0, vi, srclen);
  SetLength(s2, srclen - vi + 1);
  fi := Pos(' SET ', src) + 5;
  s1 := Copy(src0, fi, vi - 7);
  SetLength(s1, vi - 7 - fi);
  t1 := StringReplace(s1, ',', #13, [rfReplaceAll]);
  ts01.Text := t1;
  TsCount := ts01.Count - 1;
  while TsCount >= 0 do
  begin
    TmpTS := 0;
    if stlod = 3 then
    begin
      if Pos('ORGID1', UpperCase(trim(ts01.Strings[TsCount]))) > 0 then
        ts01.Delete(TsCount);
      if Pos('ORGID2', UpperCase(trim(ts01.Strings[TsCount]))) > 0 then
        ts01.Delete(TsCount);
      TmpTS := 1;
    end;

    if stlod = 2 then
    begin
      if Pos('ORGID2', UpperCase(trim(ts01.Strings[TsCount]))) > 0 then
        ts01.Delete(TsCount);
      TmpTS := 1;
    end;

    if stlod = 1 then
    begin
      if Pos('ORGID1', UpperCase(trim(ts01.Strings[TsCount]))) > 0 then
        ts01.Delete(TsCount);
      TmpTS := 1;
    end;

    if (stlod < 1) or (stlod > 3) then
    begin
      TmpTS := 1;
    end;

    if TmpTS > 0 then
      TsCount := TsCount - 1;
  end;

  t1 := '';
  t2 := '';
  if ts01.Count > 0 then
    for I := 0 to ts01.Count - 1 do
    begin
      if (ts01.Strings[i] <> '') then
      begin
        if i <> 0 then
          t1 := t1 + ', ' + ts01.Strings[i]
        else
          t1 := t1 + ts01.Strings[i];
      end;
    end;

  Result := Copy(src0, 1, fi - 1) + t1 + ' where ' + s2;
  ts01.Free;
  ts02.Free;
end;

function GetTableORGIDStyle(NetQuery: TServerSockQuery): integer;
var
  jsq, OrgNum1, OrgNum2, FldCount: integer;
  FldNm: array[0..253] of string;
  FldType: array[0..253] of TFieldType;
begin
  FldCount := NetQuery.NetData.FieldCount;
  OrgNum2 := 0;
  OrgNum1 := 0;
{$IFNDEF FPC}
  for jsq := 0 to FldCount - 1 do
  begin
    FldType[jsq] := NetQuery.NetData.FieldDefList[jsq].DataType;
    FldNm[jsq] := NetQuery.NetData.FieldDefList[jsq].Name;
    if UpperCase(NetQuery.NetData.FieldDefList[jsq].Name) = 'ORGID1' then
      OrgNum1 := 1
    else
      OrgNum1 := 0;
    if UpperCase(NetQuery.NetData.FieldDefList[jsq].Name) = 'ORGID2' then
      OrgNum2 := 2
    else
      OrgNum2 := 0;
  end;
  Result := OrgNum1 + OrgNum2;
{$ENDIF}
{$IFDEF FPC}
  for jsq := 0 to FldCount - 1 do
  begin
    FldType[jsq] := NetQuery.NetData.FieldDefs[jsq].DataType;
    FldNm[jsq] := NetQuery.NetData.FieldDefs[jsq].Name;
    if UpperCase(NetQuery.NetData.FieldDefs[jsq].Name) = 'ORGID1' then
      OrgNum1 := 1
    else
      OrgNum1 := 0;
    if UpperCase(NetQuery.NetData.FieldDefs[jsq].Name) = 'ORGID2' then
      OrgNum2 := 2
    else
      OrgNum2 := 0;
  end;
  Result := OrgNum1 + OrgNum2;
{$ENDIF}
end;

function GetTableORGIDStyle2(NetQuery: TServerSockQuery; TBName: TNetProcString): integer;
var
  TempSQL: TNetProcString;
begin
  TempSQL := NetQuery.SQL.Text;
  try
    NetQuery.Close;
    NetQuery.SQL.Clear;
    NetQuery.SQL.Text := 'select * from ORGIDDB where TBNAME = ' + QuotedStr(TBName);
    NetQuery.SuperOpen;
    if NetQuery.NetData.RecordCount > 0 then
      Result := NetQuery.NetData.FieldByName('ORGSTYLE').AsInteger
    else
      Result := 0;
  finally
    NetQuery.Close;
    NetQuery.SQL.Text := TempSQL;
  end;
end;



