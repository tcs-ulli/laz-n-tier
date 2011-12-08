function SetAnsiDoubleStr(Value: AnsiNetProcString): AnsiNetProcString;
var TempStr: AnsiNetProcString; i, j, k, valuelen: integer;
begin
  valuelen := Length(Value);
  SetLength(TempStr, valuelen);
  j := 0;
  k := 0;
  for i := 1 to valuelen do
    if (Value[i] >= '0') and (Value[i] <= '9') then
    begin
      j := j + 1;
      TempStr[j] := Value[i];
    end
    else
      k := i;
  if k <> 0 then
    TempStr := Copy(TempStr, 1, k - 1) + '.' + Copy(TempStr, k, j);
  SetLength(TempStr, j + 1);
  Result := TempStr;
end;

function FloatToDotStr(Value: Double): AnsiNetProcString;
begin
  SetLength(Result, SizeOf(Double));
  move(Value, Result[1], SizeOf(Double));
end;

function DotStrToFloat(Value: AnsiNetProcString): Double;
begin
  Result := $FFFFFFFF;
  Result := PDouble(@Value[1])^;
end;

function GetBufDotStr(s: AnsiNetProcString; Dot: AnsiChar; xLen: integer): StrArray;
var str: StrArray; i, j: integer;
begin
  i := 1;
  j := 0;
  SetLength(str, xLen + 1);
  while Pos(dot, s) > 0 do
  begin
    str[j] := Copy(s, i, Pos(dot, s) - i);
    i := Pos(dot, s) + 1;
    s[i - 1] := AnsiChar(Ord(dot) + 1);
    j := j + 1;
  end;
  str[j] := copy(s, i, StrLen(PAnsiChar(s)) - i + 1);
  result := str;
end;

procedure PutIntegerToArray(s: PNetProcString; index, num: integer);
begin            
  s^[index] := AnsiChar(num and $FF);
  s^[index + 1] := AnsiChar((num shr 8) and $FF);
  s^[index + 2] := AnsiChar((num shr 16) and $FF);
  s^[index + 3] := AnsiChar((num shr 24) and $FF);
end;

function PutArrayToInteger(s: AnsiNetProcString; index: integer): integer;
var t1, t2, t3, t4: integer;
begin
  t1 := Ord(s[index]);
  t2 := Ord(s[index + 1]);
  t2 := (t2 shl 8) and $FF00;
  t3 := Ord(s[index + 2]);
  t3 := (t3 shl 16) and $FF0000;
  t4 := Ord(s[index + 3]);
  t4 := (t4 shl 24) and $FF000000;
  result := t1 + t2 + t3 + t4;
end;

function RetrieveStr(var txt: AnsiNetProcString; const Separador: AnsiNetProcString): AnsiNetProcString;
var i: integer;
begin
  i := Pos(Separador, txt);
  if i < 1 then begin
    Result := txt;
    txt := '';
  end
  else begin
    Result := Copy(txt, 1, i - 1);
    txt := Copy(txt, i + Length(Separador), length(txt) - i);
  end;
end;


