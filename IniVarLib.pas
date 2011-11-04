unit IniVarLib;

interface

uses IniFiles, variants;

//简单数据类型
type

  TIniVarType = (stInt, stFloat, stString, stDateTime, stDate, stTime, stBoolean);

 /// <summary>
 /// 读Ini文件的函数
 /// </summary>
 /// <param name="FileName">Ini文件名</param>
 /// <param name="Section">节点</param>
 /// <param name="Name">字段名</param>
 /// <param name="xDataType">简单数据类型</param>
 /// <param name="DefaultValue">默认值</param>
 /// <returns>返回变体类型</returns>

function ReadIniValue(const FileName, Section, Name: string;
  xDataType: TIniVarType; DefaultValue: variant): variant;
procedure WriteIniValue(const FileName, Section, Name: string;
  Value: variant; xDataType: TIniVarType);

implementation

function ReadIniValue(const FileName, Section, Name: string;
  xDataType: TIniVarType; DefaultValue: variant): variant;
begin
  try
    with TIniFile.Create(FileName) do
    try
      if xDataType = stString then
        Result := ReadString(Section, Name, DefaultValue)
      else if xDataType = stInt then
        Result := ReadInteger(Section, Name, DefaultValue)
      else if xDataType = stFloat then
        Result := ReadFloat(Section, Name, DefaultValue)
      else if xDataType = stDateTime then
        Result := ReadDateTime(Section, Name, DefaultValue)
      else if xDataType = stDate then
        Result := ReadDate(Section, Name, DefaultValue)
      else if xDataType = stTime then
        Result := ReadTime(Section, Name, DefaultValue)
      else if xDataType = stBoolean then
        Result := ReadBool(Section, Name, DefaultValue);
    finally
      Free;
    end;
  except
  end;
end;

 /// <summary>
 /// 写INI文件的函数
 /// </summary>
 /// <param name="FileName">ini文件名</param>
 /// <param name="Section">节点名</param>
 /// <param name="Name">字段名</param>
 /// <param name="Value">字段值</param>
 /// <param name="xDataType">简单类型</param>

procedure WriteIniValue(const FileName, Section, Name: string;
  Value: variant; xDataType: TIniVarType);
begin
  try
    with TIniFile.Create(FileName) do
    try
      if xDataType = stString then
        WriteString(Section, Name, VarToStr(Value))
      else if xDataType = stInt then
        WriteInteger(Section, Name, Value)
      else if xDataType = stFloat then
        WriteFloat(Section, Name, Value)
      else if xDataType = stDateTime then
        WriteDateTime(Section, Name, VarToDateTime(Value))
      else if xDataType = stDate then
        WriteDate(Section, Name, VarToDateTime(Value))
      else if xDataType = stTime then
        WriteTime(Section, Name, VarToDateTime(Value))
      else if xDataType = stBoolean then
        WriteBool(Section, Name, Value);
    finally
      Free;
    end;
  except
  end;
end;

end.

