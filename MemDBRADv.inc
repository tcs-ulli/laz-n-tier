{$IFDEF VER200} { CodeGear Delphi 2009 12.x }
  {$DEFINE OverRad2k7}
{$ENDIF}

{$IFDEF VER210} { CodeGear Delphi 2010 14.x }
  {$DEFINE OverRad2k7}
{$ENDIF}

{$IFDEF VER220} { CodeGear Delphi 2011 15.x }
  {$DEFINE OverRad2k7}
{$ENDIF}

{$IFDEF VER180} 
  {$DEFINE EnableLOG}
{$ENDIF}

{$IFDEF FPC} 
  {$IFDEF MSWINDOWS}
    {$DEFINE EnableLOG}
  {$ENDIF} 
{$ENDIF}