{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DBNetProcessor; 

interface

uses
  OnLineQuery, SynaSSockets, IniVarLib, ClientProc, MD5, MemDBUtils, 
  ServerProc, SynaCSockets, NetConnection, ZeosDataServer, ZeosProv, 
  OnlineQueryReg, DataProcUtils, SynaSockUtils, MemDataBase, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('OnlineQueryReg', @OnlineQueryReg.Register); 
end; 

initialization
  RegisterPackage('DBNetProcessor', @Register); 
end.
