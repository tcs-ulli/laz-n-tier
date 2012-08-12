const
  OnlineDataSignature: AnsiString = 'AMTF';
  SuperUsr = 'User37';
  SuperPsw = '546h87hi27';
  InstructionNames: array[low(TInstruction)..High(TInstruction)] of TInstructionName = (
    (Instruction: istNone; Name: 'None'),
    (Instruction: istError; Name: 'Error'),
    (Instruction: istLogin; Name: 'Login'),
    (Instruction: istTime; Name: 'Time'),
    (Instruction: istSQL; Name: 'SQL'),
    (Instruction: istLoadFile; Name: 'LoadFile'),
    (Instruction: istNoIst; Name: 'NoIst'),
    (Instruction: istSQLCacheExec; Name: 'SQLCacheExec'),
    (Instruction: istChangePSW; Name: 'ChangePSW'),
    (Instruction: istSpecialSQL; Name: 'SPSQL'),
    (Instruction: istSQLScript; Name: 'SCRIP'),
    (Instruction: istStoredProc; Name: 'Stored'),
    (Instruction: istDynamicCustProc; Name: 'ClDLL'),
    (Instruction: istInternalCustProc; Name: 'INNER'),
    (Instruction: istInternalOpen; Name: 'INOPEN'),
    (Instruction: InternalSQLInstruciton; Name: 'INSQLExec'),
    (Instruction: istSysName; Name: 'SysName'),
    (Instruction: istUpgradeInfo; Name: 'UpgradeInfo'),
    (Instruction: istUpgradeFile; Name: 'UpgradeFile')
    );
type
  TLogonStyle = (UserNameError, PassWordError, LogedOnServer, PermDenied,
    PermModeII, PermModeIII);

{$DEFINE EVENTLOGON}



