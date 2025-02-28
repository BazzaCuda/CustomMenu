{   CustomMenu
    Copyright (C) 2022-2099 Baz Cuda
    https://github.com/BazzaCuda/CustomMenu

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit runElevatedSupport;
// https://github.com/eversonturossi/delphi/blob/master/Executa%20como%20adm/Runas%20Admin%20Elevated%20UAC%20ShellExecute%20Delphi/RunElevatedSupport.pas
{$WARN SYMBOL_PLATFORM OFF}
{$R+}

interface

uses
  Windows;

type
  TElevatedProc = function(const AParameters: String): Cardinal;
  TProcessMessagesMeth = procedure of object;

var
  // Warning: this function will be executed in external process.
  // Do not use any global variables inside this routine!
  // Use only supplied AParameters.
  OnElevateProc: TElevatedProc;

  // Call this routine after you have assigned OnElevateProc
procedure checkForElevatedTask;

// Runs OnElevateProc under full administrator rights
function runElevated(const AParameters: String; const AWnd: HWND = 0; const AProcessMessages: TProcessMessagesMeth = nil): Cardinal; overload;

function  isAdministrator: Boolean;
function  isAdministratorAccount: Boolean;
function  isUACEnabled: Boolean;
function  isElevated: Boolean;
procedure setButtonElevated(const AButtonHandle: THandle);

implementation

uses
  SysUtils, Registry, ShellAPI, ComObj;

const
  RunElevatedTaskSwitch = '0CC5C50CB7D643B68CB900BF000FFFD5'; // some unique value, just a GUID with removed '[', ']', and '-'

function CheckTokenMembership(TokenHandle: THandle; SidToCheck: Pointer; var IsMember: BOOL): BOOL; stdcall; external advapi32 name 'CheckTokenMembership';

function RunElevated(const AParameters: String; const AWnd: HWND = 0; const AProcessMessages: TProcessMessagesMeth = nil): Cardinal; overload;
var
  SEI: TShellExecuteInfo;
  Host: String;
  Args: String;
begin
  Assert(Assigned(OnElevateProc), 'OnElevateProc must be assigned before calling RunElevated');

  if IsElevated then
  begin
    if Assigned(OnElevateProc) then
      Result := OnElevateProc(AParameters)
    else
      Result := ERROR_PROC_NOT_FOUND;
    Exit;
  end;

  Host := ParamStr(0);
  Args := Format('/%s %s', [RunElevatedTaskSwitch, AParameters]);

  FillChar(SEI, SizeOf(SEI), 0);
  SEI.cbSize := SizeOf(SEI);
  SEI.fMask := SEE_MASK_NOCLOSEPROCESS;
{$IFDEF UNICODE}
  SEI.fMask := SEI.fMask or SEE_MASK_UNICODE;
{$ENDIF}
  SEI.Wnd := AWnd;
  SEI.lpVerb := 'runas';
  SEI.lpFile := PChar(Host);
  SEI.lpParameters := PChar(Args);
  SEI.nShow := SW_NORMAL;

  if not ShellExecuteEx(@SEI) then
    RaiseLastOSError;
  try

    Result := ERROR_GEN_FAILURE;
    if Assigned(AProcessMessages) then
    begin
      repeat
        if not GetExitCodeProcess(SEI.hProcess, Result) then
          Result := ERROR_GEN_FAILURE;
        AProcessMessages;
      until Result <> STILL_ACTIVE;
    end
    else
    begin
      if WaitForSingleObject(SEI.hProcess, INFINITE) <> WAIT_OBJECT_0 then
        if not GetExitCodeProcess(SEI.hProcess, Result) then
          Result := ERROR_GEN_FAILURE;
    end;

  finally
    CloseHandle(SEI.hProcess);
  end;
end;

function IsAdministrator: Boolean;
var
  psidAdmin: Pointer;
  B: BOOL;
const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
begin
  psidAdmin := nil;
  try
    // Создаём SID группы админов для проверки
    Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdmin));

    // Проверяем, входим ли мы в группу админов (с учётов всех проверок на disabled SID)
    if CheckTokenMembership(0, psidAdmin, B) then
      Result := B
    else
      Result := False;
  finally
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;
{$R-}

function IsAdministratorAccount: Boolean;
var
  psidAdmin: Pointer;
  Token: THandle;
  Count: DWORD;
  TokenInfo: PTokenGroups;
  HaveToken: Boolean;
  I: Integer;
const
  SECURITY_NT_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
begin
  Result := Win32Platform <> VER_PLATFORM_WIN32_NT;
  if Result then
    Exit;

  psidAdmin := nil;
  TokenInfo := nil;
  HaveToken := False;
  try
    Token := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, Token);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token);
    if HaveToken then
    begin
      Win32Check(AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdmin));
      if GetTokenInformation(Token, TokenGroups, nil, 0, Count) or (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
        RaiseLastOSError;
      TokenInfo := PTokenGroups(AllocMem(Count));
      Win32Check(GetTokenInformation(Token, TokenGroups, TokenInfo, Count, Count));
      for I := 0 to TokenInfo^.GroupCount - 1 do
      begin
        Result := EqualSid(psidAdmin, TokenInfo^.Groups[I].Sid);
        if Result then
          Break;
      end;
    end;
  finally
    if TokenInfo <> nil then
      FreeMem(TokenInfo);
    if HaveToken then
      CloseHandle(Token);
    if psidAdmin <> nil then
      FreeSid(psidAdmin);
  end;
end;
{$R+}

function IsUACEnabled: Boolean;
var
  Reg: TRegistry;
begin
  Result := CheckWin32Version(6, 0);
  if Result then
  begin
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Policies\System', False) then
        if Reg.ValueExists('EnableLUA') then
          Result := (Reg.ReadInteger('EnableLUA') <> 0)
        else
          Result := False
        else
          Result := False;
    finally
      FreeAndNil(Reg);
    end;
  end;
end;

function IsElevated: Boolean;
const
  TokenElevation = TTokenInformationClass(20);
type
  TOKEN_ELEVATION = record
    TokenIsElevated: DWORD;
  end;
var
  TokenHandle: THandle;
  ResultLength: Cardinal;
  ATokenElevation: TOKEN_ELEVATION;
  HaveToken: Boolean;
begin
  if CheckWin32Version(6, 0) then
  begin
    TokenHandle := 0;
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, TokenHandle);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle);
    if HaveToken then
    begin
      try
        ResultLength := 0;
        if GetTokenInformation(TokenHandle, TokenElevation, @ATokenElevation, SizeOf(ATokenElevation), ResultLength) then
          Result := ATokenElevation.TokenIsElevated <> 0
        else
          Result := False;
      finally
        CloseHandle(TokenHandle);
      end;
    end
    else
      Result := False;
  end
  else
    Result := IsAdministrator;
end;

procedure SetButtonElevated(const AButtonHandle: THandle);
const
  BCM_SETSHIELD = $160C;
var
  Required: BOOL;
begin
  if not CheckWin32Version(6, 0) then
    Exit;
  if IsElevated then
    Exit;

  Required := True;
  SendMessage(AButtonHandle, BCM_SETSHIELD, 0, LPARAM(Required));
end;

procedure CheckForElevatedTask;

  function GetArgsForElevatedTask: String;

    function PrepareParam(const ParamNo: Integer): String;
    begin
      Result := ParamStr(ParamNo);
      if Pos(' ', Result) > 0 then
        Result := AnsiQuotedStr(Result, '"');
    end;

  var
    X: Integer;
  begin
    Result := '';
    for X := 1 to ParamCount do
    begin
      if (AnsiUpperCase(ParamStr(X)) = ('/' + RunElevatedTaskSwitch)) or (AnsiUpperCase(ParamStr(X)) = ('-' + RunElevatedTaskSwitch)) then
        Continue;

      Result := Result + PrepareParam(X) + ' ';
    end;

    Result := Trim(Result);
  end;

var
  ExitCode: Cardinal;
begin
  if not FindCmdLineSwitch(RunElevatedTaskSwitch) then
    Exit;

  ExitCode := ERROR_GEN_FAILURE;
  try
    if not IsElevated then
      ExitCode := ERROR_ACCESS_DENIED
    else
      if Assigned(OnElevateProc) then
        ExitCode := OnElevateProc(GetArgsForElevatedTask)
      else
        ExitCode := ERROR_PROC_NOT_FOUND;
  except
    on E: Exception do
    begin
      if E is EAbort then
        ExitCode := ERROR_CANCELLED
      else
        if E is EOleSysError then
          ExitCode := Cardinal(EOleSysError(E).ErrorCode)
        else
          if E is EOSError then
          else
            ExitCode := ERROR_GEN_FAILURE;
    end;
  end;

  if ExitCode = STILL_ACTIVE then
    ExitCode := ERROR_GEN_FAILURE;
  TerminateProcess(GetCurrentProcess, ExitCode);
end;

end.
