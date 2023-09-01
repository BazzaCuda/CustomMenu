{   CustomMenu
    Copyright (C) 2022 Baz Cuda
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
unit CustomMenuCommon;

interface

uses
  System.Generics.Collections, SysUtils, VCL.Controls, VCL.Graphics, WinAPI.Windows, WinAPI.Messages, WinAPI.CommCtrl, ShellAPI, System.Types, VCL.Menus,
  System.Classes;

type
  TItemData = record
    idName:           string;   // text displayed to the user in the menu
    idSubMenu:        boolean;  // is this a submenu header
    idSubMenuName:    string;   // only used in config
    idSeparatorAfter: boolean;
    idHasLUAShield:   boolean;
    idIconFile:       string;   // can be a .ico, .exe, .dll, etc.
    idIconIx:         integer;
    idCommand:        string;   // the command to be executed, e.g. an executable, or a special command like 'FolderContents'
    idParams:         string;
    idDirectory:      string;
    idRunType:        string;   // normal, minimized, maximized
    idRunAsAdmin:     boolean;
    idHint:           string;
    idDisabled:       boolean;  // allow for commented lines with : to be preserved
    idItemID:         integer;  // for when this item owns a submenu (unique to this item in this session only)
    idImageIx:        integer;  // used during config sessions
    idPrevImageIx:    integer;  // used during config sessions - holds original ImageIx if user checks LUAShield option
    idBrowseBlocked:  boolean;
  end;
  PItemData = ^TItemData;

const
  CM_APP_NAME         = 'Custom Menu';
  CM_APP_VERSION      = 'v1.0.1';
  CM_BACKGROUND_COLOR = $2B2B2B;
  CM_HIGHLIGHT_COLOR  = $484848;
  CM_ITEM_HEIGHT      = 22;
  CM_INI_FILE_NAME    = 'CustomMenu.ini';
  CM_NEW_ITEM_NAME    = 'New Menu Item';
  CM_REGISTRY_KEY     = 'SOFTWARE\Classes\DesktopBackground\shell';         // don't add the trailing \
  CM_CTRL_FILE_NAME   = 'ctrl-click';

  IL2_CHEVRON   = 0;
  IL2_LUASHIELD = 1;
  IL2_UNIVERSAL = 2;
  IL2_CHECKED   = 3;

  WM_XY_MESSAGE = WM_APP + 0;

function alreadyRunning: boolean;
function ctrlClickToActivate: boolean;
function copyIcons(src: TImageList; dst: TImageList): boolean;
function createMiniIni: boolean;
function customMenuWnd: HWND;
function delay(dwMilliseconds:DWORD): boolean;
function doShellExecute(id: TItemData): boolean;
function enableHook(active: boolean = TRUE): boolean;
function enableTrayExit(enable: boolean = TRUE): boolean;
function expandEnvs(const str: string): string;
function expandPathRelToBaseDir(const filePath: string; baseDir: string): string;
function findCustomMenu: boolean;
function getExePath: string;
function getFileNameWithoutExt(filename: string): string;
function getFileVersion(const aFilePath: string = ''; const fmt: string = '%d.%d.%d.%d'): string;
function getINIFileName: string;
function getParamXY: TPoint;
function getScreenHeight: integer;
function getScreenWidth: integer;
function iconFileFromIconEntry(iconEntry: string): string;
function itemDataClear(var itemData: TList<TItemData>): boolean;
function iconIxFromIconEntry(iconEntry: string): integer;
function isRunningAsAdmin: boolean;
function isShiftKeyDown: boolean;
function isUserAdmin: Boolean;
function loadIcon(iconFile: string; iconIx: integer; imageList: TImageList; size: integer = 16): boolean;
function loadIcons(id: TList<TItemData>; imageList: TImageList; defaultIcon: TIcon; size: integer = 16): boolean;
function hasParamConfig: boolean;
function hasParamShowMenu: boolean;
function hasParamSingle: boolean;

var GTrayExit: TMenuItem; // not a great way to do it, but it'll do for now at a pinch

implementation

uses _debugWindow, VCL.Forms, TLHelp32, FormCustomMenu, ShLwApi;

function delay(dwMilliseconds:DWORD): boolean;
var
  iStart, iStop: DWORD;
begin
  iStart := GetTickCount;
  repeat
    iStop  := GetTickCount;
    Application.ProcessMessages;
  until (iStop  -  iStart) >= dwMilliseconds;
end;

function processExists(exeFileName: string): cardinal;
var
  vLoop:           BOOL;
  vSnapshotHandle: THandle;
  vProcessEntry32: TProcessEntry32;
  vPID: cardinal;
begin
  result := 0;
  vSnapshotHandle := createToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  vProcessEntry32.dwSize := sizeOf(vProcessEntry32);
  vLoop := process32First(vSnapshotHandle, vProcessEntry32);
  while integer(vLoop) <> 0 do begin
    case ((lowerCase(extractFileName(vProcessEntry32.szExeFile))) = lowerCase(ExeFileName)) or (lowerCase(vProcessEntry32.szExeFile) = lowerCase(ExeFileName)) of TRUE: result := vProcessEntry32.th32ProcessID; end;
    case (result > 0) and (result <> GetCurrentProcessId) of TRUE: BREAK;
                                                             FALSE: result := 0; end;
    vLoop := Process32Next(vSnapshotHandle, vProcessEntry32);
  end;
  closeHandle(vSnapshotHandle);
end;

function alreadyRunning: boolean;
begin
  result := processExists('CustomMenu.exe') > 0;
end;

function ctrlClickToActivate: boolean;
begin
  result := fileExists(getExePath + CM_CTRL_FILE_NAME);
end;

function createMiniIni: boolean;
begin
  case fileExists(getIniFileName) of TRUE: EXIT; end;
  var ini := TStringList.create;
  try
    ini.add('name=Right-Click me to open Config;icon=' + getExePath + 'CustomMenu.exe,0;hint=You can right-click any menu to open Config;');
    ini.saveToFile(getIniFileName);
//    sleep(1000); // make sure it gets saved to disk before buildAndShowTheMenu tries to read it.
  finally
    ini.free;
  end;
end;

var foundWND: HWND;
function EnumWindowsProc(WND: HWND; LPARAM: nativeInt): BOOL; stdcall;
var
  WndText   : Array[0..255] of Char;
  ClassName : Array[0..255] of Char;
begin
  result := TRUE;
  getClassName(WND, ClassName, SizeOf(ClassName));
  getWindowText(WND, wndText, SizeOf(wndText));
  case (className = 'TCustomMenu') and (wndText = 'CustomMenu') of TRUE: foundWND := WND; end;
end;

function customMenuWnd: HWND;
begin
  EnumWindows(@EnumWindowsProc, 0);
  result := foundWND;
end;

function copyIcons(src: TImageList; dst: TImageList): boolean;
begin
  for var i: integer := 0 to src.count -1 do  begin
                                                var vIcon := TIcon.create;
                                                try
                                                  src.getIcon(i, vIcon);
                                                  dst.addIcon(vIcon);
                                                finally
                                                  vIcon.Free;
                                                end;end;
end;

function doShellExecute(id: TItemData): boolean;
//  SW_NORMAL = 1; SW_SHOWMINIMIZED = 2; SW_SHOWMAXIMIZED = 3;
// CoInitializeEx(NULL, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE)
// https://learn.microsoft.com/en-gb/windows/win32/api/shellapi/nf-shellapi-shellexecutea?redirectedfrom=MSDN
begin
  var vShowCmd := pos(id.idRunType, '        Normal   MinimizedMaximized') div 9;
  case vShowCmd = 0 of TRUE: vShowCmd := 1; end;

  case id.idCommand = 'browse' of TRUE: begin id.idCommand := 'explorer.exe';
                                              id.idParams  := id.idDirectory; end;end;

  // shellExecute will handle a relative path in the command but only if the directory is blank...
  case trim(id.idDirectory) = '' of  TRUE:  {execute asis};
                                    FALSE:  begin
                                              case (length(id.idDirectory) > 1) and (id.idDirectory[2] = ':') of // can't be a relative path
                                                 TRUE:  {execute asis};
                                                FALSE:  begin
                                                          id.idDirectory := expandPathRelToBaseDir(id.idDirectory, getExePath);
                                                          { and if we convert the non-blank directory from relative to absolute,
                                                            we have to do the same with command; shellExecute can't handle a mixture }
                                                          id.idCommand := expandPathRelToBaseDir(id.idCommand, getExePath); end;end;end;end;

//  debug('idDirectory: ' + id.idDirectory);
//  debug('idCommand:   ' + id.idCommand);
//  debug('');

  case id.idRunAsAdmin of
   TRUE: shellExecute(getDesktopWindow, 'runas', pchar(id.idCommand), pchar(id.idParams), pchar(id.idDirectory), vShowCmd);
  FALSE: shellExecute(getDesktopWindow, 'open', pchar(id.idCommand), pchar(id.idParams), pchar(id.idDirectory), vShowCmd); end;
end;

function enableHook(active: boolean = TRUE): boolean;
begin
  case mainMenu.hook = NIL of TRUE: EXIT; end;
  mainMenu.hook.active := active;
end;

function enableTrayExit(enable: boolean = TRUE): boolean;
begin
  GTrayExit.enabled := enable;
end;

function expandEnvs(const str: string): string;
var
  bufSize: integer; // size of expanded string
begin
  bufSize := ExpandEnvironmentStrings(PChar(str), nil, 0);
  case bufSize > 0 of TRUE: begin
                              setLength(result, bufSize);
                              ExpandEnvironmentStrings(PChar(str), PChar(result), bufSize); end;end;
end;

function expandPathRelToBaseDir(const filePath: string; baseDir: string): string;
// don't convert to CASE statements.
// This is one of those occasions where BOOL doesn't match CASE's TRUE.
var
  buffer: array [0..MAX_PATH - 1] of Char;
begin
  if pathIsRelative(PChar(filePath)) then
    result := includeTrailingBackslash(baseDir) + filePath
  else
    result := filePath;

  if pathCanonicalize(@Buffer[0], PChar(result)) then begin
    result := buffer;
  end;
end;

function findCustomMenu: boolean;
// Tell the existing mainMenu where to display itself, assuming the command line contains positioning params.
// Otherwise just show it.
// This function is called from the .dpr project file before this instance of CustomMenu.exe exits.
begin
  var WND := customMenuWnd;
  case WND = 0 of TRUE: EXIT; end;
  var vPt := getParamXY;
  case (vPt.X <> 0) and (vPt.Y <> 0) of  TRUE:  sendMessage(WND, WM_XY_MESSAGE, NativeUInt(vPt.X), NativeUInt(vPt.Y));
                                        FALSE:  begin
                                                  showWindow(WND, SW_SHOW);
                                                  bringWindowToTop(WND);
                                                  setForegroundWindow(WND); end;end;
end;

function getExePath: string;
begin
  result := includeTrailingBackslash(extractFilePath(paramStr(0)));
end;

function getFileNameWithoutExt(filename: string): string;
begin
  var vExt  := extractFileExt(filename);
  result    := copy(extractFileName(filename), 1, length(extractFileName(filename)) - length(vExt));
end;

function getFileVersion(const aFilePath: string = ''; const fmt: string = '%d.%d.%d.%d'): string;
var
  vFilePath: string;
  iBufferSize: DWORD;
  iDummy: DWORD;
  pBuffer: Pointer;
  pFileInfo: Pointer;
  iVer: array[1..4] of Word;
begin
  // set default value
  Result := '';
  // get filename of exe/dll if no filename is specified
  vFilePath := aFilePath;
  case vFilePath = '' of TRUE:  begin
                                  // prepare buffer for path and terminating #0
                                  SetLength(vFilePath, MAX_PATH + 1);
                                  SetLength(vFilePath, GetModuleFileName(hInstance, PChar(vFilePath), MAX_PATH + 1));
                                end;end;

  // get size of version info (0 if no version info exists)
  iBufferSize := GetFileVersionInfoSize(PChar(vFilePath), iDummy);

  case iBufferSize > 0 of TRUE:   begin
                                    GetMem(pBuffer, iBufferSize);
                                    try
                                      // get fixed file info (language independent)
                                      GetFileVersionInfo(PChar(vFilePath), 0, iBufferSize, pBuffer);
                                      VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
                                      // read version blocks
                                      iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
                                      iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
                                      iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
                                      iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
                                    finally
                                      FreeMem(pBuffer);
                                    end;
                                    // format result string
                                    Result := Format(Fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
                                  end;end;
end;

function getINIFileName: string;
begin
  result := getExePath + CM_INI_FILE_NAME; {EXIT;} // default
  for var i := 1 to paramCount do
    case (fileExists(paramStr(i))) or (fileExists(getExePath + paramStr(i))) of TRUE: result := paramStr(i); end; // a fully-qualified path or a file in the exe folder
end;

function getScreenHeight: integer;
begin
  var rect := screen.WorkAreaRect; // the screen minus the taskbar
  result := rect.Bottom - rect.Top;
end;

function getScreenWidth: integer;
begin
  result := GetSystemMetrics(SM_CXVIRTUALSCREEN); // we'll assume that the taskbar is in it's usual place at the bottom of the screen
end;

function getParamXY: TPoint;
begin
  result := point(0, 0);
  for var i := 1 to paramCount do begin
    case lowerCase(paramStr(i)) = 'center' of TRUE: begin result.X := getScreenWidth div 2; result.Y := getScreenHeight div 2;  end;end;
    case (length(paramStr(i)) > 2) and (pos('x:', lowerCase(paramStr(i))) = 1) of TRUE: result.X := strToIntDef(copy(paramStr(i), 3, 255), 0); end; // "center" can be overridden by either
    case (length(paramStr(i)) > 2) and (pos('y:', lowerCase(paramStr(i))) = 1) of TRUE: result.Y := strToIntDef(copy(paramStr(i), 3, 255), 0); end; // or both of these.
  end;
end;

function hasParamConfig: boolean;
begin
  result := FALSE;
  for var i := 1 to paramCount do
    case lowerCase(paramStr(i)) = 'config' of TRUE: result := TRUE; end;
end;

function hasParamShowMenu: boolean;
begin
  result := FALSE;
  for var i := 1 to paramCount do
    case lowerCase(paramStr(i)) = 'showmenu' of TRUE: result := TRUE; end;
end;

function hasParamSingle: boolean;
// There can be only one
begin
  result := FALSE;
  for var i := 1 to paramCount do
    case lowerCase(paramStr(i)) = 'single' of TRUE: result := TRUE; end;
end;

function iconFileFromIconEntry(iconEntry: string): string;
// e.g. for "shell32.dll,10", will return "shell32.dll"
begin
  result := iconEntry;                                                  // assume it might just be an icon (.ico) filename, for example
  case trim(iconEntry) = '' of TRUE: EXIT; end;
  case iconEntry[length(iconEntry)] in ['0'..'9'] of FALSE: EXIT; end;  // doesn't end in at least one digit
  case LastDelimiter(',', iconEntry) = 0 of TRUE: EXIT; end;            // doesn't contain a comma [followed by digits]

  result := copy(iconEntry, 1, LastDelimiter(',', iconEntry) - 1);      // copy up to but not including the comma
  case result[1] = '"' of TRUE: delete(result, 1, 1); end;              // remove leading and trailing double quote marks
  case result[length(result)] = '"' of TRUE: delete(result, length(result), 1); end;
end;

function iconIxFromIconEntry(iconEntry: string): integer;
// e.g. for "shell32.dll,10", will return "10"
begin
  result := 0;                                                                    // assume there's no icon index entry
  case trim(iconEntry) = '' of TRUE: EXIT; end;
  case length(iconEntry) > 2 of FALSE: EXIT; end;                                 // must at least end in ,0
  case iconEntry[length(iconEntry)] in ['0'..'9'] of FALSE: EXIT; end;            // doesn't end in at least one digit
  case LastDelimiter(',', iconEntry) = 0 of TRUE: EXIT; end;                      // doesn't contain a comma [followed by digits]

  TryStrToInt(copy(iconEntry, LastDelimiter(',', iconEntry) + 1, 255), result);   // try to convert everything after the last comma
end;

function itemDataClear(var itemData: TList<TItemData>): boolean;
begin
  case itemData = NIL of TRUE: EXIT; end;
  for var i: integer := 0 to itemData.count - 1 do
    itemData[i] := default(TItemData);
end;
//========== ICONS ==========
function PrivateExtractIcons(lpszFile: PChar; nIconIndex, cxIcon, cyIcon: integer; phicon: PHANDLE; piconid: PDWORD; nicon, flags: DWORD): DWORD; stdcall ; external 'user32.dll' name 'PrivateExtractIconsW';

function privateExtractIcon(iconFile: string; iconIx: integer; imageList: TImageList; size: integer = 16): boolean;
var
  hIcon:    THandle;
  nIconId:  DWORD;
begin
  result := FALSE;
  var res: DWORD := PrivateExtractIcons(pchar(iconFile), iconIx, size, size, @hIcon, @nIconId, 1, LR_LOADFROMFILE);
  case hIcon = 0 of TRUE: EXIT; end;
  case res > 0 of TRUE: // changed from <>
  try
    var icon: TIcon := TIcon.Create;
    try
      icon.Handle := hIcon;
      imageList.addIcon(icon);
    finally
      icon.free;
    end;
  finally
    destroyIcon(hIcon);
  end;end;
  result := res > 0; // changed from <>
end;

function loadIcon(iconFile: string; iconIx: integer; imageList: TImageList; size: integer = 16): boolean;
begin
  result := FALSE;
  case trim(iconFile) = '' of TRUE: EXIT; end;
  iconFile := expandEnvs(iconFile);

  case lowerCase(extractFileExt(iconFile)) = '.exe' of TRUE: begin result := privateExtractIcon(iconFile, iconIx, imageList, size); EXIT; end;end;
  case lowerCase(extractFileExt(iconFile)) = '.dll' of TRUE: begin result := privateExtractIcon(iconFile, iconIx, imageList, size); EXIT; end;end;

  case lowerCase(extractFileExt(iconFile)) = '.ico' of TRUE:  begin
                                                                case fileExists(iconFile) of FALSE: EXIT; end; // privateExtractIcon will find files like "regedit.exe" without a full path
                                                                var vIcon := TIcon.create;
                                                                try
                                                                  vIcon.loadFromFile(iconFile);
                                                                  imageList.addIcon(vIcon);
                                                                  result := TRUE;
                                                                  EXIT;
                                                                finally
                                                                  vIcon.Free;
                                                                end;
                                                              end;end;

  // -1 index adds the icon to the end of the image list.
  result := imageList_replaceIcon(imageList.Handle, -1, extractIcon(0, pchar(iconFile), iconIx)) <> -1; // don't use the imageList_addIcon macro. It doesn't return a result.
end;

function loadIcons(id: TList<TItemData>; imageList: TImageList; defaultIcon: TIcon; size: integer = 16): boolean;
// load the specified icon for each node and add it to the imageList or add the supplied defaultIcon instead
// for idHasLUAShield, any icon assigned here will be overridden in InitNode
begin
  for var i: integer := 0 to id.Count - 1 do
    case loadIcon(id[i].idIconFile, id[i].idIconIx, imageList, size) of FALSE: imageList.AddIcon(defaultIcon); end;
end;
//========== ICONS ==========

function CheckTokenMembership(TokenHandle: THandle; SIdToCheck: PSID; var IsMember: BOOL): BOOL; StdCall; External AdvApi32;
const
  SECURITY_NT_AUTHORITY: SID_IDENTIFIER_AUTHORITY = (Value: (0,0,0,0,0,5)); // ntifs
  SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
  DOMAIN_ALIAS_RID_ADMINS: DWORD = $00000220;

function isUserAdmin: Boolean;
var
  b: BOOL;
  AdministratorsGroup: PSID;
begin
  {
    This function returns true if you are currently running with admin privileges.
    In Vista and later, if you are non-elevated, this function will return false
    (you are not running with administrative privileges).
    If you *are* running elevated, then IsUserAdmin will return true, as you are
    running with admin privileges.

    Windows provides this similar function in Shell32.IsUserAnAdmin.
    But the function is deprecated, and this code is lifted
    from the docs for CheckTokenMembership:
      http://msdn.microsoft.com/en-us/library/aa376389.aspx
  }

  {
    Routine Description: This routine returns TRUE if the callers
    process is a member of the Administrators local group. Caller is NOT
    expected to be impersonating anyone and is expected to be able to
    open its own process and process token.
      Arguments: None.
      Return Value:
        TRUE - Caller has Administrators local group.
        FALSE - Caller does not have Administrators local group.
  }
  b := AllocateAndInitializeSid(
      SECURITY_NT_AUTHORITY,
      2, //2 sub-authorities
      SECURITY_BUILTIN_DOMAIN_RID,  //sub-authority 0
      DOMAIN_ALIAS_RID_ADMINS,      //sub-authority 1
      0, 0, 0, 0, 0, 0,             //sub-authorities 2-7 not passed
      AdministratorsGroup);
  if (b) then
  begin
    if not CheckTokenMembership(0, AdministratorsGroup, b) then b := False;
    FreeSid(AdministratorsGroup);
  end;

  Result := b;
end;

// -----------------------------------------------------------------------------
//
//              RunningAsAdmin
//
// -----------------------------------------------------------------------------
// Detect if we're running at an elevated security level.
// -----------------------------------------------------------------------------
function isRunningAsAdmin: boolean;
var
  hToken, hProcess: THandle;
  pTokenInformation: pointer;
  ReturnLength: DWord;
  TokenInformation: TTokenElevation;
begin
  Result := False;
  hProcess := GetCurrentProcess;
  try
    if OpenProcessToken(hProcess, TOKEN_QUERY, hToken) then
      try
        FillChar(TokenInformation, SizeOf(TokenInformation), 0);
        pTokenInformation := @TokenInformation;
        GetTokenInformation(hToken, TokenElevation, pTokenInformation, SizeOf(TokenInformation), ReturnLength);
        Result := (TokenInformation.TokenIsElevated <> 0);
      finally
        CloseHandle(hToken);
      end;
  except
    // Ignore error - although none of the above should throw an exception...
  end;
end;

function isShiftKeyDown: boolean;
begin
  result := getKeyState(VK_SHIFT) < 0;
end;

initialization

end.
