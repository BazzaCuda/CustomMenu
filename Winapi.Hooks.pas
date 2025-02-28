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
unit Winapi.Hooks;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes;

{$M+}

const
  MAX_KEY_NAME_LENGTH = 100;
  SHIFTED = $8000;

  (*
    * Low level hook flags
  *)
  LLKHF_EXTENDED = $01;
  LLKHF_INJECTED = $10;
  LLKHF_ALTDOWN = $20;
  LLKHF_UP = $80;

const
  VK_A = 65;
  VK_B = 66;
  VK_C = 67;
  VK_D = 68;
  VK_E = 69;
  VK_F = 70;
  VK_G = 71;
  VK_H = 72;
  VK_I = 73;
  VK_J = 74;
  VK_K = 75;
  VK_L = 76;
  VK_M = 77;
  VK_N = 78;
  VK_O = 79;
  VK_P = 80;
  VK_Q = 81;
  VK_R = 82;
  VK_S = 83;
  VK_T = 84;
  VK_U = 85;
  VK_V = 86;
  VK_W = 87;
  VK_X = 88;
  VK_Y = 89;
  VK_Z = 90;
  VK_0 = 48;
  VK_1 = 49;
  VK_2 = 50;
  VK_3 = 51;
  VK_4 = 52;
  VK_5 = 53;
  VK_6 = 54;
  VK_7 = 55;
  VK_8 = 56;
  VK_9 = 57;

type
  THook = class;
  THookMessage = TMessage;
  THookNotify = reference to procedure(Hook: THook; var HookMsg: THookMessage);

  TKeyState = (ksKeyDown = 0, ksKeyIsDown = 1, ksKeyUp = 2);
  pKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;

  KBDLLHOOKSTRUCT = packed record
    vkCode: DWORD;
    ScanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;

  TKBDLLHookStruct = KBDLLHOOKSTRUCT;

  pLowLevelKeyStates = ^TLowLevelKeyStates;

  TLowLevelKeyStates = packed record
    ExtendKey: Boolean;
    InjectedKey: Boolean;
    AltDown: Boolean;
    CtrlDown: Boolean;
    ShiftDown: Boolean;
    KeyState: TKeyState;
    KeyboardState: TKeyboardState;
  end;

  pKeyNames = ^TKeyNames;

  TKeyNames = packed record
    ScanCode: Integer;
    KeyChar: array [0 .. 1] of Char;
    KeyExtName: string;
    // array [0 .. MAX_KEY_NAME_LENGTH] of Char;
  end;

  TKeyStates = packed record
    KeyState: TKeyState;
    KeyDown: Boolean;
    ShiftDown: Boolean;
    AltDown: Boolean;
    CtrlDown: Boolean;
    ExtendedKey: Boolean;
    MenuKey: Boolean;
    KeyRepeated: Boolean;
    RepeatCount: Integer;
    CharCount: Integer;
  end;

  TCustomHook = class abstract
  strict private
    FActive: Boolean;
    FHook: hHook;
    FHookProc: Pointer;
    FThreadID: Integer;

    FOnPreExecute: THookNotify;
    FOnPostExecute: THookNotify;
    procedure HookProc(var HookMsg: THookMessage);
    procedure SetActive(const Value: Boolean);
  private

  protected
    function GetHookID: Integer; virtual; abstract;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); virtual;
    procedure PostExecute(var HookMsg: THookMessage); virtual;

    property Active: Boolean read FActive write SetActive;
    property OnPreExecute: THookNotify read FOnPreExecute write FOnPreExecute;
    property OnPostExecute: THookNotify read FOnPostExecute write FOnPostExecute;

    property ThreadID: Integer read FThreadID write FThreadID;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  THook = class abstract(TCustomHook)
  published
    property Active;
    property OnPreExecute;
    property OnPostExecute;
    property ThreadID;
  end;

  TCallWndProcHook = class sealed(THook)
  private
    FCWPStruct: TCWPStruct;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property CWPStruct: TCWPStruct read FCWPStruct;
  end;

  TCallWndProcRetHook = class sealed(THook)
  private
    FCWPRetStruct: TCWPRetStruct;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property CWPRetStruct: TCWPRetStruct read FCWPRetStruct;
  end;

  TCBTHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TDebugHook = class sealed(THook)
  private
    FDebugHookInfo: TDebugHookInfo;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property DebugHookInfo: TDebugHookInfo read FDebugHookInfo;
  end;

  TGetMessageHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TJournalPlaybackHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TJournalRecordHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TKeyboardHook = class sealed(THook)
  private
    FKeyState: TKeyStates;
    FKeyNames: TKeyNames;
  protected
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
    function GetHookID: Integer; override;
  public
    property KeyStates: TKeyStates read FKeyState;
    property KeyName: TKeyNames read FKeyNames;
  end;

  TMouseHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TMsgHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TShellHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TSysMsgHook = class sealed(THook)
  protected
    function GetHookID: Integer; override;
  end;

  TLowLevelKeyboardHook = class sealed(THook)
  private
    FHookStruct: TKBDLLHookStruct;
    FLowLevelKeyStates: TLowLevelKeyStates;
    FKeyNames: TKeyNames;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property HookStruct: TKBDLLHookStruct read FHookStruct;
    property LowLevelKeyStates: TLowLevelKeyStates read FLowLevelKeyStates;
    property KeyName: TKeyNames read FKeyNames;
  end;

  TLowLevelMouseHook = class sealed(THook)
  strict private
  type
    pMSLLHOOKSTRUCT = ^MSLLHOOKSTRUCT;

    MSLLHOOKSTRUCT = packed record
      Pt: TPoint;
      MouseData: DWORD;
      flags: DWORD;
      time: DWORD;
      dwExtraInfo: ULONG_PTR;
    end;

    TMSLLHookStruct = MSLLHOOKSTRUCT;

  var
    FHookStruct: TMSLLHookStruct;
  protected
    function GetHookID: Integer; override;
    procedure PreExecute(var HookMsg: THookMessage; var Handled: Boolean); override;
    procedure PostExecute(var HookMsg: THookMessage); override;
  public
    property HookStruct: TMSLLHookStruct read FHookStruct;
  end;

type
  THookContainer<T: THook, constructor> = class(TComponent)
  private
    FHook: T;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    class function Construct(AOwner: TComponent): T;
    property Hook: T read FHook;
  end;

  THookInstance<T: THook, constructor> = record
  public
    class function CreateHook(AOwner: TComponent): T; static;
  end;

implementation

uses
  System.SysUtils
//  , VCL.Menus
  ;

{ TLowLevelMouseHook }

function KeyIsDown(const nVirtKey: Integer): Boolean;
begin
  Result := (GetKeyState(nVirtKey) and SHIFTED) <> 0;
end;

function TLowLevelMouseHook.GetHookID: Integer;
begin
  Result := WH_MOUSE_LL;
end;

procedure TLowLevelMouseHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FHookStruct, SizeOf(FHookStruct));
end;

procedure TLowLevelMouseHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  FHookStruct := pMSLLHOOKSTRUCT(HookMsg.WParam)^;
  inherited;
end;

{ TCustomHook }

constructor TCustomHook.Create;
begin
  inherited;
  FHookProc := MakeObjectInstance(HookProc);
  FHook := 0;
  FActive := False;
  FThreadID := GetCurrentThreadID;
end;

destructor TCustomHook.Destroy;
begin
  Active := False;
  FreeObjectInstance(FHookProc);
  inherited;
end;

procedure TCustomHook.HookProc(var HookMsg: THookMessage);
var
  Handled: Boolean;
begin
  Handled := False;
  PreExecute(HookMsg, Handled);
  if not Handled then
  begin
    HookMsg.Result := CallNextHookEx(FHook, HookMsg.Msg, HookMsg.WParam, HookMsg.LParam);
    PostExecute(HookMsg);
  end;
end;

procedure TCustomHook.PostExecute(var HookMsg: THookMessage);
begin
  if Assigned(FOnPostExecute) then
    FOnPostExecute(THook(Self), HookMsg)
end;

procedure TCustomHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  if Assigned(FOnPreExecute) then
    FOnPreExecute(THook(Self), HookMsg);

  Handled := HookMsg.Result <> 0;
end;

procedure TCustomHook.SetActive(const Value: Boolean);
var
  ID: Integer;
begin
  if FActive = Value then
    Exit;

  FActive := Value;

  case Active of
    True:
      begin
        ID := GetHookID;

        if ID in [WH_KEYBOARD_LL, WH_MOUSE_LL] then
          FThreadID := 0;

        FHook := SetWindowsHookEx(GetHookID, FHookProc, HInstance, FThreadID);
        if (FHook = 0) then
        begin
          FActive := False;
          raise Exception.Create(Classname + ' CREATION FAILED!');
        end;
      end;

    False:
      begin
        if (FHook <> 0) then
          UnhookWindowsHookEx(FHook);
        FHook := 0;
      end;
  end;
end;

{ TLowLevelKeyboardHook }

function TLowLevelKeyboardHook.GetHookID: Integer;
begin
  Result := WH_KEYBOARD_LL;
end;

procedure TLowLevelKeyboardHook.PostExecute(var HookMsg: THookMessage);
begin
  ZeroMemory(@FHookStruct, SizeOf(TKBDLLHookStruct));
  ZeroMemory(@FLowLevelKeyStates, SizeOf(TLowLevelKeyStates));
  ZeroMemory(@FKeyNames, SizeOf(TKeyNames));
  inherited;
end;

procedure TLowLevelKeyboardHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
var
  KBS: TKeyboardState;
  VKeyName: array [0 .. MAX_KEY_NAME_LENGTH] of Char;
  dwMsg: DWORD;
  CharCount: Integer;
begin
  CharCount := 1;
  FHookStruct := pKBDLLHOOKSTRUCT(HookMsg.WParam)^;

  GetKeyboardState(KBS);
  Move(KBS, FLowLevelKeyStates.KeyboardState, SizeOf(KBS));

  with FLowLevelKeyStates do
  begin
    ExtendKey := (FHookStruct.flags and LLKHF_EXTENDED) <> 0;
    InjectedKey := (FHookStruct.flags and LLKHF_INJECTED) <> 0;
    AltDown := (FHookStruct.flags and LLKHF_ALTDOWN) <> 0;
    CtrlDown := FHookStruct.vkCode in [VK_LCONTROL, VK_RCONTROL];
    ShiftDown := FHookStruct.vkCode in [VK_LSHIFT, VK_RSHIFT];

    if (FHookStruct.flags and LLKHF_UP) <> 0 then
      KeyState := ksKeyUp
    else
      KeyState := ksKeyDown;
  end;

  dwMsg := 1;
  dwMsg := dwMsg + (FHookStruct.ScanCode shl 16);
  dwMsg := dwMsg + (FHookStruct.flags shl 24);


  if GetKeyNameText(dwMsg, @VKeyName, SizeOf(VKeyName)) > 0 then
    FKeyNames.KeyExtName := VKeyName
  else
    FKeyNames.KeyExtName := '';

  ZeroMemory(@VKeyName, $D * SizeOf(Char));
  ZeroMemory(@FKeyNames.KeyChar, 2 * SizeOf(Char));
  FKeyNames.ScanCode := FHookStruct.vkCode;
  try
    CharCount :=
//{$IFDEF UNICODE}
      ToUnicode(FHookStruct.vkCode, FHookStruct.ScanCode, KBS, @VKeyName, 2, 0);
//{$ELSE}
//      ToAscii(FHookStruct.vkCode, FHookStruct.ScanCode, KBS, @VKeyName, 0);
//{$ENDIF}
  except

  end;
  Move(VKeyName, FKeyNames.KeyChar, CharCount);
  inherited;
end;

{ TCallWndProcHook }

function TCallWndProcHook.GetHookID: Integer;
begin
  Result := WH_CALLWNDPROC;
end;

procedure TCallWndProcHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FCWPStruct, SizeOf(TCWPStruct));
end;

procedure TCallWndProcHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  inherited;
  FCWPStruct := PCWPStruct(HookMsg.LParam)^;
end;

{ TCallWndProcRetHook }

function TCallWndProcRetHook.GetHookID: Integer;
begin
  Result := WH_CALLWNDPROCRET;
end;

procedure TCallWndProcRetHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FCWPRetStruct, SizeOf(TCWPRetStruct));
end;

procedure TCallWndProcRetHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  FCWPRetStruct := pCWPRetStruct(HookMsg.LParam)^;
  inherited;
end;

{ TCBTHook }

function TCBTHook.GetHookID: Integer;
begin
  Result := WH_CBT;
end;

{ TDebugHook }

function TDebugHook.GetHookID: Integer;
begin
  Result := WH_DEBUG;
end;

procedure TDebugHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FDebugHookInfo, SizeOf(TDebugHookInfo));
end;

procedure TDebugHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
begin
  FDebugHookInfo := pDebugHookInfo(HookMsg.LParam)^;
  inherited;
end;

{ TGetMessageHook }

function TGetMessageHook.GetHookID: Integer;
begin
  Result := WH_GETMESSAGE;
end;

{ TJournalPlaybackHook }

function TJournalPlaybackHook.GetHookID: Integer;
begin
  Result := WH_JOURNALPLAYBACK;
end;

{ TJournalRecordHook }

function TJournalRecordHook.GetHookID: Integer;
begin
  Result := WH_JOURNALRECORD;
end;

{ TKeyboardHook }

function TKeyboardHook.GetHookID: Integer;
begin
  Result := WH_KEYBOARD;
end;

procedure TKeyboardHook.PostExecute(var HookMsg: THookMessage);
begin
  inherited;
  ZeroMemory(@FKeyState, SizeOf(TKeyState));
  ZeroMemory(@FKeyNames, SizeOf(TKeyNames));
end;

procedure TKeyboardHook.PreExecute(var HookMsg: THookMessage; var Handled: Boolean);
var
  KBS: TKeyboardState;
  VKeyName: array [0 .. MAX_KEY_NAME_LENGTH - 1] of Char;
  CharCount: Integer;
begin
  FKeyState.KeyDown := (HookMsg.LParam and (1 shl 31)) = 0;
  FKeyState.KeyRepeated := (HookMsg.LParam and (1 shl 30)) = 0;
  FKeyState.AltDown := KeyIsDown(VK_MENU);
  FKeyState.MenuKey := (HookMsg.LParam and (1 shl 28)) = 0;
  FKeyState.ExtendedKey := (HookMsg.LParam and (1 shl 24)) = 0;
  FKeyState.CtrlDown := KeyIsDown(VK_CONTROL);
  FKeyState.ShiftDown := (GetKeyState(VK_SHIFT) and (1 shl 15)) = 0;
  FKeyState.KeyState := TKeyState(HookMsg.LParam shr 30);

  FKeyNames.ScanCode := HookMsg.Msg;

  if (FKeyState.KeyRepeated and FKeyState.KeyDown) then
    Inc(FKeyState.RepeatCount)
  else
    FKeyState.RepeatCount := 0;

  GetKeyboardState(KBS);

  if GetKeyNameText(HookMsg.WParam, @VKeyName, SizeOf(VKeyName)) > 0 then
    FKeyNames.KeyExtName := VKeyName
  else
    FKeyNames.KeyExtName := '';

  ZeroMemory(@VKeyName, $D * SizeOf(Char));
  ZeroMemory(@FKeyNames.KeyChar, $2 * SizeOf(Char));
  try
    CharCount :=
//{$IFDEF UNICODE}
      ToUnicode(HookMsg.Msg, HookMsg.LParam, KBS, @VKeyName, 2, 0);
//{$ELSE}
//      ToAscii(HookMsg.Msg, HookMsg.LParam, KBS, @VKeyName, 0);
//{$ENDIF}
  except
    CharCount := 1;
  end;

  if VKeyName[0] = VKeyName[1] then
    CharCount := 1;

  // There is a minor bug in Windows when pressing the TREMA (и) key
  // ToUnicode actual return two chars (ии) insted of one (и)

  FKeyState.CharCount := CharCount;
  Move(VKeyName, FKeyNames.KeyChar, CharCount * SizeOf(Char));

  inherited;
end;

{ TMouseHook }

function TMouseHook.GetHookID: Integer;
begin
  Result := WH_MOUSE;
end;

{ TMsgHook }

function TMsgHook.GetHookID: Integer;
begin
  Result := WH_MSGFILTER;
end;

{ TShellHook }

function TShellHook.GetHookID: Integer;
begin
  Result := WH_SHELL;
end;

{ TSysMsgHook }

function TSysMsgHook.GetHookID: Integer;
begin
  Result := WH_SYSMSGFILTER;
end;

{ THookContainer<T> }

class function THookContainer<T>.Construct(AOwner: TComponent): T;
begin
  Result := THookContainer<T>.Create(AOwner).FHook;
end;

constructor THookContainer<T>.Create(AOwner: TComponent);
begin
  inherited;
  FHook := T.Create;
end;

destructor THookContainer<T>.Destroy;
begin
  FHook.Free;
  inherited;
end;

{ THookInstance<T> }

class function THookInstance<T>.CreateHook(AOwner: TComponent): T;
begin
  Result := THookContainer<T>.Construct(AOwner)
end;

end.
