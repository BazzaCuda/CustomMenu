unit FormHotkeys;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  THotkeyForm = class(TForm)
    hkHotkey: THotKey;
    Label1: TLabel;
    pnlOK: TPanel;
    procedure pnlOKClick(Sender: TObject);
  private
  public
    property hotKey: THotKey read hkHotKey;
  end;

function checkHotkey(aWND: HWND): boolean;

var
  FHotkeyEnabled: boolean;
  FHotkeyAtom:    integer;

implementation

uses
  vcl.menus, customMenuCommon, _debugWindow;

const
  CM_HOTKEY_FILE_NAME = 'hotkey';

var
  FHotkeyWND:   HWND;
  FHotKey:      integer;
  FShift:       THKModifiers;

{$R *.dfm}

function SetToInt(const aSet; const Size: integer): integer;
begin
  Result := 0;
  Move(aSet, Result, Size);
end;

procedure IntToSet(const Value:integer; var aSet; const Size: integer);
begin
  Move(Value, aSet, Size);
end;

function readHotkey: boolean;
begin
  result := FALSE;
  FHotkey := 0;
  FShift := [];

  var sl := TStringList.create;
  try
    sl.loadFromFile(getExePath + CM_HOTKEY_FILE_NAME);
    case sl.count = 0 of TRUE: EXIT; end;
    FHotkey := strToIntDef(sl[0], 0);
    case sl.count > 1 of TRUE: intToSet(strToIntDef(sl[1], 0), FShift, sizeOf(FShift)); end;
    result := TRUE;
  finally
    sl.free;
  end;
end;

function shortcutAsKey(aHotkey: integer): integer;
var
  key: WORD;
  dummyShift: TShiftState;
begin
  ShortCutToKey(aHotkey, key, dummyShift);
  result := key;
end;

function registerHotkey(aHotkey: integer; aShift: THKModifiers): boolean;
var
  key, mods: WORD;
begin
  result := FALSE;
  case FHotkeyEnabled of TRUE: EXIT; end;

  FHotkeyAtom := globalAddAtom('CustomMenuHotkey');

  key := shortcutAsKey(aHotkey);

  mods := 0;
  case hkCtrl   in aShift of TRUE: mods := mods or MOD_CONTROL; end;
  case hkShift  in aShift of TRUE: mods := mods or MOD_SHIFT; end;
  case hkAlt    in aShift of TRUE: mods := mods or MOD_ALT; end;
  FHotkeyEnabled := winAPI.windows.RegisterHotKey(FHotkeyWnd, FHotkeyAtom, mods, key);
  result := TRUE;
end;

function checkHotkey(aWND: HWND): boolean;
var
  hotkeyForm: THotkeyForm;
begin
  case fileExists(getExePath + CM_HOTKEY_FILE_NAME) of FALSE: EXIT; end;
  FHotkeyWnd := aWND;
  case readHotkey of   TRUE:  registerHotKey(FHotkey, FShift);
                      FALSE:  begin
                                hotkeyForm := THotkeyForm.create(NIL);
                                try
                                  hotkeyForm.showModal;
                                finally
                                  hotkeyForm.free;
                                end;end;end;
end;

function saveHotkey(aHotkey: integer; aShift: THKModifiers): boolean;
begin
  var sl := TStringList.create;
  try
    sl.add(intToStr(shortcutAsKey(aHotkey)));
    case aShift <> [] of TRUE: sl.add(intToStr(setToInt(aShift, sizeOf(aShift)))); end;
    sl.saveToFile(getExePath + CM_HOTKEY_FILE_NAME);
  finally
    sl.free;
  end;
end;

{ THotkeyForm }

procedure THotkeyForm.pnlOKClick(Sender: TObject);
begin
  registerHotkey(hotkey.hotkey, hotkey.Modifiers);
  saveHotkey(hotkey.hotkey, hotkey.Modifiers);
  modalResult := mrOK;
end;

initialization
  FHotkeyEnabled := FALSE;
  FHotkeyAtom    := 0;

finalization
  case FHotkeyEnabled of TRUE: winAPI.windows.UnregisterHotKey(FHotKeyWnd, FHotkeyAtom); end;

end.
