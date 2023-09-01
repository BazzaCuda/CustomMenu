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
  hotkeyEnabled: boolean;
  hotkeyAtom:    integer;

implementation

uses
  vcl.menus, customMenuCommon, _debugWindow;

const
  CM_HOTKEY_FILE_NAME = 'hotkey';

var
  hotkeyWND:  HWND;

{$R *.dfm}

function readHotkey: integer;
begin
  result := 0;
  var sl := TStringList.create;
  try
    sl.loadFromFile(getExePath + CM_HOTKEY_FILE_NAME);
    case sl.count = 0 of TRUE: EXIT; end;
    result := strToIntDef(sl[0], 0);
  finally
    sl.free;
  end;
end;

function registerHotkey(hotkey: integer): integer;
var
  key, mods: WORD;
  shift: TShiftState;
begin
  result := 0;
  case hotkeyEnabled of TRUE: EXIT; end;

  hotkeyAtom := globalAddAtom('CustomMenuHotkey');

  ShortCutToKey(hotkey, key, shift);
  mods := 0;
  case ssCtrl   in shift of TRUE: mods := mods or MOD_CONTROL; end;
  case ssShift  in shift of TRUE: mods := mods or MOD_SHIFT; end;
  case ssAlt    in shift of TRUE: mods := mods or MOD_ALT; end;
  hotkeyEnabled := winAPI.windows.RegisterHotKey(hotkeyWnd, hotkeyAtom, mods, key);
  result := key;
end;

function checkHotkey(aWND: HWND): boolean;
var
  hotkeyForm: THotkeyForm;
begin
  case fileExists(getExePath + CM_HOTKEY_FILE_NAME) of FALSE: EXIT; end;
  hotkeyWnd := aWND;
  var hotkey := readHotkey;
  case hotkey <> 0 of  TRUE:  registerHotKey(hotkey);
                      FALSE:  begin
                                hotkeyForm := THotkeyForm.create(NIL);
                                try
                                  hotkeyForm.showModal;
                                finally
                                  hotkeyForm.free;
                                end;end;end;
end;

function saveHotkey(aHotkey: integer): boolean;
begin
  var sl := TStringList.create;
  try
    sl.add(intToStr(aHotkey));
    sl.saveToFile(getExePath + CM_HOTKEY_FILE_NAME);
  finally
    sl.free;
  end;
end;

procedure THotkeyForm.pnlOKClick(Sender: TObject);
begin
  saveHotkey(registerHotkey(hotkey.hotkey));
  modalResult := mrOK;
end;

initialization
  hotkeyEnabled := FALSE;
  hotkeyAtom    := 0;

finalization
  case hotkeyEnabled of TRUE: winAPI.windows.UnregisterHotKey(hotKeyWnd, hotkeyAtom); end;

end.
