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
unit FormCustomMenu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.Menus, System.Generics.Collections, strutils,

  WinAPI.Hooks, CustomMenuCommon, Vcl.Buttons;

type
  TListBox = class(Vcl.StdCtrls.TListBox)           // a technique worth remembering!
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  TMenuType = (mtTBD, mtRegKey, mtINI, mtBrowse);

  TCustomMenu = class(TForm)
    menuTimer: TTimer;
    trayIcon: TTrayIcon;
    imageList1: TImageList;
    listBox: TListBox;
    imageList2: TImageList;
    dummyLabel: TLabel;
    trayMenu: TPopupMenu;
    menuExit: TMenuItem;
    bottomPanel: TPanel;
    btnDown: TSpeedButton;
    btnUp: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure menuTimerTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure listBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure listBoxMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure listBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure listBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure listBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure menuExitClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure btnUpMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;  MousePos: TPoint; var Handled: Boolean);
  private
    FHook:  THook;
    FPt:    TPoint;

    itemData: TList<TItemData>;
    MenuID:   integer;            // a unique identifier for this menu/window/form

    parentForm: TCustomMenu;      // the form that owns this subMenu

    subMenu:        TCustomMenu;  // any subMenu window created by this menu/window/form
    subMenuItemID:  integer;      // the itemID of the TItemData listBox item that spawned the submenu

    FListBoxWndProc: TWndMethod;

    FCtrlClickActivate: boolean;

    var FOldHintIx: integer;
    procedure listBoxWndProc(var Msg: TMessage);

    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
    procedure WMHotKey(var Msg : TWMHotKey); message WM_HOTKEY;

    function  buildAndShowTheMenu: boolean;
    function  buildMenu(menuType: TMenuType; extraInfo: array of string): boolean;
    function  closeApp: boolean;
    function  configListBox: boolean;
    function  hookOff: boolean;
    function  setMouseTrap: boolean;
    function  shutForm: boolean;
    function  shutSubMenu(ownerItemID: integer = -1): boolean;
//    procedure WMXYMessage(var Msg: TMessage); message WM_XY_MESSAGE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    function shutMenus: boolean;
    property ctrlClickActivate: boolean read FCtrlClickActivate write FCtrlClickActivate;
    property Hook: THook read FHook;
    property Pt: TPoint read FPt write FPt;
  end;

var
  mainMenu: TCustomMenu;
  GREFRESH: boolean;

//function mainMenu: TCustomMenu; // only because the IDE keeps changing the .dpr file from what I want!

implementation

uses System.win.Registry, ShellAPI, WinAPI.CommCtrl, FormConfig, system.types, FormHotkeys, _debugWindow;

var
  GFIRST:     boolean;
  hWNDs:      TList<HWND>;
  menuID:     integer = -1;     // given to each menu/window/form - the main menu/window/form will have a menuID of 0
  itemID:     integer = -1;     // given to each item in a menu so we know which item owns a submenu
  GDotWidth:  integer;

//function mainMenu: TCustomMenu;
//begin
//  result := customMenu;
//end;

function newMenuID: integer;
begin
  inc(menuID);
  result := menuID;
end;

function newItemID: integer;
begin
  inc(itemID);
  result :=itemID;
end;

function adjustWindowPosX(WND: HWND): boolean;
var vWndRect : TRect;
begin
  getWindowRect(WND, vWndRect); // the dimensions of the bounding rectangle given in screen coordinates relative to screen's upper-left corner
  var vWndWidth := vWndRect.right - vWndRect.left;

  var vDesktopRect := screen.WorkAreaRect; // the screen minus the taskbar, which we assume is at the bottom of the desktop
  var vDesktopWidth := vDesktopRect.right - vDesktopRect.left;

  case vWndWidth > vDesktopWidth of TRUE: EXIT; end;                  // Too wide - not our problem
  case vWndRect.left + vWndWidth > vDesktopWidth of FALSE: EXIT; end; // width: entirely visible!

  // It will fit on the screen but it's too far right and some of it is missing
  setWindowPos(WND, 0, vDesktopWidth - vWndWidth, vWndRect.top, 0, 0, SWP_NOSIZE);
end;

function adjustWindowPosY(WND: HWND): boolean;
var vWndRect : TRect;
begin
  getWindowRect(WND, vWndRect); // the dimensions of the bounding rectangle given in screen coordinates relative to screen's upper-left corner
  var vWndHeight := vWndRect.bottom - vWndRect.top;

  var vDesktopRect := screen.WorkAreaRect; // the screen minus the taskbar, which we assume is at the bottom of the desktop
  var vDesktopHeight := vDesktopRect.bottom - vDesktopRect.top;

  case vWndHeight > vDesktopHeight of TRUE: EXIT; end;                  // Too tall - not our problem
  case vWndRect.top + vWndHeight > vDesktopHeight of FALSE: EXIT; end;  // height: entirely visible!

  // It will fit on the screen but it's too low and some of it is missing
  setWindowPos(WND, 0, vWndRect.left, vDesktopHeight - vWndHeight, 0, 0, SWP_NOSIZE);
end;

function deleteHWND(aHWND: HWND): boolean;
begin
  EXIT;
  for var i: integer := 0 to hWNDs.Count - 1 do
    case hWNDs[i] = aHWND of TRUE: hWNDs.Delete(i); end;
end;

function menuWnd(mouseWnd: HWND): boolean;
begin
  result := TRUE;
  for var i: integer := 0 to hWNDs.Count - 1 do
    case hWNDs[i] = mouseWnd of TRUE: EXIT; end;
  result := FALSE;
end;

function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
var
  l: integer;
begin
  if ws = '' then
    Result := ''
  else
  begin
    l := WideCharToMultiByte(codePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @ws[1], - 1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], - 1, @Result[1], l - 1, nil, nil);
  end;
end;


function buildMenuFromFiles(aPath: string; aMaskList: string; itemData: TList<TItemData>; imageList: TImageList): boolean;
const
  faDIR   = faDirectory + faSysFile + faHidden;
  faAny   = faAnyFile - faHidden - faSysFile;
  faFile  = faAnyFile - faDirectory - faHidden - faSysFile;
var
  vIcon:       TIcon;
  vFileInfo:   SHFILEINFOW;
  vExt:        string;
  vAll:        boolean;
  vIncDirs:    boolean;
  vDirsOnly:   boolean;
  id:          TItemData;
  findData:    WIN32_FIND_DATAW;
  hFind:       THandle;

  function doIcon: boolean;
  begin
    SHGetFileInfo(PWideChar(aPath + findData.cFileName), 0, vFileInfo, SizeOf(vFileInfo), SHGFI_ICON or SHGFI_SMALLICON);
    vIcon.Handle := vFileInfo.hIcon;
    imageList.AddIcon(vIcon);
    DestroyIcon(vFileInfo.hIcon);
  end;

begin
  aPath     := includeTrailingBackslash(aPath);

  case trim(aMaskList) = '' of  TRUE: aMaskList := '*.*';
                               FALSE: aMaskList := lowerCase(aMaskList); end;

  vAll      := pos('*.*', aMaskList) > 0;
  vIncDirs  := pos('+folders', aMaskList) > 0;
  vDirsOnly := pos('-files', aMaskList) > 0;
  vIcon     := TIcon.Create;
  id        := default(TItemData);

  try

  case vIncDirs of TRUE: begin
    hFind := findFirstFileW(PChar(aPath + '*.*'), findData);
    case hFind <> INVALID_HANDLE_VALUE of TRUE:
      repeat
        case (findData.dwFileAttributes AND faDIR) = faDirectory of FALSE: CONTINUE; end; // only folders which aren't hidden or system
        case {isDotDir} (findData.cFileName[0] = '.') or (findData.cFileName = '..') of TRUE:  CONTINUE; end;

        SHGetFileInfoW(PWideChar(aPath + findData.cFileName), 0, vFileInfo, SizeOf(vFileInfo), SHGFI_DISPLAYNAME);
        id.idName       := vFileInfo.szDisplayName;
        id.idCommand    := 'browse';
        id.idSubMenu    := TRUE;
        id.idDirectory  := includeTrailingBackslash(aPath + id.idName);
        id.idHint       := id.idDirectory;
        id.idParams     := aMaskList;
        id.idItemID     := newItemID;
        itemData.add(id);
        id := default(TItemData);
        doIcon;
      until NOT findNextFile(hFind, findData);
    end;
  end;end;

  closehandle(hFind);
  case vDirsOnly of TRUE: EXIT; end;

    hFind := findFirstFileW(PChar(aPath + '*.*'), findData);
    case hFind <> INVALID_HANDLE_VALUE of TRUE:
    repeat
      case (findData.dwFileAttributes AND faDIR) <> 0 of TRUE: CONTINUE; end;

      vExt := lowerCase(extractFileExt(findData.cFileName));
      case pos('-*' + vExt, aMaskList) > 0 of TRUE: CONTINUE; end;                 // extension excluded in masks
      case not vAll and (pos('*' + vExt, aMaskList) = 0) of TRUE: CONTINUE; end;   // file extension not listed in masks

      SHGetFileInfoW(PWideChar(aPath + findData.cFileName), 0, vFileInfo, SizeOf(vFileInfo), SHGFI_DISPLAYNAME);
      id.idName     := vFileInfo.szDisplayName;
      id.idCommand  := aPath + findData.cFileName;
      id.idHint     := aPath + id.idName;
      itemData.add(id);
      id := default(TItemData);
      doIcon;
      until NOT findNextFile(hFind, findData);
  end;

  closeHandle(hFind);

  finally
    vIcon.Free;
  end;
end;

var iniFile: TStringList = NIL; // temporary hack. Was a local variable
function buildMenuFromINI(iniFilePath: string; itemData: TList<TItemData>; subMenuName: string = ''): boolean;
var
  i:        integer;

  function iniClause(clauseName: string): string;
  // extracts a named clause from the one-line config
  var
    posClause:  integer;
    posEquals:  integer;
    posDelim:   integer;
  begin
    result := '';
    posClause := pos(lowerCase(clauseName) + '=', lowerCase(iniFile[i])); // is there a clause with this name?
    case posClause = 0 of TRUE: EXIT; end;
    posEquals := pos('=', iniFile[i], posClause);   // find the = after the clause name
    case posEquals = 0 of TRUE: EXIT; end;
    posDelim := pos(';', iniFile[i], posEquals);    // find the ; at the end of the clause
    case posDelim = 0 of TRUE: EXIT; end;
    result := copy(iniFile[i], posEquals + 1, posDelim - posEquals - 1); // return whatever's inbetween the two.
  end;

begin
  case GREFRESH and (iniFile <> NIL) of TRUE: begin iniFile.free; iniFile := NIL; end;end; // user did Alt-RightClick on desktop to force reload of the ini file
  case (iniFile = NIL) and (not fileExists(iniFilePath)) of TRUE: begin result := FALSE; EXIT; end;end;
  case iniFile = NIL of TRUE: iniFile := TStringList.create; end;
  subMenuName := lowerCase(subMenuName); // so that subMenuName comparison isn't case sensitive
  try

    case iniFile.count = 0 of TRUE: iniFile.loadFromFile(iniFilePath); end; // don't keep reloading from disk
    for i := 0 to iniFile.count - 1 do begin
      case trim(iniFile[i])     = ''  of TRUE: CONTINUE; end;                                                     // ignore blank lines in ini file
      case trim(iniFile[i])[1]  = ':' of TRUE: CONTINUE; end;                                                     // ignore comment lines beginning with a colon
      case (subMenuName <> '') and (subMenuName <> lowerCase(iniClause('subMenuName'))) of TRUE: CONTINUE; end;   // filter by the passed subMenuName, if any
      case (subMenuName =  '') and (iniClause('subMenuName') <> '') of TRUE: CONTINUE; end;                       // filter out all submenu items from the main menu

      var id: TItemData;
      id.idName           := iniClause('name'); // the name/text of the item as it appears in the menu. Allow for SPACE after the icon.
      id.idSubMenu        := lowerCase(iniClause('subMenu'))        = 'yes';
      id.idSeparatorAfter := lowerCase(iniClause('separatorAfter')) = 'yes';
      id.idHasLUAShield   := lowerCase(iniClause('hasLUAshield'))   = 'yes';
      id.idIconFile       := iconFileFromIconEntry(iniClause('icon'));
      id.idIconIx         := iconIxFromIconEntry(iniClause('icon'));
      id.idCommand        := iniClause('command');
      id.idParams         := iniClause('params');
      id.idDirectory      := iniClause('directory');
      id.idRunType        := iniClause('runType');
      id.idRunAsAdmin     := lowerCase(iniClause('runAsAdmin'))     = 'yes';
      id.idHint           := iniClause('hint');

      id.idItemID := newItemID;  // unique to this item in this session only

      itemData.Add(id);
      id := default(TItemData); // clear all the fields
    end;

  finally
//    iniFile.free; // moved to Finalization
    result := itemData.count > 0;
  end;

end;

function buildMenuFromItemData(itemData: TList<TItemData>; listBox: TListBox): boolean;
begin
  lockWindowUpdate(listBox.handle);
  screen.cursor := crHourGlass;
  try
    for var i: integer := 0 to itemData.Count - 1 do listBox.Items.Add(itemData[i].idName); // this nobbles any unicode characters in idName. See listBoxDrawItem.
  finally
    screen.cursor := crDefault;
    lockWindowUpdate(0);
  end;
end;

function checkScrollButtons(btnUp: TSpeedButton; btnDown: TSpeedButton; listBox: TListBox): boolean;
begin
  btnUp.enabled   := listBox.topIndex > 0;
  btnDown.enabled := listBox.topIndex < (listBox.items.count - 1) - 39;
end;

function openSubMenu(parentForm: TCustomMenu; itemDataIx: integer; ownerItemID: integer; aPt: TPoint; disableParent: boolean = FALSE): boolean;
// If the user moves the mouse within a subMenu item, it generates multiple calls to openSubMenu without
// having closed the previous instance of the subMenu. Consequently, we check whether the subMenu already exists
// and is being viewed, and bail-out immediately rather than creating duplicates.
//
// disableParent is only TRUE when this subMenu is triggered by the user using the keyboard UP and DOWN arrows on the menu (see listBoxKeyUp).
// This is necessary because if they have left the mouse over another item, TListBox will generate a MouseMove for that item even with the mouse physically disconnected!!!
// Consequently, this app would immediately close THIS subMenu and open the one over which the mouse is hovering.
// Disabling the parent menu alleviates this problem while still allowing the user to use a mixture of keyboard and mouse to navigate the menus.
// In this scenario however, the user will need to use the keyboard LEFT ARROW or the ESCAPE key to close a subMenu that was triggered by the keyboard.
//
// You can replicate this problem quite easily:
// 1. Move the mouse over a subMenu item so that the subMenu opens and leave the mouse cursor where it is.
// 2. Use either the LEFT ARROW key or the ESCAPE key to close the subMenu.
// 3. Use the UP ARROW or DOWN ARROW keys to navigate to another subMenu item so that the subMenu opens.
// 4. As in step 2, use either the LEFT ARROW key or the ESCAPE key to close the subMenu
// 5. The bogus MouseMove event in listBoxMouseMove will reopen the subMenu under the mouse cursor.
// This problem can also be seen if the mouse cursor is left over items other than subMenus, but it's not quite so obvious.
var
  allowBrowse: boolean;
  wannaBrowse: boolean;
begin
  case parentForm.subMenu <> NIL of TRUE: EXIT; end;             // "I told him we've already got one; it's verrry nice."

  var vPt: TPoint             := parentForm.ClientToScreen(aPt);
  var vSubMenu: TCustomMenu   := TCustomMenu.Create(nil);        // create another TCustomMenu form for the submenu
  vSubMenu.FPt                := vPt;                            // tell the subMenu where to display itself
  vSubMenu.menuID             := newMenuID;                      // give a unique menuID
  parentForm.subMenu          := vSubMenu;                       // tell the parent menu it has a child
  vSubMenu.parentForm         := parentForm;                     // tell the child who its parent is
  parentForm.subMenuItemID    := ownerItemID;                    // and which itemID spawned it
  var id: TItemData           := parentForm.itemData[itemDataIx];

  var finalSegment: shortString;
  wannaBrowse := id.idCommand = 'browse';
  allowBrowse := (length(id.idDirectory) <= 3) or (pos('-files', lowerCase(id.idParams)) <> 0); // e.g. B:\ c:  and always let them browse just folders
  case wannaBrowse and not allowBrowse of TRUE: begin
                                                  finalSegment  := id.idDirectory;
                                                  setLength(finalSegment, length(finalSegment) - 1); // buildMenuFromFiles always adds a trailing backslash
                                                  finalSegment  := lowerCase(extractFileName(finalSegment)) + '\';
                                                  allowBrowse   := pos(finalSegment, 'cursors\winsxs\inf\system32\syswow64\temp\drivers\') = 0; end;end; // not practical

  case wannaBrowse of
     TRUE: case allowBrowse of TRUE: vSubMenu.buildMenu(mtBrowse, [id.idDirectory, id.idParams]); end;
    FALSE: vSubMenu.buildMenu(mtINI, [getINIFileName, id.idName]); end;

  case vSubMenu.listBox.items.count = 0 of TRUE: begin
                                                  id.idSubMenu := FALSE; // the chevron will be disabled in drawItem for empty folders
                                                  id.idBrowseBlocked := wannaBrowse and NOT allowBrowse;
                                                  parentForm.itemData[itemDataIx] := id;
                                                  parentForm.listBox.refresh; EXIT; end;end;

  case disableParent of TRUE: enableWindow(parentForm.handle, FALSE); end;
end;

function resizeListBox(listBox: TListBox; dummyLabel: TLabel; isMainMenu: boolean): boolean;
const
  subMenuRes = 8 + 16 + 8 + 0 + 8 + 16; // margin + icon + margin + <available space> + margin + chevron
var
  i: integer;
  vAvg: integer;
begin
  case listBox.items.count = 0 of TRUE: EXIT; end;

  case isMainMenu or (listBox.items.count <= 40) of  TRUE: listBox.height := listBox.Items.count * CM_ITEM_HEIGHT + 2; // allow for a 2-pixel margin at the bottom to match the top
                                                    FALSE: listBox.height := 40 * CM_ITEM_HEIGHT + 2; end; // limit the height and show the scroll buttons


  { The width of the menu is based on the longest textWidth of the first 40 to be displayed, up to a maximum of 420 pixels }
  var vLongest: integer;
  var vTextWidth: integer;
  for i := 0 to 40 do begin     // base the subMenu listBox width only on the first 40 items that will be displayed
    vTextWidth := listBox.canvas.textWidth(listBox.items[i]);
    case vTextWidth > vLongest of TRUE: vLongest := vTextWidth; end;
    case i = listBox.count - 1 of TRUE: BREAK; end;end; // less than 40 in the list

  listBox.width := vLongest + subMenuRes;

  case isMainMenu of TRUE: // make sure the main menu looks presentable - subMenus can be narrower based on the item name lengths
  case listBox.width < 200 of TRUE: listBox.width := 200; end;end;  // don't let files/folders with very short names make the [browse] menu look silly
  case listBox.width > 420 of TRUE: listBox.width := 420; end;      // don't let files/folders with very long names make the [browse] menu look silly

  GDotWidth := listBox.canvas.textWidth('...');
end;

function resizeWindow(form: TCustomMenu): boolean;
begin
  form.width   := form.listBox.Width + 2;   // listBox.width will lose these 2 pixels when alAlign := alTop;
  form.height  := form.listBox.Height;
  case form.bottomPanel.visible of TRUE: form.height := form.height + form.bottomPanel.height; end;
end;

function prepareMenuForDisplay(form: TCustomMenu): boolean;
begin
  case (menuID <> 0) and (form.listBox.items.count = 0) of TRUE: begin result := FALSE; EXIT; end;end;
  case (menuID <> 0) and (form.listBox.items.count > 0) of TRUE: form.listBox.itemIndex := 0; end; // select the first item in a subMenu so keyboard UP/DOWN keys are primed
  form.listBox.align := alNone;
  resizeListBox(form.listBox, form.dummyLabel, form.MenuID = 0);

  // it's up to the user how many items are in the main menu. For subMenus, we limit to 40 items to make browsing folders manageable.
  case form.MenuID <> 0 of TRUE: form.bottomPanel.visible := form.listBox.items.count > 40; end;
  checkScrollButtons(form.btnUp, form.btnDown, form.listBox);

  hWNDs.Add(form.listBox.handle); // delayed until now; the handle changes when the listBox has items added to it!
  resizeWindow(form);
  form.listBox.align := alTop;
  SetWindowPos(form.handle, HWND_TOP, form.FPt.X, form.FPt.Y, 0, 0, SWP_SHOWWINDOW OR SWP_NOSIZE);
  SetWindowPos(form.handle, HWND_TOPMOST, form.FPt.X, form.FPt.Y, 0, 0, SWP_SHOWWINDOW OR SWP_NOSIZE);
  adjustWindowPosX(form.handle);
  adjustWindowPosY(form.handle);

  { NB  If the user is quick enough with the mouse after right-clicking the desktop, this keyboard event can go
        to any application the user has open. So the sent key needs to be entirely inocuous.
        e.g even VK_SHIFT sent to the window of a text editor can select text when the mouse moves, so it isn't suitable.
        7 is currently undefined (https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes) }
  keybd_event(7, 0, 0, 0); // simulate keyboard input in this app, otherwise SetForegroundWindow doesn't always work
  SetForegroundWindow(form.handle);
end;

function showTrayIcon(trayIcon: TTrayIcon): boolean;
begin
  GTrayExit               := mainMenu.menuExit;
  trayIcon.Visible        := TRUE;
  trayIcon.BalloonTimeout := 3000;
  trayIcon.Hint           := 'CustomMenu: Shift-RightClick the desktop to close';
  trayIcon.BalloonTitle   := 'Custom Menu';
  trayIcon.BalloonHint    := 'Right-click the desktop!';
  trayIcon.ShowBalloonHint;
end;

//==========  Form Methods ==========
{$R *.dfm}

function TCustomMenu.buildAndShowTheMenu: boolean;
begin
  case GREFRESH of TRUE: begin itemDataclear(itemData); itemData.clear; imageList1.Clear; hWNDs.Clear; listBox.Clear; end;end;
  case GFIRST or GREFRESH of  TRUE: result := buildMenu(mtINI, [getINIFileName, '']);  // builds the main menu once. For subMenus, buildMenu will be called in openSubMenu
                             FALSE: result := TRUE; end; // because subsequent calls don't call buildMenu unless GREFRESH is TRUE
  GFIRST := FALSE; GREFRESH := FALSE;

  case result of  TRUE: prepareMenuForDisplay(self); // redisplays the main menu on subsequent right-clicks on the desktop
                 FALSE: showConfigForm; end;

end;

//========== CLOSING MENUS AND SUBMENUS ==========
function TCustomMenu.hookOff: boolean;
begin try
  case FHook = NIL of TRUE: EXIT; end;                 // This can't be the main menu/window/form
  case menuID = 0 of TRUE: FHook.Active := FALSE; end; // only unhook when closing the main form/app, not a submenu
  except debug('exception in hookOff'); end;
end;

function TCustomMenu.closeApp: boolean;
begin
  shutConfigForm;
  shutForm;
  close;
end;

function TCustomMenu.shutForm: boolean;
begin try
  hookOff;               // only if this is the mainMenu.
  shutSubMenu;           // and all subordinate submenus
  case listBox = NIL of TRUE: debug('lisbox = NIL'); end;
  deleteHWND(listBox.handle);
  case parentForm <> NIL of TRUE: enableWindow(parentForm.handle, TRUE); end;
  case itemData <> NIL of TRUE: begin itemDataClear(itemData); itemData.clear; itemData.free; itemData := NIL; end;end;
  except debug('exception in shutForm'); end;
end;

function TCustomMenu.shutSubMenu(ownerItemID: integer = -1): boolean;
// shut the subMenu if it's not owned by ownerItemID
begin try
  case subMenu = NIL of TRUE: EXIT; end;
  case (ownerItemID <> -1) and (ownerItemID = subMenuItemID) of TRUE: EXIT; end;      // only close the current submenu if the mouse moves over a different menu item to the current submenu's owner item
  submenu.shutForm; subMenu.close; subMenu.free; subMenu := NIL; subMenuItemID := -1; // close this menu's/window's/form's current submenu
  except debug('exception in shutSubMenu'); end;
end;

procedure TCustomMenu.btnDownClick(Sender: TObject);
begin
  shutSubMenu;
  case listBox.topIndex   < listBox.items.count - 1 of TRUE:  listBox.TopIndex  := listBox.TopIndex + 1;  end;
  case listBox.itemIndex  < listBox.items.count - 1 of TRUE:  listBox.itemIndex := listBox.itemIndex + 1; end;
  checkScrollButtons(btnUp, btnDown, listBox);
end;

procedure TCustomMenu.btnDownMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  btnDownClick(NIL);
  sleep(30);
end;

procedure TCustomMenu.btnUpClick(Sender: TObject);
begin
  shutSubMenu;
  case listBox.topIndex   > 0 of TRUE:  listBox.topIndex  := listBox.topIndex   - 1; end;
  case listBox.itemIndex  > 0 of TRUE:  listBox.itemIndex := listBox.itemIndex  - 1; end;
  checkScrollButtons(btnUp, btnDown, listBox);
end;

procedure TCustomMenu.btnUpMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  btnUpClick(NIL);
  sleep(30);
end;

function TCustomMenu.shutMenus: boolean;
// close all submenus; hide the main appwindow menu
begin try
  shutSubMenu;            // cascades down into all subMenus of subMenus via shutForm
  SetWindowPos(Handle, HWND_BOTTOM, 0, 0, 0, 0, SWP_HIDEWINDOW OR SWP_NOMOVE OR SWP_NOSIZE);
  enableConfigForm; // for when the user has used the "Show Menu" button on the config form and then closed the menus
  except debug('exception in shutMenus'); end;
end;
//\\========== CLOSING MENUS AND SUBMENUS ==========\\


function TCustomMenu.buildMenu(menuType: TMenuType; extraInfo: array of string): boolean;
begin
  result := TRUE;
  // populate itemData from the appropriate source
  case menuType of
    mtTBD: ;
    mtINI:    result := buildMenuFromINI(extraInfo[0], itemData, extraInfo[1]);        // was passed the path of the INI file
    mtBrowse: buildMenuFromFiles(extraInfo[0], extraInfo[1], itemData, imageList1); // was passed the path and a list of file masks
  end;

  case result of FALSE: EXIT; end; // couldn't build a menu - currently only for buildMenuFromINI

  // use itemData to create the menu
  var defaultIcon: TIcon := TIcon.create;
  imageList2.GetIcon(IL2_UNIVERSAL, defaultIcon); // universal.ico
  try
    loadIcons(itemData, imageList1, defaultIcon);
    buildMenuFromItemData(itemData, listBox);
    prepareMenuForDisplay(self);
  finally
    defaultIcon.free;
  end;
end;

function TCustomMenu.configListBox: boolean;
begin
  listBox.Color       := CM_BACKGROUND_COLOR;
  listBox.Align       := alTop;
  listBox.BorderStyle := bsNone; // We draw a subtle border in WMNCPaint
  listBox.BevelInner  := bvNone;
  listBox.BevelOuter  := bvNone;
  listBox.BevelWidth  := 1;
  listBox.Font.Name   := 'Segoe UI';
  listBox.Font.Size   := 9;
  listbox.Style       := lbOwnerDrawVariable;
  FListBoxWndProc     := listBox.WindowProc;  // save old window proc
  listBox.WindowProc  := listBoxWndProc;      // subclass to suppress horizontal and vertical scrollbars - see listBoxWndProc
end;

var hProgman: HWND;
    hWorkerW: HWND;
    hDefView: HWND;
    hDesktop: HWND;

function findDefView(Handle: HWND; Temp: LongInt): BOOL; stdcall; // find DefView as a child of progman
begin
  hDefView := FindWindowEx(handle, 0, 'SHELLDLL_DefView', '');
  result   := hDefView = 0;  // TRUE = continue, FALSE = stop
end;

function findWorkerW(Handle: HWND; Temp: LongInt): BOOL; stdcall; // find DefView as a child of a WorkerW child window
begin
  hWorkerW := FindWindowEx(handle, 0, 'WorkerW', '');
  case hWorkerW <> 0 of TRUE: begin hDefView := findWindowEx(hWorkerW, 0, 'SHELLDLL_DefView', '');
                                    result   := hDefView = 0; end;end; // TRUE = continue, FALSE = stop
end;

function TCustomMenu.setMouseTrap: boolean;
begin
  case menuID <> 0 of TRUE: begin debug('Tried to create a mouse trap in a submenu'); EXIT; end;end;
  case FHook <> NIL of TRUE: begin debug('Tried to create a second mouse trap in the main menu'); EXIT; end;end;

  FHook := THookInstance<TLowLevelMouseHook>.CreateHook(self);
  FHook.OnPreExecute := procedure(Hook: THook; var HookMsg: THookMessage)
    var
      LLMouseHook: TLowLevelMouseHook;
      isDesktop: boolean;
      isRButton: boolean;
      isRButtonUp: boolean;
      isMenuWnd: boolean;
      isButtonClick: boolean;
      mouseWnd: HWND;
    begin
      HookMsg.Result := 0;                                                                              // default to not handling other windows' mouses. It's just rude!

      case FCtrlClickActivate and NOT (GetKeyState(VK_CONTROL) < 0) of TRUE: EXIT; end;
      case NOT FCtrlClickActivate and (GetKeyState(VK_CONTROL) < 0) of TRUE: EXIT; end;
      LLMouseHook := TLowLevelMouseHook(Hook);

      case (HookMsg.msg = WM_MOUSEMOVE) or (HookMsg.Msg = WM_MOUSEWHEEL) of TRUE: EXIT; end;            // only interested in clicks
      case LLMouseHook.HookStruct.Pt.Y > screen.WorkAreaRect.Bottom of TRUE: EXIT; end;                 // the mouse is over the taskbar/system tray
                                                                                                        // if we add more menu options to the systray icon, this will need to be revisited.

      mouseWnd      := WindowFromPoint(LLMouseHook.HookStruct.Pt);                                      // get the window this mouse message is for

      isDesktop     := (mouseWnd = hDesktop) or (mouseWnd = hDefView);                                  // is it the desktop?
      isMenuWnd     := menuWnd(MouseWnd);                                                               // is it one of our menus?

      // ignore mouse wheel and middle button clicks for now. We're only interested in left and right button clicks and releases.
      isButtonClick := (HookMsg.msg = WM_LBUTTONDOWN) or (HookMsg.msg = WM_LBUTTONUP) or (HookMsg.msg = WM_RBUTTONDOWN) or (HookMsg.msg = WM_RBUTTONUP);

      case isButtonClick and NOT isMenuWnd of TRUE: shutMenus; end;                                     // if it's not for an existing menu, any click closes all existing menus (Mouse movement is ok, but not clicks)
      case isDesktop or isMenuWnd of FALSE: EXIT; end;                                                  // Is mouse click for the desktop or one of our menus? If not, ignore it.

      FPT :=  LLMouseHook.HookStruct.Pt;                                                                // screen coords of mouse click
      isRButton   := (HookMsg.msg = WM_RBUTTONDOWN) OR (HookMsg.msg = WM_RBUTTONUP);                    // click or release of right mouse button?
      isRButtonUp := (HookMsg.msg = WM_RBUTTONUP);                                                      // release of right mouse button?
      case isDesktop and isRButton of TRUE: HookMsg.Result := 1; end;                                   // trap every click and release of the right mouse button on the desktop
      case isDesktop and isRButtonUp and (GetKeyState(VK_SHIFT) < 0) of TRUE: closeApp; end;            // SHIFT-rightclick on desktop closes this app
      case isDesktop and isRButtonUp and (GetKeyState(VK_MENU)  < 0) of TRUE: GREFRESH := TRUE; end;    // ALT-rightclick on desktop refreshes all menu data when the next statement executes
      case isDesktop and isRButtonUp of TRUE: MenuTimer.Enabled := TRUE; end;                           // right-click release on desktop, show main menu
    end;

    hProgman := FindWindow('Progman', 'Program Manager');
    hDefView := FindWindowEx(hProgman, 0, 'SHELLDLL_DefView', '');
    case hDefView = 0 of TRUE: enumWindows(@findDefView, 0); end;
    case hDefView = 0 of TRUE: enumWindows(@findWorkerW, 0); end;
    case hDefView = 0 of TRUE: raise exception.create('Can''t find the DefView window - please raise an issue on Github'); end;
    hDesktop := FindWindowEx(hDefView, 0, 'SysListView32', 'FolderView');

//    debugInteger('hProgman', hProgman);
//    debugInteger('hDefView', hDefView);
//    debugInteger('hWorkerW', hWorkerW);
//    debugInteger('hDesktop', hDesktop);

    FHook.Active := TRUE;
end;

//========== VCL Event Handlers ===========
procedure TCustomMenu.CreateParams(var Params: TCreateParams);
// No taskbar icon for the app, nor for any subMenus.
// For the main form, (NOT WS_EX_APPWINDOW) works in conjunction with
// "Application.MainFormOnTaskbar := TRUE" in the .dpr file, to ensure no icon gets created.
begin
  inherited;
  case GFIRST of TRUE: begin
    Params.ExStyle    := Params.ExStyle AND (NOT WS_EX_APPWINDOW);
    Params.WndParent  := Application.Handle;
  end;end;
end;

procedure TCustomMenu.menuExitClick(Sender: TObject);
begin
  closeApp;
end;

procedure TCustomMenu.FormActivate(Sender: TObject);
// the app starts with the main menu/app window hidden until the user right-clicks the desktop
begin
  case GFIRST of TRUE: SetWindowPos(handle, HWND_BOTTOM, 0, 0, 0, 0, SWP_HIDEWINDOW OR SWP_NOMOVE OR SWP_NOSIZE); end;
  case GFIRST of TRUE: setMouseTrap; end;
end;

procedure TCustomMenu.FormCreate(Sender: TObject);
begin
  left := -1000; position := poDesigned; // prevent the window from flashing momentarily on the desktop
  itemData  := TList<TItemData>.Create;
  MenuID    := newMenuID;
  case GFIRST of TRUE: ShowTrayIcon(trayIcon); end;
  FOldHintIx := -1;

  configListBox;

  hWNDs.add(bottomPanel.handle); // so the mouse hook can identify it as ours.

  case hasParamConfig of  TRUE: showConfigForm;
                         FALSE: case hasParamShowMenu of TRUE: begin Pt := getParamXY; buildAndShowTheMenu; end;end;end;

  case GFIRST of TRUE: checkHotkey(handle); end;
end;

procedure TCustomMenu.FormDestroy(Sender: TObject);
begin
  listBox.WindowProc := FListBoxWndProc; // restore window proc
  FListBoxWndProc    := NIL;
end;

procedure TCustomMenu.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_ESCAPE of TRUE: case menuID = 0 of  TRUE: shutMenus;
                                                   FALSE: begin shutForm; close; end;end;end;
end;

procedure TCustomMenu.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  case bottomPanel.visible of FALSE: EXIT; end;
  case listBox.topIndex = (listBox.items.count - 1) - 39 of TRUE: EXIT; end;
  btnDown.click;
end;

procedure TCustomMenu.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  case bottomPanel.visible of FALSE: EXIT; end;
  case listBox.topIndex = 0 of TRUE: EXIT; end;
  btnUp.click;
end;

procedure TCustomMenu.listBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  IMAGE_WIDTH       = 16;
  CHEVRON_MARGIN    = 4 + IMAGE_WIDTH;
  SUBMENU_RES       = 8 + 16 + 8 + 0 + 8 + 16; // margin + icon + margin + <available space> + margin + chevron
  LEFT_TEXT_MARGIN  = 8 + IMAGE_WIDTH + 8;
  NO_CHEVRON_RES    = SUBMENU_RES - 16;        // still want an 8-pixel margin on both the left and right of every menu item
  HORZ_MARGIN       = 8;
  VERT_MARGIN       = 4;
var
  vTextSpace: integer;
  vTextWidth: integer;
begin
  case listBox.Count  = -1 of TRUE: EXIT; end; // is this even possible?
  case index          = -1 of TRUE: EXIT; end; // is this even possible?
  case itemData.count =  0 of TRUE: EXIT; end;
  case index > imageList1.count - 1 of TRUE: begin debugFormat('index: %d, imageList1.count: %d', [index, imageList1.count]); debug('not enough icons in imageList'); EXIT; end;end;
  case index > itemData.count   - 1 of TRUE: begin debugFormat('index: %d, itemData.count: %d',   [index, itemData.count]);   debug('not enough items in itemData'); EXIT; end;end;

  var id: TItemData  := itemData[index];
  var vText: string := itemData[index].idName; // don't use listBox.items[index] as the text will have nobbled any unicode characters

  case id.idSubMenu of  TRUE: vTextSpace := rect.right - rect.left - SUBMENU_RES;
                       FALSE: vTextSpace := rect.right - rect.left - NO_CHEVRON_RES; end;

  vTextWidth := listBox.canvas.textWidth(vText);
  case vTextWidth <= vTextSpace of FALSE: begin
                                            vTextSpace := vTextSpace - GDotWidth;
                                            repeat setLength(vText, length(vText) - 2); // half as many calls to textWidth required with negligible difference to the text
                                            until listBox.canvas.textWidth(vText) <= vTextSpace;
                                            setLength(vText, length(vText) + 3);
                                            vText[length(vText)] := '.'; vText[length(vText) - 1] := '.'; vText[length(vText) - 2] := '.'; end;end;

  case odSelected in State of TRUE: begin
                                      listBox.Canvas.Brush.Color := CM_HIGHLIGHT_COLOR;
                                      listBox.Canvas.Font.Color  := clWhite; end;end;

  listbox.Canvas.FillRect(Rect);
  case id.idHasLUAShield of  TRUE: imageList2.Draw(listbox.Canvas, Rect.Left + HORZ_MARGIN, Rect.Top + VERT_MARGIN, 1); // draw LUAShield from imageList2
                            FALSE: imageList1.Draw(listbox.Canvas, Rect.Left + HORZ_MARGIN, Rect.Top + VERT_MARGIN, index); end;

  case id.idBrowseBlocked of  TRUE: listBox.canvas.font.color := clRed; // folder blocked in openSubMenu
                             FALSE: case (NOT id.idSubMenu) and (id.idCommand = 'browse') of TRUE: listBox.canvas.font.color := $8C8C8C; end;end; // empty folder identified in openSubMenu

  var vCenterText: integer := (Rect.Bottom - Rect.Top - listbox.Canvas.TextHeight(text)) div 2;
  listbox.Canvas.TextOut(Rect.left + LEFT_TEXT_MARGIN, Rect.Top + vCenterText, vText); // add additional 8-pixel space for a gap before and after the icon

  case id.idSubMenu of TRUE: imageList2.Draw(listbox.Canvas, Rect.Right - CHEVRON_MARGIN, Rect.Top + VERT_MARGIN, 0); end; // draw the transparent chevron icon

  case id.idSeparatorAfter of TRUE: begin
                                      listBox.Canvas.pen.Color := clGray;
                                      listBox.Canvas.MoveTo(Rect.Left + HORZ_MARGIN, Rect.Bottom - 1);
                                      listBox.Canvas.LineTo(Rect.Right - HORZ_MARGIN, Rect.Bottom - 1); end;end;

  if odFocused in State then listBox.Canvas.DrawFocusRect(Rect); // prevents the dotted box around a focussed item. It gets XOR-ed in the VCL's own call to DrawFocusRect.
end;

procedure TCustomMenu.listBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
// TListBox processes VK_RIGHT the same as VK_DOWN when there's only one column. Ridiculous!!
// It also processes VK_LEFT the same as VK_UP, but our handling of it here should be too quick to bother the user.
// TListBox also generates a MouseMove event after KeyUp when the new item is a subMenu and the mouse has been left hovering over another item,
//     even though the subMenu has the keyboard focus!
// Currently, the user can use the keyboard to navigate the menus but must ensure that they've moved the mouse out of the way.
begin
  case isWindowVisible(self.handle) of FALSE: begin key := 0; EXIT; end;end;

  var vKey := key;

  case listBox.itemIndex = -1 of TRUE: EXIT; end;
  var vIndex := listBox.itemIndex;
  case vIndex > itemData.count - 1 of TRUE: EXIT; end;
  var id: TItemData := itemData[vIndex];

  checkScrollButtons(btnUp, btnDown, listBox);

  case vKey in [VK_UP, VK_DOWN] of TRUE: shutSubMenu(id.idItemID); end; // always close any subMenu on key up/down...

  case (vKey = VK_RIGHT) and id.idSubMenu and (id.idCommand = 'browse') of
    TRUE: begin openSubMenu(self, vIndex, id.idItemID, point(listBox.left + listBox.width, listBox.top + ((vIndex - listBox.topIndex) * CM_ITEM_HEIGHT)), TRUE); end;end;

  case (id.idCommand <> 'browse') and              {(vKey in [VK_RETURN, VK_RIGHT]) <-- TListBox prevents us from doing this (better now we've subclassed TListBox)
  and} id.idSubMenu of TRUE: begin openSubMenu(self, vIndex, id.idItemID, point(listBox.left + listBox.width, listBox.top + ((vIndex - listBox.topIndex) * CM_ITEM_HEIGHT)), TRUE); end;end; // ...and always open the next subMenu

  case vKey = VK_LEFT of TRUE: begin
                                case menuID = 0 of  TRUE: shutMenus;
                                                   FALSE: begin shutForm; close; end;end;
                                EXIT; end;end;

  case vKey = VK_RETURN of TRUE:  begin
                                    case id.idCommand = '' of TRUE: EXIT; end;
                                    doShellExecute(id);
                                    mainMenu.shutMenus; end;end; // close all submenus; hide the main menu;
end;

procedure TCustomMenu.listBoxMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
// This procedure is actually redundant now that we don't draw separators as individual listbox items,
// and configListBox could set listBox.style to lbOwnerDrawFixed instead of lbOwnerDrawVariable
begin
  height := CM_ITEM_HEIGHT;
end;

procedure TCustomMenu.listBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case listBox.Count  = 0 of TRUE: EXIT; end;
  case itemData.count = 0 of TRUE: EXIT; end;

  var vIndex: integer := SendMessage(listBox.Handle, LB_ITEMFROMPOINT, 0, MakeLParam(X, Y));
  case vIndex = -1 of TRUE: EXIT; end;
  listBox.ItemIndex := vIndex; // otherwise the item isn't highlighted as you move the mouse up and down the menu

  // this can happen when the user clicks on a menu item and drags the mouse outside the bounds of the menu. It can be ignored.
  case vIndex > itemData.count - 1 of TRUE: begin {debug('A: vIndex > itemData.count - 1');} EXIT; end;end;

  var newY := (vIndex - listBox.topIndex) * CM_ITEM_HEIGHT;

  shutSubMenu(itemData[vIndex].idItemID);
  case itemData[vIndex].idSubMenu of TRUE: begin openSubMenu(self, vIndex, itemData[vIndex].idItemID, point(listBox.left + listBox.width, newY)); end;end;

  case vIndex <> FOldHintIx of TRUE: begin
                                      FOldHintIx    := vIndex;
                                      application.cancelHint; // allows a new hint to be displayed for each item.
                                      listbox.hint  := itemData[vIndex].idHint; end;end;
end;

procedure TCustomMenu.listBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  var vIndex: integer := SendMessage(listBox.Handle, LB_ITEMFROMPOINT, 0, MakeLParam(X, Y));
  case vIndex         = -1 of TRUE: EXIT; end;
  case itemData.count =  0 of TRUE: EXIT; end;
  case vIndex > itemData.count - 1 of TRUE: begin {debug('B: vIndex > itemData.count - 1');} EXIT; end;end; // it's a user mouse-drag issue which can be ignored

  listBox.ItemIndex := vIndex;
  var id: TItemData := itemData[vIndex];

  case button = mbLeft  of TRUE:  begin
                                    case id.idSubMenu of TRUE: begin openSubMenu(self, vIndex, id.idItemID, point(listBox.left + listBox.width, Y)); {EXIT}; end;end;
                                    case id.idCommand = '' of TRUE: EXIT; end;
                                    doShellExecute(id);
                                  end;end;

  case button = mbRight of TRUE:  begin
                                    mainMenu.shutMenus;
                                    case configFormOpen of  TRUE: enableConfigForm;
                                                           FALSE: showConfigForm; end;end;end;

  mainMenu.shutMenus; // close all submenus; hide the main menu;
end;

procedure TCustomMenu.listBoxWndProc(var Msg: TMessage);
begin
  ShowScrollBar(listBox.Handle, SB_HORZ, FALSE);
  ShowScrollBar(listBox.Handle, SB_VERT, FALSE);
  FListBoxWndProc(Msg); // process message
end;

procedure TCustomMenu.menuTimerTimer(Sender: TObject);
// We can't call buildAndShowTheMenu directly from the anonymouse (ha!) hook procedure in setMouseTrap.
// Instead, that procedure enables the timer (currently set to 10ms) and we immediately disable the timer
// each time it fires. The user re-enables it each time they RightClick or Alt-RightClick on the desktop.
// Shift-RightClick does an immediate closeApp and doesn't enable the timer.
// Ctrl-RightClick is ignored and therefore shows the Windows Desktop Context Menu without enabling the timer.
begin
  MenuTimer.Enabled := FALSE;
//  hookOff; // temporary, development only. Allows the app to be debugged in Delphi/RAD Studio once the hook has triggered this method.
             // Good luck trying to debug if you forget to hookOff! :D
             // Although, it's ok if your breakpoint is in FormConfig as the hook is disabled when that form is shown.
  buildAndShowTheMenu;
end;

procedure TCustomMenu.WMHotKey(var Msg: TWMHotKey);
begin
  FPT.X := (getscreenWidth - (width div 2)) div 2;
  FPT.Y := 20;
  menuTimer.enabled := hotkeyEnabled and (msg.HotKey = hotkeyAtom);
end;

procedure TCustomMenu.WMNCPaint(var Msg: TWMNCPaint);
// draw a subtle 1-pixel border around the window.
var
  dc: hDc;
  Pen: hPen;
  OldPen: hPen;
  OldBrush: hBrush;
begin
  inherited;
  dc := GetWindowDC(Handle);
  Msg.Result := 1;
  Pen := CreatePen(PS_SOLID, 1, RGB(140, 140, 140));
  OldPen := SelectObject(dc, Pen);
  OldBrush := SelectObject(dc, GetStockObject(NULL_BRUSH));
  Rectangle(dc, 0, 0, self.Width, self.Height);
  SelectObject(dc, OldBrush);
  SelectObject(dc, OldPen);
  DeleteObject(Pen);
  ReleaseDC(Handle, Canvas.Handle);
end;
//
//procedure TCustomMenu.WMXYMessage(var Msg: TMessage);
//var vPt: TPoint;
//begin
//  debug('hello');
//  vPt.X := msg.WParam;
//  vPt.Y := msg.LParam;
//  Pt := vPt;
//  buildAndShowTheMenu;
//end;

{ TListBox }

procedure TListBox.KeyDown(var Key: Word; Shift: TShiftState);
// prevent TListBox from interpreting LEFT and RIGHT keys as UP and DOWN keys.
begin
  case key = VK_RIGHT of TRUE: key := 0; end;
  case key = VK_LEFT  of TRUE: key := 0; end;
end;

initialization
  GFIRST    := TRUE;
  GREFRESH  := FALSE;
  hWNDs     := TList<HWND>.Create;

finalization
  case hWNDS    <> NIL of TRUE: begin hWNDS.Clear; FreeAndNil(hWNDs); end;end;
  case iniFile  <> NIL of TRUE: iniFile.free; end;

end.
