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
unit FormConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, System.Generics.Collections,

  CustomMenuCommon, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, WinAPI.CommCtrl,
  Vcl.Buttons, Vcl.Imaging.pngimage, Vcl.Menus, VirtualTrees, WinAPI.ActiveX,
  Vcl.Samples.Spin,

  Clipbrd, WinAPI.ShlObj;

type
  TConfigForm = class(TForm)
    imageList1: TImageList;
    imageList2: TImageList;
    backPanel: TPanel;
    treeMenu: TPopupMenu;
    treePanel: TPanel;
    buttonPanel: TPanel;
    btnMoveMenuItemUp: TSpeedButton;
    btnMoveMenuItemDown: TSpeedButton;
    btnDeleteMenuItem: TSpeedButton;
    btnAddMenuItem: TSpeedButton;
    btnMoveMenuItemLeft: TSpeedButton;
    lblDragAndDrop: TLabel;
    editPanel: TPanel;
    iconBevel: TBevel;
    btnSave: TButton;
    btnSavedChanges: TSpeedButton;
    btnSelectCommandDirectory: TButton;
    btnSelectCommandFile: TButton;
    btnSelectICOfile: TSpeedButton;
    btnSelectIconFromDLLExe: TSpeedButton;
    btnShowMenu: TButton;
    CloseBtn: TButton;
    comboRunType: TComboBox;
    comboCommandCategories: TComboBox;
    editCommand: TLabeledEdit;
    editDirectory: TLabeledEdit;
    editIconFile: TLabeledEdit;
    editIconIx: TSpinEdit;
    editName: TLabeledEdit;
    editParams: TLabeledEdit;
    editSubMenuName: TLabeledEdit;
    lblAppWindow: TLabel;
    lblDisabled: TLabel;
    lblDragAndDropCommandFile: TLabel;
    lblHasLUAshield: TLabel;
    lblIconGroup: TLabel;
    lblRunAsAdmin: TLabel;
    lblSelectFromExeDLLetc: TLabel;
    lblSelectICOfile: TLabel;
    lblSeparatorAfter: TLabel;
    LUAshieldIcon: TImage;
    menuIcon: TImage;
    topBevel: TBevel;
    bottomBevel: TBevel;
    vst: TVirtualStringTree;
    ImageList3: TImageList;
    checkboxPanel: TPanel;
    lblisSubMenu: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btnIsSubMenu: TBitBtn;
    btnSeparatorAfter: TBitBtn;
    btnDisabled: TBitBtn;
    btnHasLUAshield: TBitBtn;
    btnRunAsAdmin: TBitBtn;
    lblDragDropFolder: TLabel;
    lblDragDropIconFile: TLabel;
    FileOpenDialog: TFileOpenDialog;
    lblBrowseCommand: TLabel;
    lblBrowseDirectory: TLabel;
    popupMenu: TPopupMenu;
    btnCopyMenuItem: TSpeedButton;
    lblCommandList: TLabel;
    comboCommandList: TComboBox;
    editHint: TLabeledEdit;
    lblResizeTheWindow: TLabel;
    separatorBevel: TBevel;
    lblCommandCategories: TLabel;
    lblAutoFill: TLabel;
    btnSaveRegistry: TButton;
    btnSavedRegistry: TSpeedButton;
    lblWriteRegistry: TLabel;
    lblIconIx: TLabel;
    lblHelp: TLabel;
    lblFeedback: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure vstDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vstInitNode(Sender: TBaseVirtualTree; ParentNode,  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure btnAddMenuItemClick(Sender: TObject);
    procedure vstChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure editNameChange(Sender: TObject);
    procedure btnMoveMenuItemLeftClick(Sender: TObject);
    procedure btnMoveMenuItemUpClick(Sender: TObject);
    procedure btnMoveMenuItemDownClick(Sender: TObject);
    procedure vstKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnDeleteMenuItemClick(Sender: TObject);
    procedure vstDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure btnSelectCommandDirectoryClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure editCommandChange(Sender: TObject);
    procedure comboRunTypeChange(Sender: TObject);
    procedure editParamsChange(Sender: TObject);
    procedure editDirectoryChange(Sender: TObject);
    procedure editIconFileChange(Sender: TObject);
    procedure editIconIxChange(Sender: TObject);
    procedure btnSeparatorAfterClick(Sender: TObject);
    procedure btnDisabledClick(Sender: TObject);
    procedure btnHasLUAshieldClick(Sender: TObject);
    procedure btnRunAsAdminClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure WMDropFiles(var msg: TWMDropFiles); message WM_DROPFILES;
    procedure btnSelectCommandFileClick(Sender: TObject);
    procedure btnSelectICOfileClick(Sender: TObject);
    procedure btnSelectIconFromDLLExeClick(Sender: TObject);
    procedure btnShowMenuClick(Sender: TObject);
    procedure btnCopyMenuItemClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure comboCommandListSelect(Sender: TObject);
    procedure comboCommandCategoriesSelect(Sender: TObject);
    procedure btnSaveRegistryClick(Sender: TObject);
    procedure editHintChange(Sender: TObject);
    procedure lblHelpClick(Sender: TObject);
    procedure lblFeedbackClick(Sender: TObject);
  private
    itemData:       TList<TItemData>;
    FDropPoint:     TPoint;
    FImageIx:       integer;
    FInitialHeight: integer;
    FLastFolder:    string;
    FSavedTree:     TStringList;
    FTreeClick:     boolean;
    FDragDescriptionFormat: cardinal;
    function  actionThisIcon(iconFile: string; iconIx: integer): boolean;
    function  capitalize(const aString: string): string;
    function  checkIfStillSubMenu(aNode: PVirtualNode): boolean;
    function  checkNodeParentage: boolean;
    function  checkSaves: boolean;
    function  enableSaveButton(enabled: boolean): boolean;
    function  enableSaveRegistryButton(enabled: boolean): boolean;
    function  enableShowMenuButton(enabled: boolean): boolean;
    function  getCleanCaption: string;
    function  getDirtyCaption: string;
    function  getStdIconIx(iconIx: integer): integer;
    function  populateBoxesFromItemData(id: PItemData): boolean;
    function  populateBoxesFromCommand(cmdFilePath: string): boolean;
    function  populateBoxesFromLnk(lnkFilePath: string): boolean;
    function  populateCommandCategories: boolean;
    function  populateCommandList(commands: TArray<string>): boolean;
    procedure saveNode(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function  saveTree(vst: TVirtualStringTree): boolean;
    procedure saveNodeToRegistry(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function  saveToRegistry: boolean;
    function  saveTreeToRegistry(vst: TVirtualStringTree): boolean;
    function  selData: PItemData;
    function  setDragHint(DataObject: IDataObject; const Value: string; Effect: Integer): boolean;
    function  setWindowCaption: boolean;
    function  updateMenuIcon: boolean;

    //========== VCL Event Handlers ===========
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
  end;

function configFormOpen: boolean;
function showConfigForm: boolean;
function shutConfigForm: boolean;
function enableConfigForm: boolean;

implementation

uses _debugWindow, VirtualTrees.Types, WinAPI.ShellAPI, system.win.comobj, FormIconExplorer, FormCustomMenu, System.Win.Registry, winShell, runElevatedSupport,
     shellFoldersDef, mmcDef, mmcServerDef, cpl1Def, cpl2Def, runDll32Def, msSettingsDef, shellGuidsDef, system.strUtils;

var configForm: TConfigForm;
    FCurrentIx: integer = -1; // when loading all the data and the icons, FCurrentIx and all "for i" loop variables will match for all the
                              // indexes of itemData, imageList1 and imageList3.

function backupRegistryKey: boolean;
// rc doesn't actually tell us if the registry key was successfully exported, only that reg.exe was run.
var
  regFileName: string;
  regFileIx: integer;
begin
  regFileIx := 0;
  regFileName := getExePath + 'HKLM_desktopBackground.reg.bak';
  case fileExists(regFileName) of TRUE: repeat inc(regFileIx);
                                               regFileName := getExePath + format('HKLM_desktopBackground(%d).reg.bak', [regFileIx]);
                                        until  NOT fileExists(regFileName); end;
  var rc := shellExecute(0, 'open', 'reg.exe', PWideChar('EXPORT HKLM\' + CM_REGISTRY_KEY + ' ' + regFileName), PWideChar(getExePath), SW_HIDE);
  case rc > 32 of FALSE: rc := shellExecute(0, 'runas', 'reg.exe', PWideChar('EXPORT HKLM\' + CM_REGISTRY_KEY + ' ' + regFileName), PWideChar(getExePath), SW_HIDE); end;
  case rc > 32 of FALSE: showMessage('Unable to backup the registry key'#13#10'Save to registry aborted'); end;
  sleep(3000); // need to give reg.exe time to run and create the backup file. Not a particularly sound way of doing this.
  result := (rc > 32) and fileExists(regFileName); // well, we backed-up something!
end;

function buildTreeFromItemData(itemData: TList<TItemData>; vst: TVirtualStringTree): boolean;
// Everything is added as a child of subMenuHeader. The trick is to maintain the value
// of subMenuHeader correctly. When it's NIL, the new node gets added as a child of the
// root node. When we're in a submenu and the submenuName changes, crawl back up the
// levels until the matching subMenuHeader is located.
var
  newNode:            PVirtualNode;
  subMenuHeader:      PVirtualNode;
  subMenuHeaderData:  PItemData;

  function resetSubLevel: boolean;
  begin
    newNode := NIL; subMenuHeader := NIL;
  end;
begin
  case itemData.count = 0 of TRUE: EXIT; end;

  resetSubLevel;
  for var i: integer := 0 to itemData.Count - 1 do begin
    case (not itemData[i].idSubMenu) and (itemData[i].idSubMenuName = '') of TRUE: resetSubLevel; end; // it's a root level item: not a submenu itself and not a sub item

    case subMenuHeader <> NIL of TRUE: begin subMenuHeaderData := subMenuHeader.getData; end;end;

    if subMenuHeader <> NIL then
    while (subMenuHeader <> NIL) and (itemData[i].idSubMenuName <> subMenuHeaderData.idName) do begin subMenuHeader := subMenuHeader.parent; // the subMenuName doesn't match the subMenuHeader
                                                                                                      subMenuHeaderData := subMenuHeader.getData; end; // back-up until it does
    FCurrentIx := i;  // signal to initNode that it can populate its TItemData from TList<TitemData> - specifically itemData[FCurrentIx]

    newNode := vst.addChild(subMenuHeader);

    case itemData[i].idSubMenu of TRUE: subMenuHeader := newNode; end; // the newNode is now the subMenuHeader
  end;
  FCurrentIx := -1; // signal to initNode that from now on there's no pre-existing TItemData for new nodes
end;

function buttonChecked(button: TBitBtn): boolean;
// mimics "if checkBox.checked..."
begin
  result := button.imageIndex = IL2_CHECKED;
end;

function configFormOpen: boolean;
begin
  result := configForm <> NIL;
end;

function enableConfigForm: boolean;
begin
  case configForm = NIL of TRUE: EXIT; end;
  enableWindow(configForm.handle, TRUE);
  setWindowPos(configForm.handle, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;

function freeItemData(var itemData: TList<TItemData>): boolean;
begin
  case itemData = NIL of TRUE: EXIT; end;
  itemDataClear(itemData); itemData.clear; itemData.free; itemData := NIL;
end;

function loadMenuDataFromINI(iniFilePath: string; itemData: TList<TItemData>): boolean;
// populate itemData: TList<TItemData> from the INI file.
var
  i: integer;
  iniFile: TStringList;

  function iniClause(clauseName: string): string;
  var
    posClause:  integer;
    posEquals:  integer;
    posDelim:   integer;
  begin
    result := '';
    posClause := pos(lowerCase(clauseName) + '=', lowerCase(iniFile[i]));
    case posClause = 0 of TRUE: EXIT; end;
    posEquals := pos('=', iniFile[i], posClause);
    case posEquals = 0 of TRUE: EXIT; end;
    posDelim := pos(';', iniFile[i], posEquals);
    case posDelim = 0 of TRUE: EXIT; end;
    result := copy(iniFile[i], posEquals + 1, posDelim - posEquals - 1);
  end;

begin
  result := FALSE;
  case fileExists(iniFilePath) of FALSE: EXIT; end;
  iniFile := TStringlist.create;
  try
    iniFile.loadFromFile(iniFilePath);
    case iniFile.count = 0 of TRUE: EXIT; end;
    for i := 0 to iniFile.count - 1 do begin
      case trim(iniFile[i]) = '' of TRUE: CONTINUE; end; // ignore blank lines in ini file
      var id: TItemData;
      id.idName           := iniClause('name');
      id.idSubMenu        := lowerCase(iniClause('subMenu'))        = 'yes';
      id.idSubMenuName    := iniClause('subMenuName');
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
      id.idDisabled       := trim(iniFile[i])[1] = ':';

      itemData.Add(id);
      id := default(TItemData); // clear all the fields
    end;
  finally
    iniFile.free;
    result := TRUE;
  end;
end;

function reverseBtnChecked(sender: TObject): boolean;
// mimic a checkBox click
begin
  with sender as TBitBtn do
    case imageIndex of
      -1:           imageIndex := IL2_CHECKED;
      IL2_CHECKED:  imageIndex := -1;
    end;
end;

function setButtonChecked(button: TBitBtn; checked: boolean): boolean;
// mimic checkBox.checked := <whatever>
begin
  case checked of   TRUE: button.imageIndex := IL2_CHECKED;
                   FALSE: button.imageIndex := -1; end;
end;

function showConfigForm: boolean;
begin
  case configForm <> NIL of TRUE: EXIT; end; // it's already being shown; re-enable it.
  case configForm = NIL of TRUE: configForm := TconfigForm.create(NIL); end;
  enableHook(FALSE);
  try
    configForm.vst.RootNodeCount := 0;
//    case fileExists(getIniFileName) of FALSE: createMiniIni; end; // moved to .dpr file
    case loadMenuDataFromINI(getINIFileName, configForm.itemData) of FALSE: configForm.vst.AddChild(NIL); end; // give them something to get started.

    var defaultIcon: TIcon := TIcon.create;
    try
      configForm.imageList2.GetIcon(IL2_UNIVERSAL, defaultIcon); // get a copy of universal.ico - the default icon for menu items that don't have one
      loadIcons(configForm.itemData, configForm.imageList1, defaultIcon);     // imageList1 holds the 16x16 icons shown in the vst "menu"
      loadIcons(configForm.itemData, configForm.imageList3, defaultIcon, 32); // imageList3 is used to display 32x32 versions in the menuIcon: TImage;
    finally
      defaultIcon.free;
    end;
    buildTreeFromItemData(configForm.itemData, configForm.vst);
    freeItemData(configForm.itemData);   // with all the nodes initialized, we can no longer need the TList<TItemData>
    configForm.vst.fullExpand;
    configForm.btnSave.enabled          := FALSE;
    configForm.btnSavedChanges.visible  := FALSE;
    enableTrayExit(FALSE);               // force the user to close the config form before closing the app
    configForm.showModal;                // formClose is called when modalResult is set.
  finally
    enableHook;
    enableTrayExit(TRUE);
    GREFRESH    := TRUE;
    configForm  := NIL;                  // formClose does a caFree;
  end;
end;

function shutConfigForm: boolean; // force close from the EXIT tray icon menu item
begin
  case configForm <> NIL of TRUE:  begin
                                    configForm.close; // does a caFree;
                                    configForm := NIL; end;end;
end;


//========== FORM FUNCTIONS ==========
function TConfigForm.getCleanCaption: string;
begin
  result := getINIFileName + ' - ' + CM_APP_NAME + ' ' + getFileVersion('', 'v%d.%d.%d');
  case isRunningAsAdmin of TRUE: result := result + ' [Admin]'; end;
end;

function TConfigForm.getDirtyCaption: string;
begin
  result := '*' + getCleanCaption;
end;

function TConfigForm.getStdIconIx(iconIx: integer): integer;
// get a copy of one of the standard icons, e.g. IL2_UNIVERSAl
// add the copy to both image lists
// return the new index
begin
  var defaultIcon: TIcon := TIcon.create;
  try
    imageList2.GetIcon(iconIx, defaultIcon);      // get a copy of the required icon
    result := imageList1.AddIcon(defaultIcon);    // add it to the end of both image lists
    imageList3.AddIcon(defaultIcon);
  finally
    defaultIcon.free;
  end;
end;

procedure TConfigForm.lblFeedbackClick(Sender: TObject);
begin
  shellExecute(0, 'open', 'https://github.com/BazzaCuda/CustomMenu/discussions/', '', '', SW_SHOW);
end;

procedure TConfigForm.lblHelpClick(Sender: TObject);
begin
  shellExecute(0, 'open', 'https://github.com/BazzaCuda/CustomMenu/wiki/Getting-Started', '', '', SW_SHOW);
end;

function TConfigForm.populateBoxesFromCommand(cmdFilePath: string): boolean;
begin
  case trim(cmdFilePath) = '' of TRUE: EXIT; end;
  var vExt := lowerCase(extractFileExt(cmdFilePath));
  case (trim(editIconFile.text) = '') of TRUE: editIconFile.text := cmdFilePath; end;
  case (trim(editName.text) = '') or (editName.text = CM_NEW_ITEM_NAME) of TRUE: editName.text := capitalize(getFileNameWithoutExt(cmdFilePath)); end;
  case (trim(editDirectory.text) = '') of TRUE: editDirectory.text := includeTrailingBackslash(extractFilePath(cmdFilePath)); end;
  case editDirectory.text = '\' of TRUE: editDirectory.text := ''; end;
end;

function TConfigForm.populateBoxesFromItemData(id: PItemData): boolean;
// if the underlying data has changed, the UI must display the up-to-date info
begin
  editName.text             := id.idName;
  setButtonChecked(btnIsSubMenu, id.idSubMenu);
  editSubMenuName.text      := id.idSubMenuName;
  setButtonChecked(btnSeparatorAfter, id.idSeparatorAfter);
  setButtonChecked(btnHasLUAshield, id.idHasLUAShield);
  editIconFile.text         := id.idIconFile;
  editIconIx.value          := id.idIconIx;
  editCommand.text          := id.idCommand;
  editParams.text           := id.idParams;
  editDirectory.text        := id.idDirectory;
  comboRunType.ItemIndex    := comboRunType.items.indexOf(id.idRunType);
  editHint.text             := id.idHint;
  case comboRunType.itemIndex = -1 of TRUE: comboRunType.itemIndex := 0; end; // default to Normal
  setButtonChecked(btnRunAsAdmin, id.idRunAsAdmin);
  setButtonChecked(btnDisabled, id.idDisabled);
end;

function TConfigForm.populateBoxesFromLnk(lnkFilePath: string): boolean;
// set the boxes directly so that it triggers their respective change events and enables the save buton
var
  linkInfo: TShellLinkInfo;
begin
  getShellLinkInfo(lnkFilePath, linkInfo);

  case (trim(editName.text) = '') or (editName.text = CM_NEW_ITEM_NAME) of TRUE: editName.text := getFileNameWithoutExt(lnkFilePath); end;
  case (linkInfo.params  = '') of FALSE: begin editParams.text := linkInfo.params; end;end;

  case isShiftKeyDown of TRUE: begin    // get relative paths to everything;
    linkInfo.targetFile := extractRelativePath(getExePath, linkInfo.targetFile);
    linkInfo.workingDir := extractRelativePath(getExePath, linkInfo.workingDir);
    linkInfo.iconFile   := extractRelativePath(getExePath, linkInfo.iconFile); end;end;

  editCommand.text := linkInfo.targetFile;
  editDirectory.text := includeTrailingBackslash(linkInfo.workingDir);

  // if there's an icon file in the .lnk, use it. If not, and the user hasn't chosen an icon file yet, try the targetFile as the icon source
  case (linkInfo.iconFile = '') of FALSE: begin editIconFile.text := linkInfo.iconFile; editIconIx.value := linkInfo.iconIx; end;
                                    TRUE: case (trim(editIconFile.text) = '') of TRUE: editIconFile.text := linkInfo.targetFile; end;end;


  comboRunType.itemIndex := linkInfo.showCmd - 1; // doesn't trigger the onChange event
  comboRunTypeChange(NIL);                        // so we do it manually.
end;

function TConfigForm.populateCommandList(commands: TArray<string>): boolean;
begin
  for var i := 0 to high(commands) do
    comboCommandList.items.add(commands[i]);
end;

//========== Save Tree to INI ==========
var
  vTabs:                  string;
  vSubMenuHeader:         PVirtualNode;
  vSubMenuHeaderData:     PItemData;
  vSubMenu:               string;
  vSeparatorAfterSubMenu: boolean;

  function resetSubLevel: boolean;
  begin
    vTabs           := '';
    vSubMenuHeader  := NIL;
    vSubMenu        := '';
  end;

  function outstandingSeparator(savedTree: TStringList): boolean;
  begin
    case vSeparatorAfterSubMenu of TRUE:  begin
                                            savedTree.add('');
                                            vSeparatorAfterSubMenu := FALSE; end;end;
  end;

procedure TConfigForm.saveNode(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
// Writes out the INI file indenting the submenus and sub-submenus and adding the separators as blank lines where applicable.
// The logic is adapted from buildTreeFromItemData and I never want to have to write either of them again!
// It was more Zen than anything else :D
var
  id: PItemData;
  lineOut: string;
begin
  id := Sender.GetNodeData(Node);

  case (NOT id.idSubMenu) and (id.idSubMenuName = '') of TRUE:  begin // it's a root level item: not a submenu itself and not a sub item
                                                                  resetSubLevel;
                                                                  outstandingSeparator(FSavedTree); end;end;

  case vSubMenuHeader <> NIL of TRUE: begin vSubMenuHeaderData := vSubMenuHeader.getData; end;end;

  case vSubMenuHeader <> NIL of TRUE:
  while (vSubMenuHeader <> NIL) and (id.idSubMenuName <> vSubMenuHeaderData.idName) do begin  vSubMenuHeader      := vSubMenuHeader.parent;  // the subMenuName doesn't match the subMenuHeader
                                                                                              vSubMenuHeaderData  := vSubMenuHeader.getData; // back-up until it does
                                                                                              vSubMenu            := vSubMenuHeaderData.idName;
                                                                                              case length(vTabs) > 0 of TRUE: delete(vTabs, 1, 1); end;
                                                                                              outstandingSeparator(FSavedTree); end;end;

  case (id.idSubMenuName <> '') and (id.idSubMenuName <> vSubMenu) of TRUE: begin
                                                                              vSubMenu := id.idSubMenuName;
                                                                              vTabs := vTabs + #9; end;end;

  lineOut := vTabs;

  case id.idDisabled          of TRUE: lineOut := lineOut + ':'; end;

  case id.idName <> ''        of TRUE: lineOut := lineOut + format('name=%s;', [id.idName]); end;

  case id.idSubMenu           of TRUE: lineOut := lineOut + 'subMenu=yes;'; end;

  case id.idSubMenuName <> '' of TRUE: lineOut := lineOut + format('subMenuName=%s;', [id.idSubMenuName]); end;

  case id.idSeparatorAfter    of TRUE: lineOut := lineOut + 'separatorAfter=yes;'; end;

  case id.idHasLUAShield      of TRUE: lineOut := lineOut + 'hasLUAshield=yes;'; end;

  case id.idIconFile <> ''    of TRUE: lineOut := lineOut + format('icon=%s,%d;', [id.idIconFile, id.idIconIx]); end;

  case id.idCommand <> ''     of TRUE: lineOut := lineOut + format('command=%s;', [id.idCommand]); end;

  case id.idParams <> ''      of TRUE: lineOut := lineOut + format('params=%s;', [id.idParams]); end;

  case id.idDirectory <> ''   of TRUE: lineOut := lineOut + format('directory=%s;', [id.idDirectory]); end;

  case id.idRunType <> ''     of TRUE: lineOut := lineOut + format('runType=%s;', [id.idRunType]); end;

  case id.idHint <> ''        of TRUE: lineOut := lineOut + format('hint=%s;', [id.idHint]); end;

  case id.idRunAsAdmin        of TRUE: lineOut := lineOut + 'runAsAdmin=yes;'; end;

  FSavedTree.add(lineOut);

  case (id.idSubMenu) and (id.idCommand <> 'browse') of TRUE: vSubMenuHeader := node; end; // the newNode is now the subMenuHeader;

  case id.idSeparatorAfter of TRUE: case NOT id.idSubMenu of   TRUE: FSavedTree.add('');
                                                              FALSE: vSeparatorAfterSubMenu := TRUE; end;end;
end;

function TConfigForm.saveTree(vst: TVirtualStringTree): boolean;
begin
  FSavedTree              := TStringList.create;
  vSeparatorAfterSubMenu  := FALSE;
  resetSubLevel;
  try
    vst.iterateSubtree(NIL, saveNode, NIL);
  finally
    FSavedTree.saveToFile(getINIFileName);
    FSavedTree.free;
  end;
end;
//\\========== Save Tree to INI ==========\\

//========== Save Tree to Registry ==========
var
  reg: TRegistry;
  vKey: string;

  function resetRegSubLevel: boolean;
  begin
    vKey            := CM_REGISTRY_KEY;
    vSubMenuHeader  := NIL;
    vSubMenu        := '';
  end;

procedure TConfigForm.saveNodeToRegistry(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
// Writes to HKLM\SOFTWARE\Classes\DesktopBackground\Shell\
// The logic is adapted from saveNode and I never wanted to have to write another one again....ever!....and yet, here I am!
// Sometimes, an idea for new functionality is just too good to ignore :D
var
  id: PItemData;

  function menuIx(absIx: integer; reverse: boolean = TRUE): integer;
  // POSITION = TOP. Main menu items must be named in reverse alphanumeric order under the HKLM\SOFTWARE\Classes\DesktopBackground\Shell registry key
  //                 but sub-menu items have to be named in alphanumeric order. Thanks Microsoft!
  begin
    case reverse of  TRUE: result := (vst.totalCount - 1) - absIx;
                    FALSE: result := absIx; end;
  end;

  function rootItemKey(aKey: string): string;
  // vKey + 123_ + idName;
  begin
    result := format('%s\%.3d_%s', [aKey, menuIx(vst.absoluteIndex(node)), id.idName]);
  end;

  function subItemKey(aKey: string): string;
  // vKey + \Shell\ + 123_ + idName;
  begin
    result := format('%s\shell\%.3d_%s', [aKey, menuIx(vst.absoluteIndex(node), FALSE), id.idName]); // *** // POSITION = TOP, but submenu items have to be named in alphanumeric order!! // *** //
  end;

  function noShell: boolean;
  begin
    result := vKey.subString(length(vKey) - 6, 6) <> '\shell';
  end;

  function subMenuKey(aKey: string): string;
  // vKey + 123_ + idName;
  begin
    var vShell := '';
    case noShell of TRUE: vShell := '\shell'; end; // subMenu keys must have a \shell parent key
    result := format('%s%s\%.3d_%s', [aKey, vShell, menuIx(vst.absoluteIndex(node)), id.idName]);  {experimental: add \shell if required}
  end;

  function writeStdValues: boolean;
  begin
    reg.writeString('MUIVerb', id.idName);
    case id.idIconFile <> '' of TRUE: reg.writeString('icon', id.idIconFile + ',' + intToStr(id.idIconIx)); end;
    case id.idHasLUAShield of TRUE: reg.writeString('HasLUAshield', ''); end;
    case id.idSeparatorAfter of TRUE: reg.writeString('SeparatorAfter', ''); end;


    { // *** // POSITION = TOP means main menu items have to be named in reverse alphanumeric order in the registry,
                   otherwise the Windows desktop menu displays upsidedown!  // *** // }
    reg.writeString('Position', 'Top');

    // Make sure our menu items are separated from any items that Windows adds to the desktop context menu
    case vst.absoluteIndex(node) = 0 of TRUE: reg.writeString('SeparatorBefore', ''); end;
    case vst.absoluteIndex(node) = vst.TotalCount - 1 of TRUE: reg.writeString('SeparatorAfter', ''); end;
  end;

  function notBrowse: boolean;
  begin
    result := id.idCommand <> 'browse';
  end;

  function removeShell: boolean;
  begin
    case vKey.subString(length(vKey) - 6, 6) = '\shell' of TRUE: setLength(vKey, length(vKey) - 6); end;
  end;

begin
  id := Sender.GetNodeData(Node);
  case id.idDisabled of TRUE: EXIT; end;

  case (NOT id.idSubMenu) and (id.idSubMenuName = '') of TRUE: resetRegSubLevel; end;    // it's a root level item: not a submenu itself and not a sub item

  case vSubMenuHeader <> NIL of TRUE: begin vSubMenuHeaderData := vSubMenuHeader.getData; end;end;

  case vSubMenuHeader <> NIL of TRUE:
  while (vSubMenuHeader <> NIL) and (id.idSubMenuName <> vSubMenuHeaderData.idName) do  begin vSubMenuHeader      := vSubMenuHeader.parent;  // the subMenuName doesn't match the subMenuHeader
                                                                                              vSubMenuHeaderData  := vSubMenuHeader.getData; // back-up until it does
                                                                                              vSubMenu            := vSubMenuHeaderData.idName;
                                                                                              { this is why the const doesn't have the trailing \ }
                                                                                              case length(vKey) > length(CM_REGISTRY_KEY) of TRUE: delete(vKey, lastDelimiter('\', vKey), 255); end;
                                                                                              case length(vKey) > length(CM_REGISTRY_KEY) of TRUE: removeShell; end;
                                                                                              end;end;

  case (id.idSubMenuName <> '') and (id.idSubMenuName <> vSubMenu) of TRUE: begin vSubMenu := id.idSubMenuName;
                                                                                  var vShell := '';
                                                                                  case noShell of TRUE: vShell := '\shell'; end; // can't have a submenu key without a parent \shell key
                                                                                  { don't change the next statement; it's the key to everything! }
                                                                                  vKey := vKey + vShell + '\' + format('%.3d_', [menuIx(vst.absoluteIndex(node) - 1)]) + id.idSubMenuName; end;end;

  case id.idSubMenu of TRUE:  begin case reg.openKey(subMenuKey(vKey), TRUE) of TRUE: begin writeStdValues;
                                                                                            reg.writeString('SubCommands', '');
                                                                                            reg.closeKey; end;end;end;end;

  case (id.idSubMenuName <> '') and NOT id.idSubMenu of TRUE: begin
                                            reg.createKey(subItemKey(vKey));
                                            case reg.openKey(subItemKey(vKey), TRUE) of TRUE: begin writeStdValues;
                                                                                                    reg.closeKey; end;end;

                                            case reg.openKey(subItemKey(vKey) + '\Command\', TRUE) of TRUE: begin reg.writeString('', id.idCommand + ' ' + id.idParams);
                                                                                                                  reg.closeKey; end;end;end;end;


  case (id.idSubMenuName = '') and NOT id.idSubMenu of TRUE: begin // root level items which aren't subMenus
                                            reg.createKey(rootItemKey(vKey));
                                            case reg.openKey(rootItemKey(vKey), TRUE) of TRUE:  begin writeStdValues;
                                                                                                      reg.closeKey; end;end;

                                            case reg.openKey(rootItemKey(vKey) + '\Command\', TRUE) of TRUE:  begin reg.writeString('', id.idCommand + ' ' + id.idParams);
                                                                                                                    reg.closeKey; end;end;end;end;

  case (id.idSubMenu) and notBrowse of TRUE: vSubMenuHeader := node; end; // the newNode is now the subMenuHeader;  // browses aren't real submenu headers.
end;

function TConfigForm.saveTreeToRegistry(vst: TVirtualStringTree): boolean;
begin
  result := FALSE;
  reg := TRegistry.create(KEY_ALL_ACCESS);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    case reg.OpenKey(CM_REGISTRY_KEY, TRUE) of FALSE: begin showMessage('Unable to open the registry key for writing'); EXIT; end;end;
    reg.closeKey;
    {case} reg.deleteKey(CM_REGISTRY_KEY) {of FALSE: begin showMessage('Unable to delete the registry key'); EXIT; end;end};  // partial deletes may be good enough. It's for the user to test.
    resetSubLevel;
    vst.iterateSubtree(NIL, saveNodeToRegistry, NIL);
    result := TRUE;
  finally
    reg.closeKey;
    reg.free;
  end;
end;

function TConfigForm.saveToRegistry: boolean;
begin
  result := FALSE;
  screen.cursor := crHourGlass;
  try
    case backupRegistryKey of TRUE: result := saveTreeToRegistry(vst); end;
  finally
    screen.cursor := crDefault;
  end;
end;

//\\========== Save Tree to Registry ==========\\


function TConfigForm.updateMenuIcon: boolean;
// If the 32x32 icon to be displayed is different from the one currently displayed
// get it from imageList3 and update the menuIcon TImage
begin
  var sel: PVirtualNode := vst.GetFirstSelected;

  case sel = NIL of TRUE: EXIT; end;
  var id: PItemData := sel.getData;
  case id = NIL of TRUE: EXIT; end;
  case id.idImageIx = FImageIx of TRUE: EXIT; end; // stops the image from flashing everytime a node is selected.

  menuIcon.Picture.Icon := NIL;
  menuIcon.Refresh;
  imageList3.GetIcon(id.idImageIx, menuIcon.Picture.Icon); // update menuIcon
  menuIcon.Refresh;
  FImageIx := id.idImageIx;
end;

{$R *.dfm}

//========== VCL Event Handlers ===========
procedure TConfigForm.btnAddMenuItemClick(Sender: TObject);
var
  sel:     PVirtualNode;
  newNode: PVirtualNode;
begin
  case vst.TotalCount = 0 of  TRUE: sel := NIL;
                             FALSE: begin
                                      sel := vst.GetFirstSelected;
                                      case sel = NIL of TRUE: EXIT; end;end;end;

  newNode               := vst.InsertNode(sel, amInsertAfter);
  vst.selected[sel]     := FALSE;
  vst.Selected[newNode] := TRUE;
  vst.fullExpand;
  editName.setFocus;
  enableSaveButton(TRUE);
end;

procedure TConfigForm.btnDeleteMenuItemClick(Sender: TObject);
begin
  case vst.totalCount = 0 of TRUE: EXIT; end;
  var selText := vst.Text[vst.getFirstSelected, 0];
  var msg := 'Are you sure you want to delete this menu item:'#13#10'"' + selText + '"  ';
  case vst.getFirstSelected.childCount > 0 of TRUE: msg := msg + #13#10' AND all of its subMenu items'; end;
  msg := msg + '?';
  case vcl.dialogs.messageDlg(msg, mtWarning, [mbYes, mbNo], 0, mbNo) = mrYes of TRUE: vst.DeleteNode(vst.getFirstSelected); end;
  checkNodeParentage; // and populateBoxesFromItemData() and updateMenuIcon()
  enableSaveButton(TRUE);
end;

function TConfigForm.selData: PItemData;
begin
  case vst.getFirstSelected = NIL of  TRUE: result := NIL;
                                     FALSE: result := vst.getFirstSelected.getData; end;
end;

function TConfigForm.setDragHint(DataObject: IDataObject; const Value: string; Effect: Integer): boolean;
// https://stackoverflow.com/questions/47395267/how-to-change-drop-hint-delphi-application-when-doing-drag-drop-from-explorer
var
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
  Data: Pointer;
  Descr: DROPDESCRIPTION;
  s: WideString;
begin
  ZeroMemory(@Descr, SizeOf(DROPDESCRIPTION));
  {Do not set Descr.&type to DROPIMAGE_INVALID - this value ignore any custom hint}
  {use same image as dropeffect type}
  Descr.&type := DROPIMAGE_LABEL;
  case Effect of
    DROPEFFECT_NONE: Descr.&type := DROPIMAGE_NONE;
    DROPEFFECT_COPY: Descr.&type := DROPIMAGE_COPY;
    DROPEFFECT_MOVE: Descr.&type := DROPIMAGE_MOVE;
    DROPEFFECT_LINK: Descr.&type := DROPIMAGE_LINK;
  end;
  {format message for system}
  if Length(Value) <= MAX_PATH then
  begin
    Move(Value[1], Descr.szMessage[0], Length(Value) * SizeOf(WideChar));
    Descr.szInsert := '';
  end
  else
  begin
    s := Copy(Value, 1, MAX_PATH - 2) + '%1';
    Move(s[1], Descr.szMessage[0], Length(s) * SizeOf(WideChar));

    s := Copy(Value, MAX_PATH - 1, MAX_PATH);
    Move(s[1], Descr.szInsert[0], Length(s) * SizeOf(WideChar));
  end;
  {prepare structures to set DROPDESCRIPTION data}
  FormatEtc.cfFormat := FDragDescriptionFormat; {registered clipboard format}
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;

  ZeroMemory(@Medium, SizeOf(TStgMedium));
  Medium.tymed := TYMED_HGLOBAL;
  Medium.HGlobal := GlobalAlloc(GHND or GMEM_SHARE, SizeOf(DROPDESCRIPTION));
  Data := GlobalLock(Medium.HGlobal);
  Move(Descr, Data^, SizeOf(DROPDESCRIPTION));
  GlobalUnlock(Medium.HGlobal);

  DataObject.SetData(FormatEtc, Medium, True);
end;

function TConfigForm.setWindowCaption: boolean;
begin
  case btnSave.enabled of  TRUE: caption := getDirtyCaption;
                          FALSE: caption := getCleanCaption; end;
end;

procedure TConfigForm.btnCopyMenuItemClick(Sender: TObject);
begin
  case selData = NIL of TRUE: EXIT; end;
  var id1 := selData;
  var sel := vst.getFirstSelected;

  var newNode         := vst.InsertNode(sel, amInsertAfter);
  var id2: PItemData  := newNode.getData;

  id2.idName            := id1.idName;
  id2.idSubMenu         := id1.idSubMenu;
  id2.idSubMenuName     := id1.idSubMenuName;
  id2.idSeparatorAfter  := id1.idSeparatorAfter;
  id2.idHasLUAShield    := id1.idHasLUAShield;
  id2.idIconFile        := id1.idIconFile;
  id2.idIconIx          := id1.idIconIx;
  id2.idCommand         := id1.idCommand;
  id2.idParams          := id1.idParams;
  id2.idDirectory       := id1.idDirectory;
  id2.idRunType         := id1.idRunType;
  id2.idRunAsAdmin      := id1.idRunAsAdmin;
  id2.idHint            := id1.idHint;
  id2.idDisabled        := id1.idDisabled;
  id2.idImageIx         := id1.idImageIx;
  id2.idPrevImageIx     := id1.idPrevImageIx;

  vst.selected[sel]     := FALSE;
  vst.Selected[newNode] := TRUE;
  vst.fullExpand;
  editName.setFocus;
  enableSaveButton(TRUE);
end;

procedure TConfigForm.btnDisabledClick(Sender: TObject);
begin
  reverseBtnChecked(sender);
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  enableSaveButton(btnSave.enabled or (id.idDisabled <> buttonChecked(btnDisabled))); // has the value changed?
  id.idDisabled := buttonChecked(btnDisabled);
end;

procedure TConfigForm.btnHasLUAshieldClick(Sender: TObject);
// loadIcons() will try to load the icon specified in idIconFile/idIconix, or set the default IL2_UNIVERSAL icon.
// If idHasLUAShield, initNode() will override this icon, putting it in idPrevImageIx.
// As such, if the user unchecks btnHasLUAshield in the UI, there's a readymade icon in idPrevImageIx to take its place.
// Or, if the user changes it to another icon before then checking and unchecking btnHasLUAshield, that icon will be the
// one that gets restored.
begin
  reverseBtnChecked(sender);
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  enableSaveButton(btnSave.enabled or (id.idHasLUAShield <> buttonChecked(btnHasLUAshield))); // has the value changed?
  id.idHasLUAShield := buttonChecked(btnHasLUAshield);
  case id.idHasLUAShield of  TRUE:  begin
                                      id.idPrevImageIx  := id.idImageIx;                      // take a copy in case the user changes their mind
                                      id.idImageIx      := getStdIconIx(IL2_LUASHIELD); end;
                            FALSE:    id.idImageIx      := id.idPrevImageIx; end;             // restore the previously-used image
  vst.InvalidateNode(vst.getFirstSelected); // repaint the node's icon
  updateMenuIcon;                           // repaint menuIcon
end;

procedure TConfigForm.btnMoveMenuItemDownClick(Sender: TObject);
var nextNode: PVirtualNode;
begin
//  case vst.selectedCount = 0 of TRUE: EXIT; end;
  case vst.getFirstSelected = NIL of TRUE: EXIT; end;
  var node       := vst.getFirstSelected;
  var vOldParent := node.parent;

  case node.childCount > 0 of  TRUE: nextNode := vst.GetNextSibling(node);
                              FALSE: nextNode := vst.getNext(node, TRUE); end;


  case nextNode = NIL of TRUE:  begin
                                  nextNode := vst.GetNextSibling(node.parent);
                                  case nextNode = NIL of TRUE: EXIT; end; // end of the line
                                end;end;

  case nextNode.childCount > 0 of  TRUE: vst.moveTo(node, nextNode, amAddChildFirst, FALSE);
                                  FALSE: vst.moveTo(node, nextNode, amInsertAfter, FALSE); end;

  checkNodeParentage; // and populateBoxesFromItemData() and updateMenuIcon()
  checkIfStillSubMenu(vOldParent);
  enableSaveButton(TRUE);
end;

procedure TConfigForm.btnMoveMenuItemLeftClick(Sender: TObject);
var vNextNode: PVirtualNode;
begin
  case vst.getFirstSelected = NIL of TRUE: EXIT; end;
  var vNode := vst.getFirstSelected;

  var vParent := vNode.parent;
  case vParent = vst.RootNode of TRUE: EXIT; end;

  vNextNode := vst.getNextSibling(vParent);
  case vNextNode <> NIL of TRUE:  begin
                                    vst.moveTo(vNode, vNextNode, amInsertBefore, FALSE);
                                    EXIT; end;end;

  vNextNode := vParent.parent;

  vst.moveTo(vNode, vNextNode, amAddChildFirst, FALSE);

  checkNodeParentage; // and populateBoxesFromItemData() and updateMenuIcon()
  checkIfStillSubMenu(vParent);
  enableSaveButton(TRUE);
end;

procedure TConfigForm.btnMoveMenuItemUpClick(Sender: TObject);
begin
  case vst.getFirstSelected = NIL of TRUE: EXIT; end;
  var node      := vst.getFirstSelected;
  var vParent   := node.parent;
  var prevNode  := vst.getPrevious(node, TRUE);
  vst.moveTo(node, prevNode, amInsertBefore, FALSE);
  checkNodeParentage;  // and populateBoxesFromItemData() and updateMenuIcon()
  checkIfStillSubMenu(vParent);
  enableSaveButton(TRUE);
end;

procedure TConfigForm.btnRunAsAdminClick(Sender: TObject);
begin
  reverseBtnChecked(sender);
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  enableSaveButton(btnSave.enabled or (id.idRunAsAdmin <> buttonChecked(btnRunAsAdmin))); // has the value changed?
  id.idRunAsAdmin := buttonChecked(btnRunAsAdmin);
end;

procedure TConfigForm.btnSaveClick(Sender: TObject);
begin
  saveTree(vst);
  enableSaveButton(FALSE);
  enableSaveRegistryButton(TRUE); // once they've saved their changes, we let them save to the registry.
  enableShowMenuButton(vst.totalCount > 0);
end;

procedure TConfigForm.btnSaveRegistryClick(Sender: TObject);
begin
  var msg := 'Are you sure?'#13#10#13#10
           + 'This will replace the static part of the Windows Desktop right-click menu with your custom menu.'#13#10#13#10
           + 'However, it does not touch the actual ''context'' items such as "new" and "paste" etc. nor any other right-click context handlers you have registered; '
           + 'Windows will still add those to your custom menu, as appropriate.'#13#10#13#10
           + 'CustomMenu will create a .reg backup file of the DesktopBackground registry key before writing to it.'#13#10#13#10
           + 'Do you wish to continue?';

  case vcl.dialogs.messageDlg(msg, mtWarning, [mbYes, mbNo], 0, mbNo) = mrNo of TRUE: EXIT; end;

  enableSaveRegistryButton(NOT saveToRegistry); // if successful, we disable btnSaveRegistry and show btnSavedRegistry
end;

procedure TConfigForm.btnSelectCommandDirectoryClick(Sender: TObject);
var vDir: string;
begin
  vDir := FLastFolder;

  with FileOpenDialog do begin
    title         := 'Select Directory';
    options       := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem, fdoNoValidate];
    OKButtonLabel := 'Select';
    defaultFolder := vDir;
    fileName      := vDir;
    fileTypes.clear;
    case Execute of TRUE: begin
                            FLastFolder := fileName;  // be kind to the user
                            editDirectory.text := fileName; end;end;
  end;
end;

procedure TConfigForm.btnSelectCommandFileClick(Sender: TObject);
var vFile: string;
begin
  vFile := FLastFolder;

  with FileOpenDialog do begin
    title         := 'Select a Command File';
    options       := [fdoPathMustExist, fdoFileMustExist, fdoForceFileSystem, fdoNoValidate];
    OKButtonLabel := 'Select';
    defaultFolder := vFile;
    fileName      := vFile;

    fileTypes.clear;
    with fileTypes.Add do begin
      displayName := 'Exe Files';
      fileMask := '*.exe';
    end;

    with fileTypes.Add do begin
      displayName := 'Bat Files';
      fileMask := '*.bat';
    end;

    with fileTypes.Add do begin
      displayName := 'Shortcuts';
      fileMask := '*.lnk';
    end;

    with fileTypes.Add do begin
      displayName := 'All Files';
      fileMask := '*.*';
    end;

    fileTypeIndex := 0;

    case execute of TRUE: begin FLastFolder := ExtractFilePath(fileName); // be kind to the user
                                case lowerCase(extractFileExt(fileName)) = '.lnk' of  TRUE: populateBoxesFromLnk(fileName);
                                                                                     FALSE: begin editCommand.text := fileName; populateBoxesFromCommand(fileName); end;end;end;end;
  end;
end;

procedure TConfigForm.btnSelectICOfileClick(Sender: TObject);
var vFile: string;
begin
  vFile := FLastFolder;

  with FileOpenDialog do begin
    title         := 'Select an ICO File';
    options       := [fdoPathMustExist, fdoFileMustExist, fdoForceFileSystem, fdoNoValidate];
    OKButtonLabel := 'Select';
    defaultFolder := vFile;
    fileName      := vFile;

    fileTypes.clear;
    with fileTypes.Add do begin
      displayName := 'ICO Files';
      fileMask := '*.ico';
    end;

    fileTypeIndex := 0;

    case execute of TRUE: begin
                            FLastFolder := ExtractFilePath(fileName);  // kind to the user
                            actionThisIcon(fileName, 0); end;end;
  end;
end;

function TConfigForm.actionThisIcon(iconFile: string; iconIx: integer): boolean;
// load an icon or use the default if it can't be loaded
begin
  var id        := selData;
  id.idIconFile := iconFile;
  id.idIconIx   := iconIx;

  var defaultIcon: TIcon := TIcon.create;
  try
    configForm.imageList2.GetIcon(IL2_UNIVERSAL, defaultIcon); // get a copy of universal.ico - the default icon for menu items that don't have one
    case loadIcon(id.idIconFile, id.idIconIx, imageList1, 16) of FALSE: imageList1.AddIcon(defaultIcon); end;
    case loadIcon(id.idIconFile, id.idIconIx, imageList3, 32) of FALSE: imageList3.AddIcon(defaultIcon); end;
  finally
    defaultIcon.free;
  end;

  id.idImageIx := imageList1.count - 1; // the index of the newly-added image
  populateBoxesFromItemData(id);
  {case NOT FTreeClick of TRUE:} enableSaveButton(TRUE); {end;} // NB, this is not the same as enableSaveButton(NOT FTreeClick), because of btnSavedChanges
  vst.invalidate;
  updateMenuIcon;
end;

procedure TConfigForm.btnSelectIconFromDLLExeClick(Sender: TObject);
// if the user selects an icon in Icon Explorer, the full path to the icon file and the icon ix are returned
var
  vIconFile: string;
  vIconIx:   integer;
begin
  FormIconExplorer.GFilePath := extractFilePath(expandEnvs(editIconFile.text));
  enableWindow(handle, FALSE);
  try
    case showIconExplorer(vIconFile, vIconIx) of TRUE: actionThisIcon(vIconFile, vIconIx); end;
  finally
    enableWindow(handle, TRUE);
  end;
end;

procedure TConfigForm.btnSeparatorAfterClick(Sender: TObject);
begin
  reverseBtnChecked(sender);
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  enableSaveButton(btnSave.enabled or (id.idSeparatorAfter <> buttonChecked(btnSeparatorAfter))); // has the value changed?
  id.idSeparatorAfter := buttonChecked(btnSeparatorAfter);
end;

procedure TConfigForm.btnShowMenuClick(Sender: TObject);
var
  winRect: TRect;
  clientPt: TPoint;
begin
//  Can't do this - the config window is disabled.
//  case btnShowMenu.caption = 'Shut Menus' of TRUE: begin
//                                                     btnShowMenu.caption := 'Show Menu';
//                                                     mainMenu.shutMenus;
//                                                     EXIT; end;end;

  getWindowRect(handle, winRect);
  clientPt := clientToScreen(point(0, 0));
  var vTitleBarHeight := clientPt.Y - winRect.top;
  GREFRESH := TRUE;
  mainMenu.Pt := backPanel.ClientToScreen(point(backPanel.left + backPanel.width,
                                                backPanel.top - vTitleBarHeight)); // height of the title bar (NB SM_CYCAPTION is the height of the caption text!)
  enableWindow(handle, FALSE);
  mainMenu.menuTimerTimer(NIL);
  SetForegroundWindow(mainMenu.handle);
end;

function TConfigForm.checkSaves: boolean;
begin
  case btnSave.enabled of TRUE: begin
                                  var msg := 'You have unsaved changes.'#13#10'Do want to save them before closing?';
                                  case vcl.dialogs.messageDlg(msg, mtWarning, [mbYes, mbNo], 0, mbNo) = mrYes of TRUE: saveTree(vst); end;end;end;
  enableSaveButton(FALSE);
end;

procedure TConfigForm.CloseBtnClick(Sender: TObject);
begin
  checkSaves;
  modalResult := mrOK;
end;

function TConfigForm.populateCommandCategories: boolean;
begin
  with comboCommandCategories.items do begin
    add('Common Shell Folders');
    add('Management Consoles');
    add('Windows Server MMCs');
    add('Control Panel Applets');
    add('Other Control Panel Settings');
    add('RunDLL32 commands');
    add('MS Settings - Accounts');
    add('MS Settings - Apps');
    add('MS Settings - Control Center');
    add('MS Settings - Cortana');
    add('MS Settings - Devices');
    add('MS Settings - Ease of Access');
    add('MS Settings - Others');
    add('MS Settings - Gaming');
    add('MS Settings - Mixed Reality');
    add('MS Settings - Network & Internet');
    add('MS Settings - Personalization');
    add('MS Settings - Privacy');
    add('MS Settings - Search');
    add('MS Settings - Surface Hub');
    add('MS Settings - System');
    add('MS Settings - Time & Language');
    add('MS Settings - Update & Security');
    add('Shell Commands {GUIDs}');
  end;
end;

procedure TConfigForm.comboCommandCategoriesSelect(Sender: TObject);
begin
  comboCommandList.clear;
  case comboCommandCategories.itemIndex of
    0:  begin
          lblCommandList.caption := 'Common Shell Folders';
          populateCommandList(shellFoldersNameArray);
        end;
    1:  begin
          lblCommandList.caption := 'MMC Consoles';
          populateCommandList(mmcNameArray);
        end;
    2:  begin
          lblCommandList.caption := 'Windows Server MMCs';
          populateCommandList(mmcServerNameArray);
        end;
    3:  begin
          lblCommandList.caption := 'Control Panel Applets';
          populateCommandList(cpl1NameArray);
        end;
    4:  begin
          lblCommandList.caption := 'Other Control Panel Settings';
          populateCommandList(cpl2NameArray);
        end;
    5:  begin
          lblCommandList.caption := 'RunDLL32 commands';
          populateCommandList(runDll32NameArray);
        end;
    6:  begin
          lblCommandList.caption := 'MS Settings - Accounts';
          populateCommandList(ms1NameArray);
        end;
    7:  begin
          lblCommandList.caption := 'MS Settings - Apps';
          populateCommandList(ms2NameArray);
        end;
    8:  begin
          lblCommandList.caption := 'MS Settings - Control Center';
          populateCommandList(ms3NameArray);
        end;
    9:  begin
          lblCommandList.caption := 'MS Settings - Cortana';
          populateCommandList(ms4NameArray);
        end;
   10:  begin
          lblCommandList.caption := 'MS Settings - Devices';
          populateCommandList(ms5NameArray);
        end;
   11:  begin
          lblCommandList.caption := 'MS Settings - Ease of Access';
          populateCommandList(ms6NameArray);
        end;
   12:  begin
          lblCommandList.caption := 'MS Settings - Others';
          populateCommandList(ms7NameArray);
        end;
   13:  begin
          lblCommandList.caption := 'MS Settings - Gaming';
          populateCommandList(ms8NameArray);
        end;
   14:  begin
          lblCommandList.caption := 'MS Settings - Mixed Reality';
          populateCommandList(ms9NameArray);
        end;
   15:  begin
          lblCommandList.caption := 'MS Settings - Network & Internet';
          populateCommandList(ms10NameArray);
        end;
   16:  begin
          lblCommandList.caption := 'MS Settings - Personalization';
          populateCommandList(ms11NameArray);
        end;
   17:  begin
          lblCommandList.caption := 'MS Settings - Privacy';
          populateCommandList(ms12NameArray);
        end;
   18:  begin
          lblCommandList.caption := 'MS Settings - Search';
          populateCommandList(ms13NameArray);
        end;
   19:  begin
          lblCommandList.caption := 'MS Settings - Surface Hub';
          populateCommandList(ms14NameArray);
        end;
   20:  begin
          lblCommandList.caption := 'MS Settings - System';
          populateCommandList(ms15NameArray);
        end;
   21:  begin
          lblCommandList.caption := 'MS Settings - Time & Language';
          populateCommandList(ms16NameArray);
        end;
   22:  begin
          lblCommandList.caption := 'MS Settings - Update & Security';
          populateCommandList(ms17NameArray);
        end;
   23:  begin
          lblCommandList.caption := 'Shell Commands {GUIDs}';
          populateCommandList(shellGuidNameArray);
        end;
  end;
end;

procedure TConfigForm.comboCommandListSelect(Sender: TObject);
begin
  editName.text := comboCommandList.text;
  case comboCommandCategories.itemIndex of
    0:  begin
          editCommand.text  := 'explorer.exe';
          editParams.text   := shellFoldersCmdArray[comboCommandList.itemIndex]; end;
    1:  begin
          editCommand.text  := 'mmc.exe';
          editParams.text   := mmcCmdArray[comboCommandList.itemIndex]; end;
    2:  begin
          editCommand.text  := 'mmc.exe';
          editParams.text   := mmcServerCmdArray[comboCommandList.itemIndex]; end;
    3:  begin
          editCommand.text  := 'control';
          editParams.text   := cpl1CmdArray[comboCommandList.itemIndex]; end;
    4:  begin
          editCommand.text  := 'control';
          editParams.text   := cpl2CmdArray[comboCommandList.itemIndex]; end;
    5:  begin
          editCommand.text  := 'rundll32.exe';
          editParams.text   := runDll32CmdArray[comboCommandList.itemIndex]; end;
    6:  begin
//          comboRunType.itemIndex := 1; comboRunTypeChange(NIL); // so we do it manually.
//          editCommand.text  := 'cmd.exe'; // alternatively, e.g. "explorer ms-settings:backup"
//          editParams.text   := '/c start ' + ms1CmdArray[comboCommandList.itemIndex]; end;
          editCommand.text  := 'explorer';
          editParams.text   := ms1CmdArray[comboCommandList.itemIndex]; end;
    7:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms2CmdArray[comboCommandList.itemIndex]; end;
    8:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms3CmdArray[comboCommandList.itemIndex]; end;
    9:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms4CmdArray[comboCommandList.itemIndex]; end;
   10:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms5CmdArray[comboCommandList.itemIndex]; end;
   11:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms6CmdArray[comboCommandList.itemIndex]; end;
   12:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms7CmdArray[comboCommandList.itemIndex]; end;
   13:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms8CmdArray[comboCommandList.itemIndex]; end;
   14:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms9CmdArray[comboCommandList.itemIndex]; end;
   15:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms10CmdArray[comboCommandList.itemIndex]; end;
   16:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms11CmdArray[comboCommandList.itemIndex]; end;
   17:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms12CmdArray[comboCommandList.itemIndex]; end;
   18:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms13CmdArray[comboCommandList.itemIndex]; end;
   19:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms14CmdArray[comboCommandList.itemIndex]; end;
   20:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms15CmdArray[comboCommandList.itemIndex]; end;
   21:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms16CmdArray[comboCommandList.itemIndex]; end;
   22:  begin
          editCommand.text  := 'explorer';
          editParams.text   := ms17CmdArray[comboCommandList.itemIndex]; end;
   23:  begin
          editCommand.text  := 'explorer';
          editParams.text   := shellGuidCmdArray[comboCommandList.itemIndex]; end;
   end;
end;

procedure TConfigForm.comboRunTypeChange(Sender: TObject);
begin
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  enableSaveButton(btnSave.enabled or (id.idRunType <> comboRunType.text)); // has the value changed?
  id.idRunType := comboRunType.Text;
end;

function TConfigForm.enableSaveButton(enabled: boolean): boolean;
// don't allow a click on the tree and the boxes being populated to change the status of these two buttons
// they should only change due to genuine user-edits of the UI.
begin
  case FTreeClick of TRUE: EXIT; end;
  btnSave.enabled         := enabled;
  btnSavedChanges.visible := NOT enabled; // NB btnSavedChanges, the check mark
  enableShowMenuButton((NOT enabled)); // don't allow ShowMenu while there are edits waiting to be saved.
  setWindowCaption;
end;

function TConfigForm.enableSaveRegistryButton(enabled: boolean): boolean;
begin
  btnSaveRegistry.enabled  := enabled;
  btnSavedRegistry.visible := NOT enabled;  // NB btnSaved (not btnSave), i.e. the check mark
end;

function TConfigForm.enableShowMenuButton(enabled: boolean): boolean;
begin
  btnShowMenu.enabled := enabled;
end;

procedure TConfigForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  checkSaves;
  action := caFree;
  configForm := NIL;
end;

procedure TConfigForm.FormCreate(Sender: TObject);
begin
  WinAPI.Windows.setParent(handle, 0);
  setForegroundWindow(handle);
  dragAcceptFiles(handle, TRUE);

  caption := getCleanCaption;

  constraints.minWidth    := width; // the height [only] can be extended at runtime. Everything else is as designed.
  constraints.maxWidth    := width;
  constraints.minHeight   := height;
  color                   := CM_BACKGROUND_COLOR;
  backPanel.borderStyle   := bsNone;
  backPanel.bevelInner    := bvNone;
  backPanel.bevelOuter    := bvNone;
  backPanel.bevelKind     := bkNone;
  vst.color               := CM_BACKGROUND_COLOR;
  vst.align               := alLeft;
  vst.width               := 320;
  vst.dragMode            := TDragMode.dmAutomatic;
  vst.images              := imageList1;
  vst.borderStyle         := bsNone;
  vst.bevelInner          := bvNone;
  vst.bevelOuter          := bvNone;
  vst.bevelKind           := bkNone;
  treePanel.borderStyle   := bsNone;
  treePanel.bevelInner    := bvNone;
  treePanel.bevelOuter    := bvNone;
  treePanel.bevelKind     := bkNone;
  buttonPanel.borderStyle := bsNone;
  buttonPanel.bevelInner  := bvNone;
  buttonPanel.bevelOuter  := bvNone;
  buttonPanel.bevelKind   := bkNone;
  editPanel.borderStyle   := bsNone;
  editPanel.bevelInner    := bvNone;
  editPanel.bevelOuter    := bvNone;
  editPanel.bevelKind     := bkNone;
  editPanel.align         := alNone;
  font.name               := 'Segoe UI';
  font.size               := 10;
  backPanel.caption       := '';
  FInitialHeight          := height;
  FImageIx                := -1;
  lblResizeTheWindow.caption := lblResizeTheWindow.caption;

  vst.TreeOptions.selectionOptions  := vst.TreeOptions.selectionOptions + [toAlwaysSelectNode] - [toFullRowSelect];
  vst.treeOptions.PaintOptions      := vst.treeOptions.paintOptions - [toUseBlendedImages]; // fuzzy [selected] images fixed!

  populateCommandCategories;
  comboCommandCategories.itemIndex := 0;
  comboCommandCategoriesSelect(NIL);

  itemData                := TList<TItemData>.create;
  vst.NodeDataSize        := sizeOf(TItemData);

  FDragDescriptionFormat := RegisterClipboardFormat(PChar(CFSTR_DROPDESCRIPTION));
end;

procedure TConfigForm.FormDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  accept := TRUE;
end;

procedure TConfigForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_F1 of TRUE: shellExecute(0, 'open', 'https://github.com/BazzaCuda/CustomMenu/wiki', '', '', 0); end;
end;

procedure TConfigForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle OR WS_EX_APPWINDOW; // display an icon on the taskbar for this window
end;

procedure TConfigForm.editCommandChange(Sender: TObject);
begin
  editCommand.text := replaceStr(editCommand.text, '"', '');
  var sel: PVirtualNode := vst.getFirstSelected;
  case sel = NIL of TRUE: EXIT; end;
  var id: PItemData := sel.getData;
  enableSaveButton(btnSave.enabled or (id.idCommand <> editCommand.text)); // has the value changed?
  id.idCommand := editCommand.text;

  case lowerCase(trim(id.idCommand)) = 'browse' of  TRUE: begin
                                                            id.idCommand := 'browse'; // force lowercase
                                                            id.idSubMenu := TRUE;
                                                            populateBoxesFromItemData(id); end;
                                                   FALSE: populateBoxesFromCommand(id.idCommand); // {experimental: this will populate every blank directory - do we really want that?}
                                                          end;
end;

procedure TConfigForm.editDirectoryChange(Sender: TObject);
begin
  var sel: PVirtualNode := vst.getFirstSelected;
  case sel = NIL of TRUE: EXIT; end;
  var id: PItemData := sel.getData;
  enableSaveButton(btnSave.enabled or (id.idDirectory <> editDirectory.text)); // has the value changed?
  id.idDirectory := editDirectory.text;
end;

procedure TConfigForm.editHintChange(Sender: TObject);
begin
  var sel: PVirtualNode := vst.getFirstSelected;
  case sel = NIL of TRUE: EXIT; end;
  var id: PItemData := sel.getData;
  enableSaveButton(btnSave.enabled or (id.idHint <> editHint.text)); // has the value changed?
  id.idHint := editHint.text;
end;

procedure TConfigForm.editIconFileChange(Sender: TObject);
begin
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  enableSaveButton(btnSave.enabled or (id.idIconFile <> editIconFile.text)); // has the value changed?
  id.idIconFile := editIconFile.text;
  case NOT id.idHasLUAShield and fileExists(id.idIconFile) of TRUE: actionThisIcon(id.idIconFile, id.idIconIx); end;
end;

procedure TConfigForm.editIconIxChange(Sender: TObject);
begin
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  var vIx := editIconIx.value;
  enableSaveButton(btnSave.enabled or (id.idIconIx <> vIx)); // has the value changed?
  id.idIconIx := editIconix.value; // this is now a TSpinEdit, not a TLabeledEdit
  case NOT id.idHasLUAShield and fileExists(id.idIconFile) of TRUE: actionThisIcon(id.idIconFile, id.idIconIx); end;
end;

procedure TConfigForm.editNameChange(Sender: TObject);
begin
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  enableSaveButton(btnSave.enabled or (id.idName <> editName.text)); // has the value changed?
  id.idName := editName.text;
  vst.invalidateNode(vst.getFirstSelected);
end;

procedure TConfigForm.editParamsChange(Sender: TObject);
begin
  case selData = NIL of TRUE: EXIT; end;
  var id := selData;
  enableSaveButton(btnSave.enabled or (id.idParams <> editParams.text)); // has the value changed?
  id.idParams := editParams.text;
end;

procedure TConfigForm.FormResize(Sender: TObject);
// if the user resizes the window vertically, keep the editable controls centralized.
begin
  editPanel.top       := (backPanel.height - editPanel.height) div 2;
  topBevel.visible    := height > FInitialHeight;
  bottomBevel.visible := height > FInitialHeight;
end;

procedure TConfigForm.FormShow(Sender: TObject);
begin
  vst.setFocus;
end;

function TConfigForm.checkNodeParentage: boolean;
// check if the node still has child nodes
// check if the node is now a child node of another node
begin
  case selData = NIL of TRUE: EXIT; end;
  var id1 := selData;
  var sel: PVirtualNode := vst.getFirstSelected;
  id1.idSubMenu         := (sel.childCount > 0) or (id1.idCommand = 'browse');  // does it still have child nodes?
  case sel.parent <> NIL of TRUE: begin
                                    var id2: PItemData    := sel.parent.getData;
                                    id1.idSubMenuName     := id2.idName;     // take note of the node's new parent...
                                    id2.idSubMenu         := TRUE; end;end;  // which is now the proud parent of a child node

  {both of these calls should really be elsewhere}
  populateBoxesFromItemData(id1);
  updateMenuIcon;
end;

function TConfigForm.capitalize(const aString: string): string;
begin
  result := aString;
  case length(result) > 0 of FALSE: EXIT; end;
  result[1] := upCase(result[1]);
end;

function TConfigForm.checkIfStillSubMenu(aNode: PVirtualNode): boolean;
begin
  case aNode = NIL of TRUE: EXIT; end;
  var id: PItemData := aNode.getData;
  id.idSubMenu := (aNode.childCount > 0) or (id.idCommand = 'browse');
end;

procedure TConfigForm.vstChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
// if the user clicks on a node, the event sequence is vstChange, vstClick, vstNodeClick
begin
  FTreeClick := TRUE;
  try
    var vOldParent := node.parent;
    checkNodeParentage; // and populateBoxesFromItemData() and updateMenuIcon()
    checkIfStillSubMenu(vOldParent);
  finally
    FTreeClick := FALSE;
  end;
end;

procedure TConfigForm.vstDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  allowed := TRUE;
end;

procedure getFileListFromObj(const dataObj: IDataObject; fileList: TStringList);
var
  fmtEtc:           TFormatEtc; // specifies required data format
  medium:           TStgMedium; // storage medium containing file list
  droppedFileCount: integer;    // number of dropped files
  i:                integer;    // loops thru dropped files
  fileNameLength:   integer;    // length of a dropped file name
  fileName:         string;     // name of a dropped file
begin
  // Get required storage medium from data object
  fmtEtc.cfFormat := CF_HDROP;
  fmtEtc.ptd      := nil;
  fmtEtc.dwAspect := DVASPECT_CONTENT;
  fmtEtc.lindex   := -1;
  fmtEtc.tymed    := TYMED_HGLOBAL;
  oleCheck(dataObj.getData(fmtEtc, medium));
  try
    try
      droppedFileCount := dragQueryFile(medium.hGlobal, $FFFFFFFF, NIL, 0); // Get count of files dropped

      for i := 0 to pred(droppedFileCount) do begin // Get name of each file dropped and process it
          fileNameLength := dragQueryFile(medium.hGlobal, i, NIL, 0); // get the length of the file name...
          setLength(fileName, fileNameLength);
          dragQueryFileW(medium.hGlobal, i, PWideChar(fileName), fileNameLength + 1); // then name itself
          fileList.append(fileName); // add file name to list
        end;
    finally
      dragFinish(medium.hGlobal); // Tidy up - release the drop handle; don't use DropH again after this
    end;
  finally
    releaseStgMedium(medium);
  end;
end;

procedure TConfigForm.vstDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
                                  Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  attachMode: TVTNodeAttachMode;
  linkInfo:   TShellLinkInfo;
  vRelDirs:   boolean; // it might be a long operation so let the user accidentally let go of the SHIFT key
begin
  case mode of  // where is the insert line being displayed?
    dmAbove:    attachMode := amInsertBefore;
    dmBelow:    attachMode := amInsertAfter;
    dmOnNode:   attachMode := amAddChildLast; // was amAddChildFirst but they're in reverse alphabetical order!
    dmNowhere:  attachmode := amAddChildLast;
  end;

  vRelDirs := isShiftKeyDown;

  case source = NIL of TRUE: begin // a file was dropped from Explorer
    var vFiles := TStringList.create;
    try
      for var i := 0 to high(formats) - 1 do // find a format we know how to process
        case formats[i] = CF_HDROP of TRUE: begin
                                              GetFileListFromObj(DataObject, vFiles);
                                              for var j := 0 to vFiles.count - 1 do begin
                                                var newNode := sender.InsertNode(sender.dropTargetNode, attachMode);
                                                vst.selected[sender.getFirstSelected] := FALSE;
                                                vst.selected[newNode]                 := TRUE;
                                                var id: PItemData := newNode.getData;

                                                case lowerCase(extractFileExt(vFiles[j])) = '.lnk' of
                                                   TRUE:  begin
                                                            getShellLinkInfo(vFiles[j], linkInfo);
                                                            id.idName     := getFileNameWithoutExt(vFiles[j]);

                                                            case vRelDirs of TRUE:  begin
                                                                                      linkInfo.targetFile := extractRelativePath(getExePath, linkInfo.targetFile);
                                                                                      linkInfo.workingDir := extractRelativePath(getExePath, linkInfo.workingDir);
                                                                                      linkInfo.iconFile   := extractRelativePath(getExePath, linkInfo.iconFile); end;end;

                                                            id.idCommand  := linkInfo.targetFile;
                                                            case linkInfo.iconFile <> '' of  TRUE:  begin
                                                                                                      id.idIconFile := linkInfo.iconFile;
                                                                                                      id.idIconIx   := linkInfo.iconIx; end;
                                                                                            FALSE:  begin
                                                                                                      id.idIconFile := linkInfo.targetFile;
                                                                                                      id.idIconIx   := 0; end;end;
                                                          end;
                                                  FALSE:  begin
                                                            id.idName     := getFileNameWithoutExt(vFiles[j]);

                                                            case vRelDirs of TRUE: vFiles[j] := extractRelativePath(getExePath, vFiles[j]); end;

                                                            id.idCommand  := vFiles[j];
                                                            id.idIconFile := vFiles[j];
                                                            id.idIconIx   := 0; end;end;

                                                actionThisIcon(id.idIconFile, id.idIconIx);

                                                checkNodeParentage; // and populateBoxesFromItemData() and updateMenuIcon()
                                                enableSaveButton(TRUE);
                                              end;end;end;

    finally
      vFiles.free;
    end;
    EXIT;
  end;end;

  var vSource := Sender.getFirstSelected;
  var vParent := vSource.parent;
  var vTarget := Sender.DropTargetNode;

  case mode of  // where is the insert line being displayed?
    dmAbove:    attachMode := amInsertBefore;
    dmBelow:    attachMode := amInsertAfter;
    dmOnNode:   attachMode := amAddChildFirst;
    dmNowhere:  EXIT;
  end;

  sender.moveTo(vSource, vTarget, attachMode, FALSE);
  sender.fullExpand;

  checkNodeParentage; // and populateBoxesFromItemData() and updateMenuIcon()
  checkIfStillSubMenu(vParent);
  enableSaveButton(TRUE);
end;

procedure TConfigForm.vstDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  accept := TRUE; // vst is an equal opportunities drop target
  effect := DROPEFFECT_LINK; {experimental - does this suppress the copy/move hint?}

  case ssShift in shift of  TRUE: SetDragHint(vst.DragManager.DataObject, 'Create with relative paths', Effect);
                           FALSE: SetDragHint(vst.DragManager.DataObject, 'Create with absolute paths', Effect); end;

  setForegroundWindow(handle);
end;

procedure TConfigForm.vstFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  case node = NIL of TRUE: EXIT; end;
  var id: PItemData := node.getData;
  id^ := default(TItemData);
end;

procedure TConfigForm.vstGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  case node = NIL of TRUE: EXIT; end;
  case kind = ikState of TRUE: EXIT; end;
  var id: PItemData := node.getData;
  case id = NIL of TRUE: EXIT; end;
  imageIndex := id.idImageIx;
  updateMenuIcon;
end;

procedure TConfigForm.vstGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
begin
  var id: PItemData := PItemData(node.getData);   // IT WORKS. DO NOT CHANGE IT !!!!
  cellText          := id.idName;
end;

procedure TConfigForm.vstInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  case node = NIL of TRUE: EXIT; end;
  var id: PItemData := node.getData;    // get the pointer to the node data
  case id = NIL of TRUE: EXIT; end;
  case FCurrentIx <> -1 of  TRUE: begin // the TItemData record was created from the ini file and this node has just been created in buildTreeFromItemData()
                                                    var td: TItemData   := itemData[FCurrentIx]; // get the original TItemData temporarily
                                                    id.idName           := td.idName;            // copy the values to the vst's ItemData
                                                    id.idSubMenu        := td.idSubMenu;
                                                    id.idSubMenuName    := td.idSubMenuName;
                                                    id.idSeparatorAfter := td.idSeparatorAfter;
                                                    id.idHasLUAShield   := td.idHasLUAShield;
                                                    id.idIconFile       := td.idIconFile;
                                                    id.idIconIx         := td.idIconIx;
                                                    id.idCommand        := td.idCommand;
                                                    id.idParams         := td.idParams;
                                                    id.idDirectory      := td.idDirectory;
                                                    id.idRunType        := td.idRunType;
                                                    id.idRunAsAdmin     := td.idRunAsAdmin;
                                                    id.idHint           := td.idHint;
                                                    id.idDisabled       := td.idDisabled;
                                                    case td.idHasLUAShield of  TRUE: id.idImageIx := getStdIconIx(IL2_LUASHIELD); // overrides any idIconFile loaded in loadIcons()
                                                                              FALSE: id.idImageIx := FCurrentIx; end;
                                                    id.idPrevImageIx    := FCurrentIx; // should have been assigned the universal icon during loadIcons() in showConfigForm()
                                                  end;
                                           FALSE: begin // the TItemData in the node data was created during this config session
                                                    id.idName           := CM_NEW_ITEM_NAME;
                                                    id.idImageIx        := getStdIconIx(IL2_UNIVERSAL);
                                                  end;end;
end;

procedure TConfigForm.vstKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case (key = VK_RIGHT) or (key = VK_LEFT) of TRUE: key := 0; end; // prevent the user from triggering the horizontal scrollbar
  case (key = VK_DELETE) of TRUE: btnDeleteMenuItemClick(NIL); end;
end;

procedure TConfigForm.WMDropFiles(var msg: TWMDropFiles);
// use the drop point to determine generally which control has been dropped on.
// use the control's tag to identify it specifically.
var
  vFilePath: string;
begin
  inherited;

  getCursorPos(FDropPoint);

  var hDrop := msg.drop;
  try
    var droppedFileCount := dragQueryFile(hDrop, $FFFFFFFF, NIL, 0);
    for var i := 0 to pred(DroppedFileCount) do begin
      var fileNameLength := dragQueryFile(hDrop, i, NIL, 0);
      setLength(vFilePath, fileNameLength);
      dragQueryFile(hDrop, i, PChar(vFilePath), fileNameLength + 1);
      BREAK;              // we currently only process the first file if multiple files are dropped
    end;
  finally
    dragFinish(hDrop);
  end;
  msg.result := 0;

  case isShiftKeyDown of  TRUE: vFilePath := extractRelativePath(getExePath, trim(vFilePath));
                         FALSE: vFilePath := trim(vFilePath); end;

  var vDropControl  := ControlAtPos(screenToClient(FDropPoint), TRUE, TRUE, TRUE);
  case vDropControl  = NIL of  TRUE:  {debug('vDropControl = NIL')};
                              FALSE:  begin
                                        case (vDropControl.tag = 1) and (lowerCase(extractFileExt(vFilePath)) = '.lnk') of TRUE: begin populateBoxesFromLnk(vFilePath); EXIT; end;end;
                                        case vFilePath <> '' of TRUE: case vDropControl.tag of
                                                                        1: editCommand.text   := vFilePath;
                                                                        2: editDirectory.text := includeTrailingBackslash(vFilePath);
                                                                        3: editIconFile.text  := vFilePath;
                                                                      end;end;
                                        case (lowerCase(extractFileExt(vFilePath)) = '.exe') and (vDropControl.tag = 1) of TRUE: populateBoxesFromCommand(vFilePath); end;
                                      end;end;
end;

initialization

end.
