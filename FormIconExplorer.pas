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
unit FormIconExplorer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, Buttons, ExtCtrls, ActnList, ComCtrls, ShlObj,
  MPShellTypes, MPCommonObjects, MPCommonUtilities,
  VirtualTrees, VirtualExplorerTree, EasyListview, VirtualExplorerEasyListview,
  MPShellUtilities, VirtualShellNotifier, VirtualThumbnails,
  ActiveX, MPDataObject, System.Actions, Vcl.FileCtrl, System.ImageList,
  Vcl.ImgList, Vcl.ToolWin;

type
  TIconExplorerForm = class(TForm)
    LV: TVirtualExplorerEasyListview;
    addressPanel: TPanel;
    addressLabel: TLabel;
    treePanel: TPanel;
    Tree: TVirtualExplorerTree;
    backPanel: TPanel;
    listPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    iconPanel: TPanel;
    iconControlsPanel: TPanel;
    IconView: TListView;
    bottomBorderPanel: TPanel;
    filterPanel: TPanel;
    filterCombo: TComboBox;
    iconList: TImageList;
    iconImage: TImage;
    toolbar: TToolBar;
    BtnSave: TToolButton;
    BtnSelect: TToolButton;
    toolbarImageList: TImageList;
    Panel1: TPanel;
    iconLabelPanel: TPanel;
    iconLabel: TLabel;
    ToolBar1: TToolBar;
    btnCancel: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure LVColumnPaintText(Sender: TCustomEasyListview; Column: TEasyColumn; ACanvas: TCanvas);
    procedure filterComboKeyPress(Sender: TObject; var Key: Char);
    procedure filterComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure filterComboChange(Sender: TObject);
    procedure LVItemClick(Sender: TCustomEasyListview; Item: TEasyItem; KeyStates: TCommonKeyStates; HitInfo: TEasyItemHitTestInfoSet);
    procedure IconViewClick(Sender: TObject);
    procedure LVItemSelectionChanged(Sender: TCustomEasyListview; Item: TEasyItem);
    procedure IconViewResize(Sender: TObject);
    procedure BtnSelectClick(Sender: TObject);
    procedure IconViewDblClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    NumberOfIcons: LongInt;

    FBackgroundColor: integer;
    FHighlightColor:  integer;
    FInfoColor:       integer;

    function  applyFilter(const bShowAll: boolean = FALSE): boolean;
    procedure iconViewLoadIcons(IFName: string);
    function  setColors: boolean;
    function  updateAddress(const aCaption: string): boolean;
  public
    IconFName, IconFExt: string;
    IsBmp, IsIco: Boolean;
    IcoColors, BmpHeight, BmpWidth: integer;
  end;

function showIconExplorer(const aStartingFilePath: string; var vSelectedFilePath: string; var vIconIx: integer): boolean;
function showingIconExplorer: boolean;
function focusIconExplorer: boolean;

implementation

uses
  WinAPI.ShellAPI, _debugWindow, WinAPI.CommCtrl,
  customMenuCommon;

var
  GFilePath:        string;
  GIconIx:          integer;
  GHWND:            integer;

{$R *.dfm}

function showingIconExplorer: boolean;
begin
  result := GHWND <> -1;
end;

function focusIconExplorer: boolean;
begin
  setForegroundWindow(GHWND);
end;

function showIconExplorer(const aStartingFilePath: string; var vSelectedFilePath: string; var vIconIx: integer): boolean;
var vMr: TModalResult;
begin
  GFilePath := aStartingFilePath;
  with TIconExplorerForm.create(NIL) do begin
  try
    GHWND := handle;
    vMr   := showModal;

    case vMr = mrOK of TRUE:  begin
                                vSelectedFilePath := GFilePath;
                                vIconIx           := GIconIx; end;end;

    result := vMr = mrOK;
  finally
    free;
    GHWND := -1;
  end;end;
end;

function TIconExplorerForm.applyFilter(const bShowAll: boolean = FALSE): boolean;
begin
try
  try
    for var i := 0 to lv.ItemCount - 1 do begin
      var vExt := trim(lowercase(extractFileExt(lv.items[i].caption)));
      lv.items[i].visible := (
                                bShowAll
                                or (vExt = '') // assume it's most likely a folder
                                or (filterCombo.items[filterCombo.itemIndex].contains('*.*'))
                                or (filterCombo.items[filterCombo.itemIndex].contains(vExt))
                             );
      lv.items[i].Invalidate(TRUE);
    end;
    lv.RereadAndRefresh(FALSE);
  finally
  end;
except end;
end;

procedure TIconExplorerForm.IconViewDblClick(Sender: TObject);
begin
  GFilePath   := lv.selectedPath;
  GIconIx     := iconView.selected.imageIndex;
  modalResult := mrOK;
end;

procedure TIconExplorerForm.btnCancelClick(Sender: TObject);
begin
  modalResult := mrCancel;
end;

procedure TIconExplorerForm.BtnSaveClick(Sender: TObject);
begin
// maybe. future implementation...maybe.
end;

procedure TIconExplorerForm.BtnSelectClick(Sender: TObject);
begin
  case IconView.Selected = NIL of TRUE: EXIT; end;
  GFilePath   := lv.selectedPath;
  GIconIx     := iconView.selected.imageIndex;
  modalResult := mrOK;
end;

// Retrieves icons from selected file. then loads them into IconView
procedure TIconExplorerForm.IconViewClick(Sender: TObject);
var
  pFName: array[0..255] of Char;
begin
  case IconView.Selected = NIL of TRUE: EXIT; end;
  case FileExists(IconFName) of FALSE: EXIT; end;

  IconFExt := ExtractFileExt(IconFName);
  case UpperCase(IconFExt) = 'ICO' of  TRUE: iconImage.Picture.Icon.LoadFromFile(IconFName);
                                      FALSE: iconImage.Picture.Icon.Handle := ExtractIcon(hInstance, StrPCopy(pFName, IconFName), IconView.Selected.ImageIndex); end;

  toolbar.visible := TRUE;
  iconLabel.caption := 'Click above to apply this icon to your menu item'#13;
end;

procedure TIconExplorerForm.iconViewLoadIcons(IFName: string);
var
  x:          integer;
  Icon:       TIcon;
  pFName:     array[0..255] of char;
  oldCursor:  TCursor;
  ListItem:   TListItem;
begin
  toolbar.visible := FALSE;
  iconList.clear;
  IconView.clear;
  iconImage.picture := NIL;

  case trim(IFName) = ''  of TRUE:  EXIT; end;
  case FileExists(IFName) of FALSE: EXIT; end; // all folders will fail this

  OldCursor := Screen.Cursor;
  Screen.CurSor := crHourGlass;
  try try
    NumberOfIcons := ExtractIcon(0, StrPCopy(pFName, IFName), $FFFFFFFF);
    IconList.Clear;
    iconView.Clear;
    iconLabel.caption := '';
    case numberOfIcons = 0 of TRUE: begin
                                      iconLabel.caption := 'No icons in ' + extractFilename(IFName);
                                      EXIT;
                                    end;end;
    IconList.Height   := 32;
    IconList.Width    := 32;
    iconImage.Picture := NIL;

    with IconView do begin
      Items.Clear;
      ViewStyle := vsIcon;
      LargeImages := IconList;
      Items.beginUpdate;
      try
        for x := 0 to NumberOfIcons - 1 do begin
          Icon := TIcon.Create;
          Icon.Handle := ExtractIcon(hInstance, pFName, x);
          IconList.AddIcon(Icon);
          ListItem := Items.Add;
          ListItem.Caption := Format('%d', [x]);
          ListItem.ImageIndex := x;
          Icon.Free;
        end;
      finally
        Items.endUpdate;
        IconView.Selected := nil;
      end;
    end;
    iconLabel.caption := 'Click an icon to view it above or double-click to use it';
  except
    iconLabel.caption := 'Oops!';
  end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TIconExplorerForm.IconViewResize(Sender: TObject);
begin
  IconViewLoadIcons(lv.selectedPath);
end;

procedure TIconExplorerForm.FormCreate(Sender: TObject);
begin
//  LV.ThumbsManager.StorageRepositoryFolder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'AlbumsRepository';
  // Register some special Folders that the thread will be able to generate
  // notification PIDLs for Virtual Folders too.
  ChangeNotifier.RegisterKernelChangeNotify(LV, AllKernelNotifiers);
  ChangeNotifier.RegisterKernelSpecialFolderWatch(CSIDL_DESKTOP);
  ChangeNotifier.RegisterKernelSpecialFolderWatch(CSIDL_PERSONAL);
  ChangeNotifier.RegisterKernelSpecialFolderWatch(CSIDL_COMMON_DOCUMENTS);
  iconLabel.caption := 'Find and select a file which contains icons';

  filterCombo.itemIndex := 0;
  tree.FileObjects := [foFolders];
  tree.TreeOptions.VETMiscOptions := [toBrowseExecuteFolder];

  setColors;
end;

procedure TIconExplorerForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_ESCAPE of TRUE: modalResult := mrCancel; end;
end;

procedure TIconExplorerForm.FormResize(Sender: TObject);
begin
  updateAddress(addressLabel.caption);
end;

procedure TIconExplorerForm.FormShow(Sender: TObject);
begin
  setForegroundWindow(SELF.Handle);
  case trim(GFilePath) <> '' of TRUE: begin
                                        tree.browseTo(GFilePath);
                                        lv.BrowseTo(GFilePath, TRUE); end;end;
  updateAddress(GFilePath);
  lv.Rebuild;
  applyFilter(TRUE);
  applyFilter(FALSE);
end;

procedure TIconExplorerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ChangeNotifier.UnRegisterKernelChangeNotify(LV);
  action := caFree;
end;

procedure TIconExplorerForm.LVColumnPaintText(Sender: TCustomEasyListview; Column: TEasyColumn; ACanvas: TCanvas);
begin
  case lowerCase(column.caption) = 'date modified' of TRUE: column.caption := 'Modified'; end;
end;

procedure TIconExplorerForm.LVItemClick(Sender: TCustomEasyListview; Item: TEasyItem; KeyStates: TCommonKeyStates; HitInfo: TEasyItemHitTestInfoSet);
// lv.selectedPath can be the fully qualified path to a file name, as well as a folder
begin
try
  IconFName := lv.selectedPath;
  IconViewLoadIcons(lv.selectedPath);
except end;
end;

procedure TIconExplorerForm.LVItemSelectionChanged(Sender: TCustomEasyListview; Item: TEasyItem);
// lv.selectedPath can be the fully qualified path to a file name, as well as a folder
begin
try
  IconFName := lv.selectedPath;
  IconViewLoadIcons(lv.selectedPath);
except end;
end;

function TIconExplorerForm.setColors: boolean;
begin
  FBackgroundColor                := getBackgroundColor;
  FHighlightColor                 := getHighlightColor;
  FInfoColor                      := getInfoColor;

  color                           := FBackgroundColor;
  tree.color                      := FBackgroundColor;
//  LV.color                        := color; // NO!!
  LV.Header.color                 := FBackgroundColor;
//  LV.PaintInfoColumn.color        := color;
//  LV.PaintInfoColumn.borderColor  := color;
  iconView.color                  := FBackgroundColor;
  addressPanel.color              := FBackgroundColor;
  iconControlsPanel.color         := FBackgroundColor;
  bottomBorderPanel.color         := FBackgroundColor;
  iconPanel.color                 := FBackgroundColor;
  panel1.color                    := FBackgroundColor;
  listPanel.color                 := FBackgroundColor;
  filterPanel.color               := FBackgroundColor;
  filterCombo.styleElements       := [seFont, seBorder];
  filterCombo.color               := FBackgroundColor;
  treePanel.color                 := FBackgroundColor;

  tree.colors.FocusedSelectionColor         := FHighlightColor;
  tree.colors.UnfocusedSelectionColor       := FHighlightColor;
  tree.colors.UnfocusedSelectionBorderColor := FHighlightColor;
  tree.colors.SelectionTextColor            := clWhite;
  tree.colors.UnfocusedColor                := clWhite; // this is the text font color
//  lv.hottrack.Color                 := FHighlightColor;
//  lv.paintInfoGroup.bandColor       := FHighlightColor;
//  lv.paintInfoGroup.bandColorFade   := FHighlightColor;
//  lv.paintInfoItem.GridLineColor    := FHighlightColor;
  lv.Selection.BlendColorSelRect    := FHighlightColor;
  lv.Selection.BorderColor          := FHighlightColor;
  lv.Selection.BorderColorSelRect   := FHighlightColor;
  lv.Selection.Color                := FHighlightColor;
  lv.Selection.InactiveBorderColor  := FHighlightcolor;
  lv.Selection.InactiveColor        := FHighlightColor;
  lv.Selection.InactiveTextColor    := clWhite;

  addressLabel.font.color         := FInfoColor;
  iconLabel.font.color            := FInfoColor;
end;

procedure TIconExplorerForm.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
//var
//  NS: TNamespace;
begin
//    case tree.validateNamespace(Node, NS) of TRUE: {changeNotifier.notifyWatchFolder(LV, NS.nameForParsing);} end;
try
  lv.BeginUpdate;
  try
    applyFilter(TRUE);
    applyFilter(FALSE);
  finally
    lv.EndUpdate(FALSE);
  end;
  updateAddress(tree.selectedPath);
except end;
end;

procedure TIconExplorerForm.TreeClick(Sender: TObject);
begin
  EXIT; // LV already knows!
end;

procedure TIconExplorerForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var NS: TNameSpace;
begin
  try
    case tree.validateNamespace(Node, NS) and NS.archive of TRUE: initialStates := initialStates + [ivsFiltered]; end; // is really ivsFilteredOut
  except end;
end;

function TIconExplorerForm.updateAddress(const aCaption: string): boolean;
begin
  addressLabel.caption := aCaption;
  addressLabel.left    := listPanel.left;
end;

procedure TIconExplorerForm.filterComboChange(Sender: TObject);
begin
try
  lv.beginUpdate;
  try
    applyFilter(TRUE);
    applyFilter(FALSE);
  finally
    lv.endUpdate(FALSE);
  end;
except end;
end;

procedure TIconExplorerForm.filterComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  filterCombo.canvas.font.color := clWhite;
  filterCombo.canvas.brush.color := FBackgroundColor;
  case (odFocused in state) or (odSelected in state) of TRUE: filterCombo.canvas.brush.color := FHighlightColor; end;

  filterCombo.canvas.fillRect(rect);

  var vCenterText: integer := (rect.bottom - rect.top - filterCombo.canvas.textHeight(filterCombo.items[index])) div 2;
  filterCombo.canvas.textOut(rect.left, rect.top + vCenterText, filterCombo.items[index]);

  case odFocused in state of TRUE: filterCombo.canvas.drawFocusRect(rect); end; // prevents the dotted box around a focused item. It gets XOR-ed in the VCL's own call to DrawFocusRect.
end;

procedure TIconExplorerForm.filterComboKeyPress(Sender: TObject; var Key: Char);
begin
  key := #0;
end;

initialization
  GFilePath         := '';
  GIconIx           := -1;
  GHWND             := -1;

end.


