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
    procedure CheckBoxCustomGroupingClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure LVColumnPaintText(Sender: TCustomEasyListview; Column: TEasyColumn; ACanvas: TCanvas);
    procedure LVItemInitialize(Sender: TCustomEasyListview; Item: TEasyItem);
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
  private
    NumberOfIcons: LongInt;
    procedure IconViewLoadIcons(IFName: string);
  public
    IconFName, IconFExt: string;
    IsBmp, IsIco: Boolean;
    IcoColors, BmpHeight, BmpWidth: integer;
  end;

var
  GFilePath: string;

function showIconExplorer(var filePath: string; var iconIx: integer): boolean;

implementation

uses
  WinAPI.ShellAPI, _debugWindow, WinAPI.CommCtrl;

var
  iconExplorerForm: TiconExplorerForm;
  GIconIx: integer;

{$R *.dfm}

function showIconExplorer(var filePath: string; var iconIx: integer): boolean;
var vMr: TModalResult;
begin
  with TIconExplorerForm.create(NIL) do vMr := showModal;

  case vMr = mrOK of TRUE:  begin
                              filePath  := GFilePath;
                              iconIx    := GIconIx; end;end;

  result := vMr = mrOK;
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
  iconLabel.caption := 'Click above to apply this icon to your menu item';
end;

procedure TIconExplorerForm.IconViewLoadIcons(IFName: string);
var
  x:          integer;
  Icon:       TIcon;
  pFName:     array[0..255] of char;
  oldCursor:  TCursor;
  ListItem:   TListItem;
begin
  case FileExists(IFName) of FALSE: EXIT; end;

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
      Items.BeginUpdate;
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
        Items.EndUpdate;
        IconView.Selected := nil;
      end;
    end;
    iconLabel.caption := 'Click an icon to view it above';
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
  LV.ThumbsManager.StorageRepositoryFolder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'AlbumsRepository';
  ChangeNotifier.RegisterKernelChangeNotify(LV, AllKernelNotifiers);
  // Register some special Folders that the thread will be able to generate
  // notification PIDLs for Virtual Folders too.
  ChangeNotifier.RegisterKernelSpecialFolderWatch(CSIDL_DESKTOP);
  ChangeNotifier.RegisterKernelSpecialFolderWatch(CSIDL_PERSONAL);
  ChangeNotifier.RegisterKernelSpecialFolderWatch(CSIDL_COMMON_DOCUMENTS);
  iconLabel.caption := 'Find and select a file which contains icons';

  case trim(GFilePath) <> '' of TRUE: lv.browseTo(extractFilePath(GFilePath), TRUE); end; // go directly to the previously-browsed folder
  addressLabel.caption := GFilePath;
end;

procedure TIconExplorerForm.FormShow(Sender: TObject);
begin
//  CheckBoxQueryInfo.Checked :=  eloQueryInfoHints in LV.Options;
//  CheckBoxNotifierThread.Checked :=  eloChangeNotifierThread in LV.Options;
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
begin
  IconFName := lv.selectedPath;
  IconViewLoadIcons(lv.selectedPath);
end;

procedure TIconExplorerForm.LVItemInitialize(Sender: TCustomEasyListview; Item: TEasyItem);
begin
  case extractFileExt(item.caption) = '' of TRUE: EXIT; end; // folders
  item.visible := pos(lowercase(extractFileExt(item.caption)), filterCombo.items[filterCombo.itemIndex]) > 0;
end;

procedure TIconExplorerForm.LVItemSelectionChanged(Sender: TCustomEasyListview; Item: TEasyItem);
begin
//  addressLabel.caption := extractFilePath(lv.selectedPath); {experimental}
  IconFName := lv.selectedPath;
  IconViewLoadIcons(lv.selectedPath);
end;

procedure TIconExplorerForm.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NS: TNamespace;
begin
  if Tree.ValidateNamespace(Node, NS) then ChangeNotifier.NotifyWatchFolder(LV, NS.NameForParsing);
  addressLabel.caption := tree.SelectedPath;
end;

procedure TIconExplorerForm.filterComboChange(Sender: TObject);
begin
  lv.rebuild;
end;

procedure TIconExplorerForm.filterComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  filterCombo.canvas.font.color := clWhite;

  // prevent the selected item from also being drawn focused when the comboBox is closed. It looks naff!
  case filterCombo.DroppedDown of  TRUE: case odFocused in state of TRUE: filterCombo.canvas.brush.color := $434343; end;
                                  FALSE: case odSelected in state of TRUE: filterCombo.canvas.brush.color := $2B2B2B; end;end;
  filterCombo.canvas.fillRect(rect);

  var vCenterText: integer := (rect.bottom - rect.top - filterCombo.canvas.textHeight(text)) div 2;
  filterCombo.Canvas.textOut(rect.left, rect.top + vCenterText, filterCombo.items[index]); // add additional space for a gap after the icon

  case odFocused in state of TRUE: filterCombo.canvas.drawFocusRect(rect); end; // prevents the dotted box around a focused item. It gets XOR-ed in the VCL's own call to DrawFocusRect.
end;

procedure TIconExplorerForm.filterComboKeyPress(Sender: TObject; var Key: Char);
begin
  key := #0;
end;

procedure TIconExplorerForm.CheckBoxCustomGroupingClick(Sender: TObject);
begin
  LV.Rebuild
end;

initialization
  iconExplorerForm  := NIL;
  GFilePath         := '';
  GIconIx           := -1;

end.


