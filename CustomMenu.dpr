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
program CustomMenu;
{$WEAKLINKRTTI ON}   // don't include extended RTTI for unused types
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])} // don't include extended RTTI for used types
{$M-} // enables the published keyword
uses
  Vcl.Forms,
//  Vcl.Styles,
//  Vcl.Themes,
  FormCustomMenu in 'FormCustomMenu.pas' {CustomMenu},
  Winapi.Hooks in 'Winapi.Hooks.pas',
  CustomMenuCommon in 'CustomMenuCommon.pas',
  FormConfig in 'FormConfig.pas' {ConfigForm},
  _debugWindow in '..\DebugWindow\_debugWindow.pas',
  FormIconExplorer in 'FormIconExplorer.pas' {IconExplorerForm},
  winShell in 'winShell.pas',
  runElevatedSupport in 'runElevatedSupport.pas',
  shellFoldersDef in 'shellFoldersDef.pas',
  mmcDef in 'mmcDef.pas',
  mmcServerDef in 'mmcServerDef.pas',
  cpl1Def in 'cpl1Def.pas',
  cpl2Def in 'cpl2Def.pas',
  rundll32Def in 'rundll32Def.pas',
  msSettingsDef in 'msSettingsDef.pas',
  shellGuidsDef in 'shellGuidsDef.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := TRUE;

  case hasParamSingle and alreadyRunning of TRUE: begin
                                                    case hasParamShowMenu of TRUE: findCustomMenu; end;
                                                    EXIT; end;end;

  createMiniIni;

//  debugPause;
  debugClear;

  Application.Initialize;
  Application.MainFormOnTaskbar := TRUE;
  Application.CreateForm(TCustomMenu, mainMenu);
  Application.Run;
end.
