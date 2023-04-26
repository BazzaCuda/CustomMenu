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
unit rundll32Def;

interface

const
  rundll32NameArray: TArray<string> = [
    'About Windows',
    'Add Network Location Wizard',
    'Add Printer Wizard',
    'Add Standard TCP/IP Printer Port Wizard',
    'Control Panel',
    'Date and Time',
    'Date and Time - Additional Clocks tab',
    'Desktop Icon Settings',
    'Device Installation Settings',
    'Device Manager',
    'Display Settings',
    'Ease of Access Center',
    'Environment Variables',
    'File Explorer Options - General tab',
    'File Explorer Options - Search tab',
    'File Explorer Options - View tab',
    'Fonts folder',
    'Forgotten Password Wizard',
    'Game Controllers',
    'Hibernate or Sleep',
    'Indexing Options',
    'Infared',
    'Internet Explorer - delete all browsing history',
    'Internet Explorer - delete all browsing history and add-ons history',
    'Internet Explorer - delete cookies and website data',
    'Internet Explorer - delete download history',
    'Internet Explorer - delete form data',
    'Internet Explorer - delete history',
    'Internet Explorer - delete passwords',
    'Internet Explorer - delete temporary Internet files and website files',
    'Internet Explorer - Organize Favorites',
    'Internet Properties - General tab',
    'Internet Properties - Security tab',
    'Internet Properties - Privacy tab',
    'Internet Properties - Content tab',
    'Internet Properties - Connections tab',
    'Internet Properties - Programs tab',
    'Internet Properties - Advanced tab',
    'Keyboard Properties',
    'Lock PC',
    'Map Network Drive wizard',
    'Mouse Button swap left and right button function',
    'Mouse Properties - Buttons tab',
    'Mouse Properties - Pointers tab',
    'Mouse Properties - Pointer Options tab',
    'Mouse Properties - Wheel tab',
    'Mouse Properties - Hardware tab',
    'Network Connections',
    'ODBC Data Source Administrator',
    'Offline Files (General tab)',
    'Offline Files (Disk Usage tab)',
    'Offline Files (Encryption tab)',
    'Offline Files (Network tab)',
    'Pen and Touch',
    'Personalization - Background Settings',
    'Power Options',
    'Printer User Interface',
    'Printers folder',
    'Process idle tasks',
    'Programs and Features',
    'Region - Formats tab',
    'Region - Location tab',
    'Region - Administrative tab',
    'Safely Remove Hardware',
    'Screen Saver Settings',
    'Security and Maintenance',
    'Set Program Access and Computer Defaults',
    'Set Up a Network wizard',
    'Sleep or Hibernate',
    'Sound - Playback tab',
    'Sound - Recording tab',
    'Sound - Sounds tab',
    'Sound - Communications tab',
    'Speech Properties - Text to Speech tab',
    'Start Settings',
    'Stored User Names and Passwords',
    'System Properties - Computer Name tab',
    'System Properties - Hardware tab',
    'System Properties - Advanced tab',
    'System Properties - System Protection tab',
    'System Properties - Remote tab',
    'Taskbar Settings',
    'Text Services and Input Languages',
    'User Accounts',
    'Windows Features',
    'Windows Firewall',
    'Windows To Go Startup Options'
];

  rundll32CmdArray: TArray<string> = [
    'shell32.dll,ShellAbout',
    '%SystemRoot%\system32\shwebsvc.dll,AddNetPlaceRunDll',
    'shell32.dll,SHHelpShortcuts_RunDLL AddPrinter',
    'tcpmonui.dll,LocalAddPortUI',
    'shell32.dll,Control_RunDLL',
    'shell32.dll,Control_RunDLL timedate.cpl',
    'shell32.dll,Control_RunDLL timedate.cpl,,1',
    'shell32.dll,Control_RunDLL desk.cpl,,0',
    '%SystemRoot%\System32\newdev.dll,DeviceInternetSettingUi',
    'devmgr.dll DeviceManager_Execute',
    'shell32.dll,Control_RunDLL desk.cpl',
    'shell32.dll,Control_RunDLL access.cpl',
    'sysdm.cpl,EditEnvironmentVariables',
    'shell32.dll,Options_RunDLL 0',
    'shell32.dll,Options_RunDLL 2',
    'shell32.dll,Options_RunDLL 7',
    'shell32.dll,SHHelpShortcuts_RunDLL FontsFolder',
    'keymgr.dll,PRShowSaveWizardExW',
    'shell32.dll,Control_RunDLL joy.cpl',
    'powrprof.dll,SetSuspendState',
    'shell32.dll,Control_RunDLL srchadmin.dll',
    'shell32.dll,Control_RunDLL irprops.cpl',
    'InetCpl.cpl,ClearMyTracksByProcess 255',
    'InetCpl.cpl,ClearMyTracksByProcess 4351',
    'InetCpl.cpl,ClearMyTracksByProcess 2',
    'InetCpl.cpl,ClearMyTracksByProcess 16384',
    'InetCpl.cpl,ClearMyTracksByProcess 16',
    'InetCpl.cpl,ClearMyTracksByProcess 1',
    'InetCpl.cpl,ClearMyTracksByProcess 32',
    'InetCpl.cpl,ClearMyTracksByProcess 8',
    'shdocvw.dll,DoOrganizeFavDlg',
    'shell32.dll,Control_RunDLL inetcpl.cpl',
    'shell32.dll,Control_RunDLL inetcpl.cpl,,1',
    'shell32.dll,Control_RunDLL inetcpl.cpl,,2',
    'shell32.dll,Control_RunDLL inetcpl.cpl,,3',
    'shell32.dll,Control_RunDLL inetcpl.cpl,,4',
    'shell32.dll,Control_RunDLL inetcpl.cpl,,5',
    'shell32.dll,Control_RunDLL inetcpl.cpl,,6',
    'shell32.dll,Control_RunDLL main.cpl @1',
    'user32.dll,LockWorkStation',
    'shell32.dll,SHHelpShortcuts_RunDLL Connect',
    'user32.dll,SwapMouseButton',
    'shell32.dll,Control_RunDLL main.cpl',
    'shell32.dll,Control_RunDLL main.cpl,,1',
    'shell32.dll,Control_RunDLL main.cpl,,2',
    'shell32.dll,Control_RunDLL main.cpl,,3',
    'shell32.dll,Control_RunDLL main.cpl,,4',
    'shell32.dll,Control_RunDLL ncpa.cpl',
    'shell32.dll,Control_RunDLL odbccp32.cpl',
    'Shell32.dll,Control_RunDLL cscui.dll,,0',
    'Shell32.dll,Control_RunDLL cscui.dll,,1',
    'Shell32.dll,Control_RunDLL cscui.dll,,2',
    'Shell32.dll,Control_RunDLL cscui.dll,,3',
    'shell32.dll,Control_RunDLL tabletpc.cpl',
    'shell32.dll,Control_RunDLL desk.cpl,,2',
    'shell32.dll,Control_RunDLL powercfg.cpl',
    'Printui.dll,PrintUIEntry /?',
    'shell32.dll,SHHelpShortcuts_RunDLL PrintersFolder',
    'advapi32.dll,ProcessIdleTasks',
    'shell32.dll,Control_RunDLL appwiz.cpl,,0',
    'shell32.dll,Control_RunDLL Intl.cpl,,0',
    'shell32.dll,Control_RunDLL Intl.cpl,,1',
    'shell32.dll,Control_RunDLL Intl.cpl,,2',
    'shell32.dll,Control_RunDLL HotPlug.dll',
    'shell32.dll,Control_RunDLL desk.cpl,,1',
    'shell32.dll,Control_RunDLL wscui.cpl',
    'shell32.dll,Control_RunDLL appwiz.cpl,,3',
    'shell32.dll,Control_RunDLL NetSetup.cpl',
    'powrprof.dll,SetSuspendState',
    'shell32.dll,Control_RunDLL Mmsys.cpl,,0',
    'shell32.dll,Control_RunDLL Mmsys.cpl,,1',
    'shell32.dll,Control_RunDLL Mmsys.cpl,,2',
    'shell32.dll,Control_RunDLL Mmsys.cpl,,3',
    'shell32.dll,Control_RunDLL %SystemRoot%\System32\Speech\SpeechUX\sapi.cpl,,1',
    'shell32.dll,Options_RunDLL 3',
    'keymgr.dll,KRShowKeyMgr',
    'shell32.dll,Control_RunDLL Sysdm.cpl,,1',
    'shell32.dll,Control_RunDLL Sysdm.cpl,,2',
    'shell32.dll,Control_RunDLL Sysdm.cpl,,3',
    'shell32.dll,Control_RunDLL Sysdm.cpl,,4',
    'shell32.dll,Control_RunDLL Sysdm.cpl,,5',
    'shell32.dll,Options_RunDLL 1',
    'Shell32.dll,Control_RunDLL input.dll,,{C07337D3-DB2C-4D0B-9A93-B722A6C106E2}',
    'shell32.dll,Control_RunDLL nusrmgr.cpl',
    'shell32.dll,Control_RunDLL appwiz.cpl,,2',
    'shell32.dll,Control_RunDLL firewall.cpl',
    'pwlauncher.dll,ShowPortableWorkspaceLauncherConfigurationUX'
];

implementation

end.
