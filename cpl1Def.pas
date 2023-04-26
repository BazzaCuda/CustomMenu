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
unit cpl1Def;

interface

const
  cpl1NameArray: TArray<string> = [
    'Add or Remove Programs',
    'Bluetooth',
    'Controllers',
    'Date and Time',
    'Device Manager',
    'Display',
    'Ease of Access',
    'Firewall',
    'Infrared',
    'Internet Options',
    'Keyboard',
    'Language',
    'Location Information',
    'Mouse',
    'Network',
    'Network Connections',
    'Power',
    'Region',
    'Screensaver',
    'Security and Maintenance',
    'Sound',
    'Speech',
    'System Properties',
    'Tablet PC'
];

  cpl1CmdArray: TArray<string> = [
    'appwiz.cpl',
    'bthprops.cpl',
    'joy.cpl',
    'timedate.cpl',
    'hdwwiz.cpl',
    'desk.cpl',
    'access.cpl',
    'firewall.cpl',
    'irprops.cpl',
    'inetcpl.cpl',
    'main.cpl keyboard',
    'control input.dll',
    'telephon.cpl',
    'main.cpl',
    'netcpl.cpl',
    'ncpa.cpl',
    'powercfg.cpl',
    'intl.cpl',
    'desk.cpl,screensaver,@screensaver', // !!!
    'wscui.cpl',
    'mmsys.cpl',
    'sapi.cpl',
    'sysdm.cpl',
    'tabletpc.cpl'
];

implementation

end.
