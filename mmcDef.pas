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
unit mmcDef;

interface

const
  mmcNameArray: TArray<string> = [
    'Authorization Manager',
    'Certificates',
    'Certificates Local Computer',
    'Component Services',
    'Computer Management',
    'Device Manager',
    'Disk Management',
    'Event Viewer',
    'Group Policy Editor',
    'Local Security Policy',
    'Local Users and Groups',
    'Performance Monitor',
    'Print Management',
    'Resultant Set of Policies',
    'Services Manager',
    'Shared Folders',
    'Task Scheduler',
    'Trusted Platform Module Management',
    'User Manager',
    'Windows Firewall',
    'WMI Management'
];

  mmcCmdArray: TArray<string> = [
    'azman.msc',
    'certmgr.msc',
    'certlm.msc',
    'comexp.msc',
    'compmgmt.msc',
    'devmgmt.msc',
    'diskmgmt.msc',
    'eventvwr.msc',
    'gpedit.msc',
    'secpol.msc',
    'lusrmgr.msc',
    'perfmon.msc',
    'printmanagement.msc',
    'rsop.msc',
    'services.msc',
    'fsmgmt.msc',
    'taskschd.msc',
    'tpm.msc',
    'devmoderunasuserconfig.msc',
    'wf.msc',
    'wmimgmt.msc'
];

implementation

end.
