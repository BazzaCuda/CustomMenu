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
unit mmcServerDef;

interface

const
  mmcServerNameArray: TArray<string> = [
'Active Directory Domains and Trust',
'Active Directory Federation Services',
'Active Directory Rights Management Services',
'Active Directory Sites and Services',
'Active Directory Users and Computers',
'ADSI Edit',
'Certification Authority',
'Certification Templates',
'DFS Management',
'DHCP Management',
'DNS Manager',
'Enterprise PKI',
'Failover Cluster Manager',
'File Server Resource Manager',
'Group Policy Management',
'Group Policy Mangement Editor',
'Group Policy Starter GPO Editor',
'Health Registration Authority',
'HTTPS Traffic Performance Monitor',
'Hyper-V Manager',
'Indexing Service',
'Internet Information Services Manager',
'Internet Information Services Manager 6.0',
'IPSec Performance Monitor',
'ISATAP Performance Monitor',
'Local Computer Certificates',
'Microsoft Fax Service Manager',
'Microsoft Identity Management for Unix',
'NAP Client Configuration',
'Network Interfaces Performance Monitor',
'Network Policy Server',
'Online Responder',
'RD Gateway Manager',
'RD Licensing Diagnoser',
'Remote Desktop Connection Manager',
'Remote Desktop Services Manager',
'Remote Desktop Session Host Configuration',
'Remote Desktops',
'RemoteApp Manager',
'Routing and Remote Access',
'Scan Management',
'Server Manager',
'Services for Network File System',
'Share and Storage Management',
'Storage Explorer',
'Storage Manager for SANs',
'Telephony',
'Update Services',
'Windows Deployment Services',
'Windows Server Backup',
'Windows Server Backup',
'Windows System Resource Manager',
'WINS'
];

  mmcServerCmdArray: TArray<string> = [
'domain.msc',
'adfs.msc',
'AdRmsAdmin.msc',
'dssite.msc',
'dsa.msc',
'adsiedit.msc',
'certsrv.msc',
'certtmpl.msc',
'dfsmgmt.msc',
'dhcpmgmt.msc',
'dnsmgmt.msc',
'pkiview.msc',
'cluadmin.msc',
'fsrm.msc',
'gpmc.msc',
'gpme.msc',
'gptedit.msc',
'hcscfg.msc',
'daihttps.msc',
'virtmgmt.msc',
'ciadv.msc',
'iis.msc',
'iis6.msc',
'daipsecdos.msc',
'daisatapmsc',
'certim.msc',
'fxsadmin.msc',
'idmumgmt.msc',
'napclcfg.msc',
'da6to4.msc',
'nps.msc',
'ocsp.msc',
'tsgateway.msc',
'lsdiag.msc',
'sbmgr.msc',
'tsadmin.msc',
'tsconfig.msc',
'tsmmc.msc',
'remoteprograms.msc',
'rrasmgmt.msc',
'scanmanagement.msc',
'servermanager.msc',
'mfsmgmt.msc',
'storagemgmt.msc',
'storexpl.msc',
'sanmmc.msc',
'tapimgmt.msc',
'wsus.msc',
'Wdsmgmt-msc',
'wbadmin.msc',
'wbiadmin.msc',
'wsrm.msc',
'winsmgmt.msc'
];

implementation

end.
