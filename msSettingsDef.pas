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
unit msSettingsDef;

interface

const
  ms1NameArray: TArray<string> = [
    'Access work or school',
    'Email & app accounts',
    'Family & other people',
    'Set up a kiosk',
    'Sign-in options',
    'Sync your settings',
    'Sync (Backup, not Win11)',
    'Windows Hello setup (face)',
    'Windows Hello setup (finger)',
    'Your info'
];

  ms1CmdArray: TArray<string> = [
    'ms-settings:workplace',
    'ms-settings:emailandaccounts',
    'ms-settings:otherusers',
    'ms-settings:assignedaccess',
    'ms-settings:signinoptions',
    'ms-settings:sync',
    'ms-settings:backup',
    'ms-settings:signinoptions-launchfaceenrollment',
    'ms-settings:signinoptions-launchfingerprintenrollment',
    'ms-settings:yourinfo'
];

  ms2NameArray: TArray<string> = [
    'Apps & Features',
    'App features',
    'Apps for websites',
    'Default apps',
    'Manage optional features',
    'Offline Maps',
    'Startup apps',
    'Video playback'
];

  ms2CmdArray: TArray<string> = [
    'ms-settings:appsfeatures',
    'ms-settings:appsfeatures-app',
    'ms-settings:appsforwebsites',
    'ms-settings:defaultapps',
    'ms-settings:optionalfeatures',
    'ms-settings:maps',
    'ms-settings:startupapps',
    'ms-settings:videoplayback'
];

  ms3NameArray: TArray<string> = [
    'Control center'
];

  ms3CmdArray: TArray<string> = [
    'ms-settings:controlcenter'
];

  ms4NameArray: TArray<string> = [
    'Cortana across my devices',
    'More details',
    'Permissions & History',
    'Searching Windows',
    'Cortana',
    'Cortana (language)',
    'Talk to Cortana'
];

  ms4CmdArray: TArray<string> = [
    'ms-settings:cortana-notifications',
    'ms-settings:cortana-moredetails',
    'ms-settings:cortana-permissions',
    'ms-settings:cortana-windowssearch',
    'ms-settings:cortana',
    'ms-settings:cortana-language',
    'ms-settings:cortana-talktocortana'
];

  ms5NameArray: TArray<string> = [
    'AutoPlay',
    'Bluetooth',
    'Connected Devices',
    'Camera settings',
    'Mouse & touchpad',
    'Pen & Windows Ink',
    'Printers & scanners',
    'Touch',
    'Touchpad',
    'Text Suggestions',
    'Typing',
    'USB',
    'Wheel',
    'Your phone'
];

  ms5CmdArray: TArray<string> = [
    'ms-settings:autoplay',
    'ms-settings:bluetooth',
    'ms-settings:connecteddevices',
    'ms-settings:camera',
    'ms-settings:mousetouchpad',
    'ms-settings:pen',
    'ms-settings:printers',
    'ms-settings:devices-touch',
    'ms-settings:devices-touchpad',
    'ms-settings:devicestyping-hwkbtextsuggestions',
    'ms-settings:typing',
    'ms-settings:usb',
    'ms-settings:wheel',
    'ms-settings:mobile-devices'
];

  ms6NameArray: TArray<string> = [
    'Audio',
    'Closed captions',
    'Color filters',
    'Color filters adaptive',
    'Color filter bluelight',
    'Display',
    'Eye control',
    'Fonts',
    'High contrast',
    'Keyboard',
    'Magnifier',
    'Mouse',
    'Mouse pointer & touch',
    'Narrator',
    'Narrator Auto Start',
    'Speech',
    'Text cursor',
    'Visual Effects'
];

  ms6CmdArray: TArray<string> = [
    'ms-settings:easeofaccess-audio',
    'ms-settings:easeofaccess-closedcaptioning',
    'ms-settings:easeofaccess-colorfilter',
    'ms-settings:easeofaccess-colorfilter-adaptivecolorlink',
    'ms-settings:easeofaccess-colorfilter-bluelightlink',
    'ms-settings:easeofaccess-display',
    'ms-settings:easeofaccess-eyecontrol',
    'ms-settings:fonts',
    'ms-settings:easeofaccess-highcontrast',
    'ms-settings:easeofaccess-keyboard',
    'ms-settings:easeofaccess-magnifier',
    'ms-settings:easeofaccess-mouse',
    'ms-settings:easeofaccess-mousepointer',
    'ms-settings:easeofaccess-narrator',
    'ms-settings:easeofaccess-narrator-isautostartenabled',
    'ms-settings:easeofaccess-speechrecognition',
    'ms-settings:easeofaccess-cursor',
    'ms-settings:easeofaccess-visualeffects'
];

  ms7NameArray: TArray<string> = [
    'Extras',
    'Family Group',
    'Default Browser settings',
    'Device Usage',
    'Your Phone (mobile devices)'
];

  ms7CmdArray: TArray<string> = [
    'ms-settings:extras',
    'ms-settings:family-group',
    'ms-settings:defaultbrowsersettings',
    'ms-settings:deviceusage',
    'ms-settings:mobile-devices'
];

  ms8NameArray: TArray<string> = [
    'Game bar',
    'Game DVR',
    'Game Mode',
    'Playing a game full screen'
];

  ms8CmdArray: TArray<string> = [
    'ms-settings:gaming-gamebar',
    'ms-settings:gaming-gamedvr',
    'ms-settings:gaming-gamemode',
    'ms-settings:quietmomentsgame'
];

  ms9NameArray: TArray<string> = [
    'Audio and speech',
    'Environment',
    'Headset display',
    'Uninstall',
    'Startup and desktop'
];

  ms9CmdArray: TArray<string> = [
    'ms-settings:holographic-audio',
    'ms-settings:privacy-holographic-environment',
    'ms-settings:holographic-headset',
    'ms-settings:holographic-management',
    'ms-settings:holographic-startupandesktop'
];

  ms10NameArray: TArray<string> = [
    'Network & internet',
    'Advanced settings',
    'Airplane mode',
    'Airplane mode (proximity)',
    'Cellular & SIM',
    'Dial-up',
    'DirectAccess',
    'Ethernet',
    'Manage known networks',
    'Mobile hotspot',
    'Proxy',
    'VPN',
    'Wi-Fi',
    'Wi-Fi provisioning'
];

  ms10CmdArray: TArray<string> = [
    'ms-settings:network-status',
    'ms-settings:network-advancedsettings',
    'ms-settings:network-airplanemode',
    'ms-settings:proximity',
    'ms-settings:network-cellular',
    'ms-settings:network-dialup',
    'ms-settings:network-directaccess',
    'ms-settings:network-ethernet',
    'ms-settings:network-wifisettings',
    'ms-settings:network-mobilehotspot',
    'ms-settings:network-proxy',
    'ms-settings:network-vpn',
    'ms-settings:network-wifi',
    'ms-settings:wifi-provisioning'
];

  ms11NameArray: TArray<string> = [
    'Background',
    'Choose start folders',
    'Colors',
    'Lock screen',
    'Personalization (category)',
    'Personalization Start',
    'Taskbar',
    'Touch Keyboard',
    'Themes'
];

  ms11CmdArray: TArray<string> = [
    'ms-settings:personalization-background',
    'ms-settings:personalization-start-places',
    'ms-settings:personalization-colors',
    'ms-settings:lockscreen',
    'ms-settings:personalization',
    'ms-settings:personalization-start',
    'ms-settings:taskbar',
    'ms-settings:personalization-touchkeyboard',
    'ms-settings:themes'
];

  ms12NameArray: TArray<string> = [
    'Account info',
    'Activity history',
    'App diagnostics',
    'Automatic file downloads',
    'Background Spatial Perception',
    'Calendar',
    'Call history',
    'Camera',
    'Contacts',
    'Documents',
    'Downloads folder',
    'Email',
    'Eye tracker',
    'Feedback & diagnostics',
    'File system',
    'General',
    'Graphics (capture programmatic)',
    'Graphics (capture w/o border)',
    'Inking & typing',
    'Location',
    'Messaging',
    'Microphone',
    'Motion',
    'Music Library',
    'Notifications',
    'Other devices',
    'Phone calls',
    'Pictures',
    'Radios',
    'Speech',
    'Tasks',
    'Videos',
    'Voice activation'
];

  ms12CmdArray: TArray<string> = [
    'ms-settings:privacy-accountinfo',
    'ms-settings:privacy-activityhistory',
    'ms-settings:privacy-appdiagnostics',
    'ms-settings:privacy-automaticfiledownloads',
    'ms-settings:privacy-backgroundspatialperception',
    'ms-settings:privacy-calendar',
    'ms-settings:privacy-callhistory',
    'ms-settings:privacy-webcam',
    'ms-settings:privacy-contacts',
    'ms-settings:privacy-documents',
    'ms-settings:privacy-downloadsfolder',
    'ms-settings:privacy-email',
    'ms-settings:privacy-eyetracker',
    'ms-settings:privacy-feedback',
    'ms-settings:privacy-broadfilesystemaccess',
    'ms-settings:privacy or ms-settings:privacy-general',
    'ms-settings:privacy-graphicscaptureprogrammatic',
    'ms-settings:privacy-graphicscapturewithoutborder',
    'ms-settings:privacy-speechtyping',
    'ms-settings:privacy-location',
    'ms-settings:privacy-messaging',
    'ms-settings:privacy-microphone',
    'ms-settings:privacy-motion',
    'ms-settings:privacy-musiclibrary',
    'ms-settings:privacy-notifications',
    'ms-settings:privacy-customdevices',
    'ms-settings:privacy-phonecalls',
    'ms-settings:privacy-pictures',
    'ms-settings:privacy-radios',
    'ms-settings:privacy-speech',
    'ms-settings:privacy-tasks',
    'ms-settings:privacy-videos',
    'ms-settings:privacy-voiceactivation'
];

  ms13NameArray: TArray<string> = [
    'Search',
    'Search more details',
    'Search Permissions'
];

  ms13CmdArray: TArray<string> = [
    'ms-settings:search',
    'ms-settings:search-moredetails',
    'ms-settings:search-permissions'
];

  ms14NameArray: TArray<string> = [
    'Accounts',
    'Session cleanup',
    'Team Conferencing',
    'Team device management',
    'Welcome screen'
];

  ms14CmdArray: TArray<string> = [
    'ms-settings:surfacehub-accounts',
    'ms-settings:surfacehub-sessioncleanup',
    'ms-settings:surfacehub-calling',
    'ms-settings:surfacehub-devicemanagenent',
    'ms-settings:surfacehub-welcome'
];

  ms15NameArray: TArray<string> = [
    'About',
    'Advanced display settings',
    'App volume and device preferences',
    'Battery Saver',
    'Battery Saver settings',
    'Battery use',
    'Clipboard',
    'Display',
    'Default Save Locations',
    'Display',
    'Duplicating my display',
    'During these hours',
    'Encryption',
    'Focus assist',
    'Graphics Settings',
    'Graphics Default Settings',
    'Multitasking',
    'Multitasking (sgupdate)',
    'Night light settings',
    'Projecting to this PC',
    'Shared experiences',
    'Tablet mode (not Win11)',
    'Taskbar',
    'Notifications & actions',
    'Remote Desktop',
    'Power & sleep',
    'Sound',
    'Sound devices',
    'Storage',
    'Storage Sense',
    'Storage recommendations',
    'Disks & volumes'
];

  ms15CmdArray: TArray<string> = [
    'ms-settings:about',
    'ms-settings:display-advanced',
    'ms-settings:apps-volume',
    'ms-settings:batterysaver',
    'ms-settings:batterysaver-settings',
    'ms-settings:batterysaver-usagedetails',
    'ms-settings:clipboard',
    'ms-settings:display',
    'ms-settings:savelocations',
    'ms-settings:screenrotation',
    'ms-settings:quietmomentspresentation',
    'ms-settings:quietmomentsscheduled',
    'ms-settings:deviceencryption',
    'ms-settings:quiethours',
    'ms-settings:display-advancedgraphics',
    'ms-settings:display-advancedgraphics-default',
    'ms-settings:multitasking',
    'ms-settings:multitasking-sgupdate',
    'ms-settings:nightlight',
    'ms-settings:project',
    'ms-settings:crossdevice',
    'ms-settings:tabletmode',
    'ms-settings:taskbar',
    'ms-settings:notifications',
    'ms-settings:remotedesktop',
    'ms-settings:powersleep',
    'ms-settings:sound',
    'ms-settings:sound-devices',
    'ms-settings:storagesense',
    'ms-settings:storagepolicies',
    'ms-settings:storagerecommendations',
    'ms-settings:disksandvolumes'
];

  ms16NameArray: TArray<string> = [
    'Date & time',
    'Japan IME settings',
    'Region',
    'Language',
    'Pinyin IME settings',
    'Speech',
    'Wubi IME settings',
    'Add display language',
    'Language options',
    'Set display language'
];

  ms16CmdArray: TArray<string> = [
    'ms-settings:dateandtime',
    'ms-settings:regionlanguage-jpnime',
    'ms-settings:regionformatting',
    'ms-settings:regionlanguage',
    'ms-settings:regionlanguage-chsime-pinyin',
    'ms-settings:speech',
    'ms-settings:regionlanguage-chsime-wubi',
    'ms-settings:regionlanguage-adddisplaylanguage',
    'ms-settings:regionlanguage-languageoptions',
    'ms-settings:regionlanguage-setdisplaylanguage'
];

  ms17NameArray: TArray<string> = [
    'Activation',
    'Backup (not Win11)',
    'Delivery Optimization',
    'Find My Device',
    'For developers',
    'Recovery',
    'Launch Security Key Enrollment',
    'Troubleshoot',
    'Windows Security',
    'Windows Insider Program',
    'Windows Update',
    'Windows Update-Active hours',
    'Windows Update-Advanced options',
    'Windows Update-Optional updates',
    'Windows Update-Restart options',
    'Windows Update-Seeker on demand',
    'Windows Update-View update history'
];

  ms17CmdArray: TArray<string> = [
    'ms-settings:activation',
    'ms-settings:backup',
    'ms-settings:delivery-optimization',
    'ms-settings:findmydevice',
    'ms-settings:developers',
    'ms-settings:recovery',
    'ms-settings:signinoptions-launchsecuritykeyenrollment',
    'ms-settings:troubleshoot',
    'ms-settings:windowsdefender',
    'ms-settings:windowsinsider',
    'ms-settings:windowsupdate',
    'ms-settings:windowsupdate-activehours',
    'ms-settings:windowsupdate-options',
    'ms-settings:windowsupdate-optionalupdates',
    'ms-settings:windowsupdate-restartoptions',
    'ms-settings:windowsupdate-seekerondemand',
    'ms-settings:windowsupdate-history'
];

implementation
end.
