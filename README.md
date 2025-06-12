# CustomMenu

**As of release v2.0.0 you now choose your own custom color scheme**

------

**Quick Tip**: 
- A right-click on your `Custom Menu` or _any_  of your submenus will open the `Config Dialog`
- Also, if you right-click your desktop with no menu items defined and saved, the `Config Dialog` will open

_N.B. you can delete the default menu item that `CustomMenu` created for you in previous versions:_

![custommenurightclickme](https://github.com/BazzaCuda/CustomMenu/assets/22550919/218b82cd-e348-4fe7-937f-796d4c2a2c05)


-----------
_NOW with system-wide [customizable] hotkey activation to allow you to display your custom menu without accessing your desktop! For information on how to activate the hotkey functionality, see the release notes for version 1.0.6 here: https://github.com/BazzaCuda/CustomMenu/releases/tag/v1.0.6_

-----------

**Tip of the Day**: the easiest way to get started with `CustomMenu` is to select all the shortcuts on your desktop and simply drag and drop them onto your empty menu tree (left panel) in the `Config Dialog`. `CustomMenu` will create a menu item for each shortcut complete with the correct icon, starting directory and any parameters. That gets your custom menu up and running instantly with minimal effort. At a later date, if you wish, you can take your time re-arranging them and putting them into suitable submenus.

----------

NOW AVAILABLE FOR DOWNLOAD! (see https://github.com/BazzaCuda/CustomMenu/releases)

Build your own customized Windows desktop menu.  Unlimited submenus with unlimited menu items.

All your favorite applications, folders, Windows settings and Windows applets at your fingertips and easily accessible just by right-clicking on your desktop.

A great alternative to the nightmarish clutter of your Windows Start Menu or even your current Windows desktop context menu.

Great for PC Support technicians! Configure and run from a USB stick containing all your favorite tools.

Many, many exciting features. And it's all free. (see the Wiki for a quick preview).

See the Wiki overview (and screenshots) at https://github.com/BazzaCuda/CustomMenu/wiki/Overview
- CustomMenu Wiki Home [Wiki Home](https://github.com/BazzaCuda/CustomMenu/wiki)
- Overview (screenshots) [Overview](https://github.com/BazzaCuda/CustomMenu/wiki/Overview)
- Getting Started [Getting Started](https://github.com/BazzaCuda/CustomMenu/wiki/Getting-Started)
- Live submenus for files and folders (also see below) [Browsing files and folders](https://github.com/BazzaCuda/CustomMenu/wiki/Browsing-files-and-folders)

Follow and stay tuned for updates.

Comments and suggestions always welcome in https://github.com/BazzaCuda/CustomMenu/discussions

Examples:

![image](https://github.com/user-attachments/assets/27c141b9-cdab-4a7a-b880-994c64dca567)



![example](https://user-images.githubusercontent.com/22550919/209433120-93bdfd3c-6990-4958-8581-75fc001b423a.jpg)




**Creating live browse menus**

- To create such menus, you specify "browse" in the command box.
- The folder to be browsed is specified in the Directory box.
- The Parameters box determines which files and folders will be shown from the folder (default is \*.\*).

![image](https://github.com/user-attachments/assets/2fbb8796-4ffa-469d-b0d3-8a5be16dd42d)


This creates a menu item called `"Browse Downloads\"`. When hovered over with the mouse, a live submenu is displayed showing the contents of the downloads folder:

![image](https://github.com/BazzaCuda/CustomMenu/assets/22550919/6a382453-61ad-4e39-8792-7f86bd8e6e88)

Because "+folders" has been specified, all folders will be displayed and each folder becomes another submenu which can be hovered over:

![image](https://github.com/BazzaCuda/CustomMenu/assets/22550919/92477c7a-5669-471a-8200-8d3805a349c3)

In this example, Custom Menu has been limited to only showing *.pdf and *.epub files. You can also specify `*.*` to show all files (this is the default).



As you move the mouse over the folders, it will very helpfully distinguish between those that have contents [which match your criteria] and those that don't:

![image](https://github.com/BazzaCuda/CustomMenu/assets/22550919/217e3d05-75de-4fa9-98de-d0e635ec6f2a)

One of the best uses of the browse feature is to have menu items set up for the most common drive letters that your Windows system assigns to USB sticks, in this case F:, G:, H: and I:

Create a top level menu item called, e.g. "USB Drives":
![image](https://github.com/BazzaCuda/CustomMenu/assets/22550919/db918b55-7a51-4af7-a176-8ab07db4e18e)

Then have individual "browse" commands for each drive:
![image](https://github.com/BazzaCuda/CustomMenu/assets/22550919/433dcfa8-8984-4224-913c-322583e28f20)

When a USB stick is inserted, a quick hover over the USB drive menu items shows which drives are mounted and which aren't:

![image](https://github.com/BazzaCuda/CustomMenu/assets/22550919/814078df-b058-44df-939f-1b95fee88e2e)

Note that if you save your `Custom Menu` to the Windows Desktop Menu with "Write to Desktop Menu" (which I always do), the "browse" menu items will only work when accessed from `CustomMenu`, not from the Windows menu as Windows doesn't understand what to do with the "browse" command.

Also, if a submenu (especially a browsed folder) has so many items that it requires the scroll buttons to be displayed, you only need to hover your mouse over a scroll button for it to automatically scroll the menu items. Alternatively, you can scroll using the mouse wheel.

![image](https://github.com/BazzaCuda/CustomMenu/assets/22550919/266a506b-766f-4e4a-9340-658e0df88ac7)




.




