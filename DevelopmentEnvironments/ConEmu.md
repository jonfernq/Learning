## ConEmu Terminal Emulator (Windows) 

> I am using the ConEmu terminal emulator for windows , i heard it supports multiple tabbed terminals. How do you create them ? 

Yes, ConEmu is a terminal emulator for Windows that supports multiple tabbed terminals. Here's how to create new tabs:

Open ConEmu by double-clicking the executable or launching it from the Start menu.

Once ConEmu is open, press Ctrl+T to create a new tab. You can also click the "+" button in the upper-right corner of the window.

To switch between tabs, you can use the Ctrl+Tab or Ctrl+Shift+Tab keyboard shortcuts. You can also click on the tabs at the top of the window to switch between them.

To close a tab, click the "X" button on the tab or press Ctrl+W.

If you need to split a tab into multiple panes, you can do so by right-clicking on the tab and selecting "Split to right" or "Split to bottom". This will create a new pane alongside the current one, allowing you to view multiple terminals at once.

Note that ConEmu also supports a wide range of customization options, including customizable keyboard shortcuts, color schemes, and font settings. You can access these settings by clicking on the "Settings" button in the toolbar or by pressing Win+Alt+P.

> Many Python pip installs fail because I do not have administrator permissions ln the ConEmu termianl emulator like I do in the Windows command window that I used before? How do I get Windows Administrator level permissions in ConEmu?    

If you are encountering issues with installing Python packages via pip because you don't have administrator permissions in ConEmu, you can try running ConEmu as an administrator. Here's how:

Close any open instances of ConEmu.

Right-click on the ConEmu executable or shortcut and select "Run as administrator". If you're using the Windows Start menu to launch ConEmu, you can right-click on the ConEmu icon and select "Run as administrator" from there.

Once ConEmu is open with administrator permissions, you should be able to install Python packages via pip without encountering permission errors.

Note that running ConEmu as an administrator may not be necessary in all cases, and you should exercise caution when granting elevated permissions to any application. Only run ConEmu as an administrator if you are certain that you need those privileges to perform the actions you want to perform.

> To find the directory with the ConEmu executable, what Windows command can I execute ? 

You can use the where command in Windows to find the directory with the ConEmu executable. Here's how:

Open a command prompt or PowerShell window in Windows.

Type the following command and press Enter: where conemu.exe

This will search your system for the ConEmu executable and display the path to the file in the output. If ConEmu is installed on your system, you should see something like: C:\Program Files\ConEmu\ConEmu\conemu.exe

Note that the location of the ConEmu executable may vary depending on how you installed it and what version you're using. If the where command doesn't find ConEmu on your system, you may need to try a different method for locating the executable.

> For the command in Windows command shell: "where ConEmu.exe" I get error: INFO: Could not find files for the given pattern(s).

If you're getting the error message "Could not find files for the given pattern(s)" when you try to run the where ConEmu.exe command in the Windows command shell, it's possible that the ConEmu executable is not on your system's PATH environment variable.

Anyway, the ConEMu settings page shows where it is installed at the top: 

![conemusettings](https://user-images.githubusercontent.com/68504324/224462162-f7ae1ee1-49cb-414c-baa9-c8eca41f31b8.jpg)




