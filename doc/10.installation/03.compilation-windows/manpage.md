---
title: Compilation (Windows) OUT-OF-DATE
taxonomy:
  category: docs
template: docs
docroot: /aten/docs
header_class: alt
---

Note that these instructions were written from the perspective of a Windows 7 system. It’s likely that the procedure for other versions will be similar, but might need tweaking in places. This

## Install Prerequisites

### Visual Studio C++ 2010

Get the C++ version of Visual Studio from [a]http://www.microsoft.com/express/Windows/,http://www.microsoft.com/express/Windows/[/a] – at the time of writing this was version 10.0, but earlier versions 8.0 and 9.0 are also fine). This will download a small web installer to your machine called [name]vc_web.exe[/name]. Run this, and accept the license. You may choose not to install Silverlight since it is not necessary for **Aten**. Remember where the installation location is set to (by default it is [value]C:\Program Files\Microsoft Visual Studio 10.0\[/value]) because you’ll need this later on for the installation of Windows PowerShell. Chances are you’ll need to restart your machine after the installation.

### Install the Windows SDK

Get the Windows SDK from [a]http://msdn.microsoft.com/en-us/windows/bb980924,http://msdn.microsoft.com/en-us/windows/bb980924[/a] (or search for ‘Windows SDK’ on the internet). Note that the download is described as being for 'Windows 7 and .NET' but this is fine since it’s backwardly compatible with XP. Run the installer ([name]winsdk_web.exe[/name]) and accept the license, and again take note of the installation directories (since they need to be provided in the PowerShell setup later on). The default installation options are fine, although you can uncheck the installation of ‘Samples’ since they are not required. Once installation has finished, the Windows Help Centre may pop up and ask where your local resources are. This can safely be canceled.

### Install CMake

Download the lastest CMake installer from [a]http://www.cmake.org/,http://www.cmake.org/[/a] (version 2.8.4 at the time or writing) and install it. Make sure you choose to add CMake to the PATH for all users when running the installation.

### Install Readline

Go to [a]http://gnuwin32.sourceforge.net/packages/readline.htm,http://gnuwin32.sourceforge.net/packages/readline.htm[/a] (or search for ‘Windows Readline’ on the internet) and download the 'Complete package, except sources' installer (around 2.3 Mb). If you choose to install somewhere other than the default location, you’ll need to tweak the PowerShell profile given in Step 7.

### Download GLext Header

Go to [a]http://www.opengl.org/registry/api/glext.h,http://www.opengl.org/registry/api/glext.h[/a] and save the page as ‘glext.h’ somwhere like your ‘My Documents’ directory. Again, if you choose somewhere other than this location, you’ll need to tweak the PowerShell profile given in Step 7.

## Download and Unpack Qt4 Source

Go to [a]http://qt.nokia.com/downloads,http://qt.nokia.com/downloads[/a] and download the LGPL package for Windows (approx 322 Mb) and run the installer file. Again, you may choose where to unpack the files to (the default is [value]C:\Qt\2010.05[/value] for the release downloaded here) but if you change the default you’ll need to modify the relevant paths accordingly in Step 7. You may choose not to install the MinGW part of the package since we will be using Visual Studio for the compilation. There is no need to run Qt Creator once the installation is finished.

### Windows PowerShell

Download PowerShell from [a]http://support.microsoft.com/kb/968930,http://support.microsoft.com/kb/968930[/a]. Once installed, run PowerShell (its Start Menu entry is typically placed inside the 'Accessories' folder) and you should be presented with a blue shell, starting inside your Documents and Settings folder ([value]C:\Documents and Settings\Your Name[/value]). First thing is to set up your profile with the relevant paths set so we can find the Visual Studio, readline, and Qt4 files. Such settings are stored in a file which doesn’t yet exist, and which is referenced by the environment variable [var]$profile[/var]. You can type this into PowerShell and see exactly where it points to:

<command style="win">$profile</command>

This will output something along the lines of:

<code>C:\Documents and Settings\Your Name\My Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1</code>

We must first create the directory where this file is expected to be:

<command style="win">mkdir 'My Documents/WindowsPowerShell'</command>

which outputs...

```
    Directory: C:\Documents and Settings\Your Name\My Documents

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----        23/04/2011     11:05            WindowsPowerShell
```

<command style="win">ls 'My Documents'</command>

```
	Directory: C:\Documents and Settings\Your Name\My Documents

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----        23/04/2011     10:49            Downloads
d-r--        10/07/2009     23:41            My Music
d-r--        08/04/2010     16:35            My Pictures
d----        14/03/2011     08:47            My Received Files
d-r--        26/06/2010     19:44            My Videos
d----        23/04/2011     11:05            WindowsPowerShell
-a---        23/04/2011     17:43     637740 glext.h
```

You can then create and edit the profile file directly with Notepad:

<command style="win">notepad $profile</command>

In the empty file, paste the following into it and adjust any paths/versions as may be necessary.

```
# Setup Visual Studio and Windows SDK environment variables

if ( test-path -path $env:VS100COMNTOOLS )
{
  echo 'Setting Visual Studio environment'
  $VCSTUDIO='C:\Program Files\Microsoft Visual Studio 10.0'
  $WINSDK='C:\Program Files\Microsoft SDKs\Windows\v7.1'

  # System variables
  $env:VSINSTALLDIR = '$VCSTUDIO'
  $env:VCINSTALLDIR = '$VCSTUDIO\VC'
  $env:FrameworkDir = 'C:\Windows\Microsoft.NET\Framework'
  $env:FrameworkVersion = 'v2.0.50727'
  $env:FrameworkSDKDir = '$VCSTUDIO\SDK\v3.5'
  $env:DevEnvDir = '$VCSTUDIO\Common7\IDE'

  # Executable path
  $env:PATH += ';$VCSTUDIO\Common7\IDE'
  $env:PATH += ';$VCSTUDIO\VC\BIN'
  $env:PATH += ';$VCSTUDIO\Common7\Tools'
  $env:PATH += ';$VCSTUDIO\Common7\Tools\bin'
  $env:PATH += ';$VCSTUDIO\VC\PlatformSDK\bin'
  $env:PATH += ';$VCSTUDIO\SDK\v2.0\bin'
  $env:PATH += ';$VCSTUDIO\VC\VCPackages'
  $env:PATH += ';$WINSDK\Bin'

  # Include directories
  $env:INCLUDE += ';$VCSTUDIO\VC\ATLMFC\INCLUDE'
  $env:INCLUDE += ';$VCSTUDIO\VC\INCLUDE'
  $env:INCLUDE += ';$VCSTUDIO\VC\PlatformSDK\include'
  $env:INCLUDE += ';$VCSTUDIO\SDK\v2.0\include'
  $env:INCLUDE += ';$WINSDK\Include'

  # Libraries
  $env:LIB += ';$VCSTUDIO\VC\ATLMFC\LIB'
  $env:LIB += ';$VCSTUDIO\VC\LIB'
  $env:LIB += ';$VCSTUDIO\VC\PlatformSDK\lib'
  $env:LIB += ';$VCSTUDIO\SDK\v2.0\lib'
  $env:LIB += ';$WINSDK\Lib'
  $env:LIBPATH += ';$VCSTUDIO\VC\ATLMFC\LIB'
}
else
{
  echo 'No Visual Studio installed, or incorrect version string used?'
}

# Setup Qt environment variables
$env:QTDIR='C:\Qt\2010.05'
if ( test-path -path $env:QTDIR )
{
  $env:PATH += ';'+$env:QTDIR+'\qt\bin'
  $env:INCLUDE += ';'+$env:QTDIR+'\qt\include'
  $env:LIB += ';'+$env:QTDIR+'\qt\lib'
}

# Setup GnuWin32 environment variables
$env:INCLUDE += ';C:\Program Files\GnuWin32\include'
$env:LIB += ';C:\Program Files\GnuWin32\lib'
$env:PATH += ';C:\Program Files\GnuWin32\bin'

# Setup custom library and header environment variables
$env:INCLUDE += ';$HOME\My Documents'
```

By default, the running of scripts in PowerShell is (likely to be) disabled, meaning that the profile will not be loaded when PowerShell next starts up. To enable the execution of scripts, run the following command:

<command style="win">set-executionpolicy remotesigned</command>

You need to answer Yes to the question which then pops up. Afterwards, close this shell by typing:

<command style="win">exit</command>

...and then restart PowerShell. Your new profile should now be loaded, and you should see the ‘Setting local environment’ message which we added to it. All being well, there should be no error messages here – if there are, then its likely that there’s a mistake in one of the paths set in the profile.

## Build Qt4

First stage is to configure Qt ready for building by running the following commands:

<command style="win">cd C:\Qt\2010.05\qt</command>

<command style="win">./configure -release -opensource -platform win32-msvc2010 -qt-libjpeg -qt-libmng -qt-libtiff -qt-libpng -qt-zlib -qt-gif -no-webkit -no-script</command>

This will take a few minutes. Once complete, build Qt with:

<command style="win">nmake</command>

This will take a fair amount of time. Best go and do something else for a bit.

## Download and Build **Aten**

Get the zipped copy of the source from the [a]downloads,aten,downloads[/a] section - for this example the latest beta version is 1.7.1626, so the zipfile is called [value]aten-1.7.1626.zip[/value]. Unpack the source somewhere – here it has been placed in the users’s home directory – and run  to generate the necessary makefiles:

<command style="win">cd aten-1.7.1626</command>

<command style="win" path="C:\Documents and Settings\Your Name\aten-1.7.1626">cmake -G "NMake Makefiles"</command>

Then, build **Aten** by running nmake:

<command style="win" path="C:\Documents and Settings\Your Name\aten-1.7.1626">nmake</command>

Time to wait again, but hopefully after a little while you will see something resembling the following (the important part is the [value][100%] Built target **Aten**[/value]):

```
C:\Program Files\Microsoft Visual Studio 10.0\VC\INCLUDE\stdlib.h(433) : see declaration of 'getenv'
C:\Documents and Settings\Your Name\My Documents\aten-1.7.1626\src\main.cpp(52) : warning C4996: 'getenv': This function or variable may be unsafe. Consider using _dupenv_s instead. To disable deprecation, use _CRT_SECURE_NO_WARNINGS. See online help for details.
        C:\Program Files\Microsoft Visual Studio 10.0\VC\INCLUDE\stdlib.h(433) : see declaration of 'getenv'
Linking CXX executable bin\**Aten**.exe
[100%] Built target **Aten**
PS C:\Documents and Settings\Your Name\My Documents\aten-1.7.1626&gt;
```

If this is what you see, then you’ve built **Aten** successfully. You’ll need to point **Aten** to it’s data directory by hand. From the command line, you can run the following:

<command style="win" path="C:\Documents and Settings\Your Name\aten-1.7.1626">.\bin\**Aten**.exe --atendata .\data</command>

You can also set up a shortcut to the executable and set the option there.

## Potential Readline Errors

There is the potential for the build to fail on the basis of odd readline errors:

```
    C:\Program Files\GnuWin32\include\readline/keymaps.h(97) : see declaration of 'rl_set_keymap'
    C:\Program Files\GnuWin32\include\readline/readline.h(364) : error C2375: 'rl_get_keymap' : redefinition; different linkage
    C:\Program Files\GnuWin32\include\readline/keymaps.h(94) : see declaration of 'rl_get_keymap'
NMAKE : fatal error U1077: 'C:\PROGRA~1\MICROS~1.0\VC\bin\cl.exe' : return code '0x2'
Stop.

NMAKE : fatal error U1077: 'C:\Program Files\Microsoft Visual Studio 10.0\VC\BIN\nmake.exe' : return code '0x2'
Stop.

NMAKE : fatal error U1077: 'C:\Program Files\Microsoft Visual Studio 10.0\VC\BIN\nmake.exe' : return code '0x2'
Stop.

PS C:\Documents and Settings\Your Name\My Documents\aten-1.7.1626&gt;
```

These errors arise because the functions [value]rl_*_*[/value] are declared differently between the readline.h and keymaps.h header files, but can be fixed as follows. Since keymaps are not used, the offending lines can be commented out in readline.h. Locate the file (by default, it is installed in [value]C:\Program Files\GnuWin32\include\readline/readline.h[/value]) and edit it with Notepad or something similar. Put a comment marker ([value]/*[/value]) at the very beginning of line 356, and a comment end marker ([value]*/[/value]) at the very end of line 364. Rerun  and everything should be fine.

## Anything Else?

Instead of downloading the zipped source of **Aten**, you could download a Windows subversion client and maintain an up-to-date copy of the source on your machine – useful if you want to frequently get the latest updates. There are many subversion clients available, but Win32svn ([a]http://sourceforge.net/projects/win32svn/,http://sourceforge.net/projects/win32svn/[/a]) works well for me.


