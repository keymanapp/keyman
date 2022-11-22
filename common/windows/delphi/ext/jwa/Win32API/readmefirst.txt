This is the try to include all converted header files into one unit.

PLEASE READ THIS FIRST!

Be aware that there are some already created packages in the folder: \trunk\packages
You can use them in your Delphi version. Older packages can be converted by newer Delphi versions
so it is not a problem that there are many empty Delphi version folders. 
Just use an old package version with your Delphi and let it recreate then copy it to a folder that describes the version correctly.
You can help make JWA better by sending us the Delphi version package.

You can get a deeper understanding if you continue reading....


To compile the bunch of files do these simple steps:

1. create a package with a name and location of your choice
2. add the file jwaWindows.pas - and only this file to the package (except jwaVista.pas for Vista structures)
3. add source path to common files (usually located in a folder named "Common")
4. Choose between dynamic or static binding by set the compiler switch
 DYNAMIC_LINK or unset it. You can change this in jwaWindows.pas. Open it to edit.
 You can also set WINXXX, where XXX is a Windows version to include or exclude
 functions (un)supported by Windows versions. This is done in includes\jediapilib.inc
 
5. Choose an output path for the files that will be created by Delphi. You can change it in the setting/options dialog of the package.
7. Also set the options optimization and debugger option in the setting page as you want.
8. Compile the package. It will need its time.
9. If everything went fine you'll find a file called jwaWindows.dcu (jwaVista.dcu) that you can use in your programs. Remember each delphi version needs its own version of that file. 
You have to repeat the compilation in each delphi version you want to use.
The advantage is that you can rebuild your project without rebuilding all API files.

To use jwaWindows.dcu you can copy it in your project folder or
Add a search path into your project settings. Delphi comes with a global path settings that would be the right choice here.
You can change it in menu Tools->Environment Options-> tab Library
-> Library path.

If you also add the source file folder to the debug path in your project/global settings, you can browse the source code without compiling it.


The includes\jediapilib.inc contains some compiler directives (like WINXP, WINVISTA) that shows or hides function declarations that are only
available on these plattforms. Make sure you use them wisely. Especially you can prevent your application from starting if you use static
function binding with these functions. Your app will not start if you use a Vista only function because it is unknown to Windows XP or even 2000.
Windows 2003 also contains some functions that are not available to Xp. Windows XP contains a known function (WNetRestoreConnectionW) that is no longer 
available in Windows Vista. 

Using dynamic binding is a safe way to make your applications startable under all Windows versions. However your problem will only be postponed
to a later failure point. You must make sure that these functions are supported by the operation system where your app is intended to be run.
The MSDN provides information wich OS supports a particular function. 


Additional information can be found at the mailing list
http://sourceforge.net/mailarchive/forum.php?thread_name=4759DDA0.7080801%40gmx.de&forum_name=jedi-apilib-wscl

Subscribe here:
http://sourceforge.net/mail/?group_id=121894


December 2007/2008
Christian Wimmer (mail(at)delphi-jedi(dot)net) 



