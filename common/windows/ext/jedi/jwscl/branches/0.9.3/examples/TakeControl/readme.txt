This demo moves or copies files (found by a filemask) from a source to
target directory. If the application cannot access the file, it takes
the ownership and rearranges the security to acess the file (only in move mode!).
The directory structure will be remained so the targets can be copied back.
If you copy them back, usually the files will get back their original
security descriptor, because they are not marked as protected. But it does not
mean that the owner of the file is changed. It will remain the group Administrators.
In this way, only Administrators can access the files. Previously it was
"Trusted Installer" only.

Use it at your own risk.


The unit file AdvFileSearch can be found on the internet or received by us :
http://blog.delphi-jedi.net/about/

Blog:
http://blog.delphi-jedi.net/2009/03/10/takecontrol-of-your-files/


