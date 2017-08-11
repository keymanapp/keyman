This sample demonstrates the components required to create a custom product with
your own user interface:

* laokeys.kmn: a sample keyboard for Lao - please note, this keyboard is
  not complete, being purely an example; if you need a complete Lao 
  solution, please see www.laoscript.net!

* laokeys.kps: contains laokeys.kmx and a font (Saysettha OT).

* laokeys.kct: a branding file that is setup to reference laounicode.exe

* laokeys.kpp: sample product source file

* laounicode.exe: a VB.NET application that replaces kmshell.exe

* laounicode.zip: source for laounicode.exe

Please note - to compile this keyboard, use the following login:
   email:    keymandevsamples@tavultesoft.com 
   password: T9myn6H2

This sample should not be redistributed as is!

If you reset the product ID in the laokeys.kct file, you will need to
modify the source to laounicode.exe to reference the correct product ID,
or the product will not start.
