# kmdecomp Keyman Decompiler

This unsupported utility decompiles a Keyman .kmx keyboard. 
It will produce a .kmn source file and optionally a .ico or
.bmp image. The source file may not contain symbols and will
include optimisations that were made by the compiler such as 
expanding `any()` rules in key components into multiple rules.

Usage: kmdecomp <infile.kmx>
