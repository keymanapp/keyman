# Keyman Developer

This is the likely future home for Keyman Developer. However, at this 
time, Keyman Developer still lives in windows/src/developer/

This folder currently contains only the Lexical Model Compiler and the
related Package Compiler. It runs on nodeJS on all supported desktop
platforms.

# Lexical Model Compiler


# Package Compiler

The package compiler is broadly compatible with the kmcomp .kps
package compiler. However at this stage it is only tested with 
lexical models, and use with keyboards (either .js or .kmx) is not
tested or supported. It is likely in the future that the kmcomp
.kps compiler will be deprecated in favour of this one.

# Transition steps

1. Split .kps and model compilers into separate paths

# Folders

## inst

Contains a minimal distribution of node.js intended just for compiling
lexical models. If more  dependencies are required, then the 
developer will be expected to install node.js themselves; this gets 
users going without requiring a large installer or a full node.js 
install.

## js

Source files for node-based compilers.