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

1. Move both of the above compilers into the keyman repo
2. Split .kps and model compilers into separate paths
3. Update lexical-models repo to pull these compiler(s)
4. Refactor compiler code to address https://github.com/keymanapp/lexical-models/issues/28 and https://github.com/keymanapp/lexical-models/issues/29