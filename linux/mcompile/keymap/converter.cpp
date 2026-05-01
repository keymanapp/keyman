#include "mcompile.h"

/**
 * @brief  main function for mcompile for Linux
 * @param  argc number of commandline arguments
 * @param  argv pointer to commandline arguments: executable, inputfile, outputfile
 * @return 0 on success
 */

  int main(int argc, char* argv[]) {


  int bDeadkeyConversion = 0;

  if (argc > 1)
    bDeadkeyConversion = (strcmp(argv[1], "-d") == 0);  // I4552

  int n = (bDeadkeyConversion ? 2 : 1);

  if (argc < 3 || argc > 4 || (argc - n) != 2) {  // I4273// I4273
    printf(
        "Usage:  \tmcompile [-d] infile.kmx outfile.kmx\n"
        "        \tmcompile converts a Keyman mnemonic layout to\n"
        "        \ta positional one based on the currently used \n"
        "        \tLinux keyboard layout\n"
        "        \t(-d convert deadkeys to plain keys) \n \n");  // I4552

    return 1;
  }

  // -u option is not available for Linux and macOS

  KMX_CHAR* infile = argv[n];
  KMX_CHAR* outfile = argv[n + 1];

  printf("mcompile%s \"%s\" \"%s\"\n", bDeadkeyConversion ? " -d" : "", infile, outfile);  // I4174

  // 1. Load the keyman keyboard file

  // 2. For each key on the system layout, determine its output character and perform a
  //    1-1 replacement on the keyman keyboard of that character with the base VK + shift
  //    state.  This fixup will transform the char to a vk, which will avoid any issues
  //    with the key.
  //
  //
  //  For each deadkey, we need to determine its possible outputs.  Then we generate a VK
  //  rule for that deadkey, e.g. [K_LBRKT] > dk(c101)
  //
  //  Next, update each rule that references the output from that deadkey to add an extra
  //  context deadkey at the end of the context match, e.g. 'a' dk(c101) + [K_SPACE] > 'b'.
  //  This will require a memory layout change for the .kmx file, plus fixups on the
  //  context+output index offsets
  //
  //  --> virtual character keys
  //
  //  [CTRL ' '] : we look at the character, and replace it in the same way, but merely
  //  switch the shift state from the VIRTUALCHARKEY to VIRTUALKEY, without changing any
  //  other properties of the key.
  //
  // 3. Write the new keyman keyboard file

  LPKMX_KEYBOARD kmxfile;

  if (!KMX_LoadKeyboard(infile, &kmxfile)) {
    KMX_LogError(L"Failed to load keyboard (%d)\n", errno);
    return 3;
  }

  if (KMX_DoConvert(kmxfile, bDeadkeyConversion, argc, (gchar**)argv)) {  // I4552F
    if(!KMX_SaveKeyboard(kmxfile, outfile)) {
      KMX_LogError(L"Failed to save keyboard (%d)\n", errno);
      return 3;
    }
  }

  delete kmxfile;
  return 0;
}
