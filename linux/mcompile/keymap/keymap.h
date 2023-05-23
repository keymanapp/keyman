
#include <gdk/gdk.h>

#include <string>
#include <iostream>
#include <vector>
#include <fstream>

static void PrintKeymapForCode(GdkKeymap *keymap, guint keycode);
std::vector<std::vector<std::vector<std::string> > > write_US_ToVector(std::string language, const char* text) ;
std::vector<std::string> CreateCompleteRow_US(FILE* fpp, const char* text, std::string language) ;
void extract_difference(std::vector<std::vector<std::vector<std::string>  >  > US_vector, std::string lang_Other ) ;
