
#include <gdk/gdk.h>

#include <string>
#include <iostream>
#include <vector>
#include <fstream>
#include <iomanip>
#include <algorithm>

typedef std::vector<std::string>                             v_str_1D;
typedef std::vector<std::vector<std::string> >               v_str_2D;
typedef std::vector<std::vector<std::vector<std::string> > > v_str_3D;

static void PrintKeymapForCode(GdkKeymap *keymap, guint keycode);

v_str_3D write_US_ToVector(std::string language, const char* text) ;
v_str_1D CreateCompleteRow_US(FILE* fpp, const char* text, std::string language) ;
void extract_difference(v_str_3D US_vector, std::string lang_Other ) ;
v_str_3D SplitTo_3D_Vector(v_str_1D completeListUS);
