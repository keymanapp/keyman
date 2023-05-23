#include "keymap.h"

static void PrintKeymapForCode(GdkKeymap *keymap, guint keycode)
{
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return;

  for (int i = 0; i < count; i++) {
    if (maps[i].level > 1 || maps[i].group > 4)
      continue;
    printf("    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n", i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
  }


  g_free(keyvals);
  g_free(maps);
}

v_str_3D write_US_ToVector(std::string language, const char* text) {

  printf("+++++++ start  to open_file \n");
  v_str_3D Vector_split;
  std::string FullPathName = "/usr/share/X11/xkb/symbols/" + language;

  const char* cc = FullPathName.c_str();
  FILE* fp = fopen((cc), "r");
  if ( !fp)
    printf("could not open file!");
  else
    printf("+++++++ file open OK \n");

  v_str_1D Vector_completeUS = CreateCompleteRow_US(fp, text, language); // creates vector of all <keys> . TTODO lookup other language files

  //here split Vector to 3D vector
  Vector_split = SplitTo_3D_Vector( Vector_completeUS);

  printf("+++++++ sizes write_US_ToVector %li..%li..%li\n", Vector_split.size(), Vector_split[0].size(),Vector_split[0][0].size());
  fclose(fp);
  return Vector_split;
}

bool test(v_str_3D V){
  printf("+++++++ stest");
std::cout << V[0][0][0]<< "..\n" 
          << V[0][0][1]<< "..\n" 
          << V[0][0][2]<< "..\n" 
          << V[0][0][3]<< "..\n" 
          << V[0][0][4]<< "..\n\n" 

          << V[0][1][0]<< "..\n" 
          << V[0][1][1]<< "..\n" 
          << V[0][1][2]<< "..\n" 
          << V[0][1][3]<< "..\n" 
          << V[0][1][4]<< "..\n\n" 

          << V[0][22][0]<< "..\n" 
          << V[0][22][1]<< "..\n" 
          << V[0][22][2]<< "..\n" 
          << V[0][22][3]<< "..\n" 
          << V[0][22][4]<< "..\n\n" 

           << V[0][45][0]<< "..\n" 
          << V[0][45][1]<< "..\n" 
          << V[0][45][2]<< "..\n" 
          << V[0][45][3]<< "..\n" 
          << V[0][45][4]<< "..\n\n" 

          << V[0][46][0]<< "..\n" 
          << V[0][46][1]<< "..\n" 
          << V[0][46][2]<< "..\n" 
          << V[0][46][3]<< "..\n" 
          << V[0][46][4]<< "..\n\n" 
         
          << V[0][0][0]<< "..\n";
          return true;
}

v_str_1D CreateCompleteRow_US(FILE* fpp, const char* text, std::string language) {
  printf("+++++++ start CreateCompleteRow_US\n");
    v_str_1D complete_List;
    int buffer_size = 512;
    char buffer[buffer_size];
    bool print_OK   = false;
    const char* key = "key <";
    std::string str_txt(text);
    std::string xbk_mark = "xkb_symbol";
    std::ofstream KeyboardFile("File_" + language + ".txt");

    printf("Keyboard %s\n", text);
    KeyboardFile << "Keyboard" << text << "\n";

    if (fpp) {
      while (fgets(buffer, buffer_size, fpp) != NULL) {
        std::string str_buf(buffer);

        // Maybe TODO: recursive search for other included Configuration files
        // if find "include"  -> recursive...

        // stop when finding the mark xkb_symbol
        if (std::string(str_buf).find(xbk_mark) != std::string::npos)
          print_OK = false;

        // start when finding the mark xkb_symbol + correct layout
        if ((std::string(str_buf).find(str_txt) != std::string::npos))
          print_OK = true;

        if ((print_OK) && (std::string(str_buf).find(key) != std::string::npos)) {
          printf("... %s", buffer);
          complete_List.push_back(buffer);
          KeyboardFile << buffer;
        }
      }
    }
    printf("end Keyboard %s..........................................\n\n", text);
    return complete_List;
}

void extract_difference(v_str_3D, std::string lang_Other ) 
{
  printf("+++++++ start extract_difference\n");
  /*
    std::ofstream Map_File("Map_" + lang_US + "_To_" +  lang_Other + ".txt");
    std::string diff;
    std::cout <<"key"<< std::setw(26)<< "Char US"<< std::setw(20)<<"Char other" << std::setw(10)<< "diff? \n";
    Map_File  <<"key"<< std::setw(26)<< "Char US"<< std::setw(20)<<"Char other" << std::setw(10)<< "diff? \n";
    std::cout <<"---------------------------------------------------------------\n";
    Map_File  <<"---------------------------------------------------------------\n";

    //ToDo for all columns; watchout for empty fields!!
    for ( int i =0; i< US_vector.size();i++)
    {
        for ( int j =0; j< other_vector.size();j++)
        {
            diff =" \t";
            if( US_vector[i][0]  == other_vector[j][0] )
            {
                if( US_vector[i][1]  != other_vector[j][1] )   diff = "   *** ";
                std::cout << US_vector[i][0] << std::setw(20)<< US_vector[i][1] << std::setw(20)<< other_vector[j][1]  <<diff<<"\r\n";
                Map_File  << US_vector[i][0] << std::setw(20)<< US_vector[i][1] << std::setw(20)<< other_vector[j][1]  <<diff<<"\r\n";
                break;
            }
        }
    }
    Map_File.close();*/
}

v_str_3D SplitTo_3D_Vector(v_str_1D p_completeList) {std::vector<char> delim{' ', '[', ']', '}', ';', '\t', '\n'};
  printf("+++++++ start SplitTo_3D_Vector\n");
  char split_bracel = '{';
  char split_char_komma  = ',';
  std::vector<std::string> tokens;
  std::vector<std::vector<std::string> > everything;  
  v_str_3D all;

  // while starts with key...
  for (int k = 0; k < (int)p_completeList.size() - 1; k++) {

    // remove alll unwanted char
    for (int i = 0; i < (int) delim.size(); i++)
      p_completeList[k].erase(remove(p_completeList[k].begin(), p_completeList[k].end(), delim[i]), p_completeList[k].end());

    // inly lines with ("key<.. are of interest
    if (p_completeList[k].find("key<") != std::string::npos) {
      // seperate key<...
      std::istringstream split1(p_completeList[k]);
      for (std::string each; std::getline(split1, each, split_bracel); tokens.push_back(each));

      // seperate rest with comma and push to everything
      std::istringstream split(tokens[1]);
      tokens.pop_back();
      for (std::string each; std::getline(split, each, split_char_komma); tokens.push_back(each));
      everything.push_back(tokens);
      tokens.clear();
    }
  }
  //return everything;
  all.push_back(everything);
  //printf("+++++++ ssizes %i..%i..%i\n", all.size(), all[0].size ,all[0][0].size);
  printf("+++++++ sizes SplitTo_3D_Vector %li..%li..%li\n", all.size(), all[0].size(),all[0][0].size());
  return all;
}

//--------------------------------------
int main(gint argc, gchar **argv)
{
  gdk_init(&argc, &argv);
  GdkDisplay *display = gdk_display_get_default();
  if (!display) {
    printf("ERROR: can't get display\n");
    return 1;
  }
  GdkKeymap *keymap = gdk_keymap_get_for_display(display);
  if (!keymap) {
    printf("ERROR: Can't get keymap\n");
    gdk_display_close(display);
    return 2;
  }

  for (int keycode = 10; keycode <= 61; keycode++) {
    printf("-------------------\n");
    printf("Keycode %d:\n", keycode);
    PrintKeymapForCode(keymap, keycode);
  }
//------------------------------------------
//GdkKeymap* kmp;
//kmp = gdk_get_default();


  std::string US_language    = "us";
  const char* text_us        = "xkb_symbols \"intl\"";

  std::string other_language = "--";
  // ToDo get other/current language from XKB
  // e.g. other_language = 2;
printf("-°°°°°°°° write_US_ToVector\n");
  v_str_3D All_Vector = write_US_ToVector(US_language, text_us);

printf("-°°°°°°°° test\n");
  //bool ok = test(All_Vector);

printf("-°°°°°°°° extract_difference\n");
  extract_difference(All_Vector,other_language);

//------------------------------------------




  gdk_display_close(display);

  return 0;
}
