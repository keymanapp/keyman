#include "keymap.h"

static void PrintKeymapForCode(GdkKeymap *keymap, guint keycode)
{
  GdkKeymapKey *maps;
  guint *keyvals;
  gint count;

  if (!gdk_keymap_get_entries_for_keycode(keymap, keycode, &maps, &keyvals, &count))
    return;

  for (int i = 0; i < count; i++) {
    if (maps[i].level > 0 || maps[i].group > 1)
      continue;
    printf("    i=%d, keycode=%d, keyval=%d (%c), level=%d, group=%d\n", i, maps[i].keycode, keyvals[i], keyvals[i], maps[i].level, maps[i].group);
  }

  g_free(keyvals);
  g_free(maps);
}

std::vector<std::vector<std::vector<std::string> > > write_US_ToVector(std::string language, const char* text) {
  printf("+++++++ start  to open_file \n");
  std::vector<std::vector<std::vector<std::string> > > Vector_split;
  std::string FullPathName = "/usr/share/X11/xkb/symbols/" + language;
  const char* cc = FullPathName.c_str();
  FILE* fp = fopen((cc), "r");
  if ( !fp)
    printf("could not open file!");
  else
    printf("+++++++ file open OK \n");
  std::vector<std::string> Vector_complete = CreateCompleteRow_US(fp, text, language); // creates vector of all <keys> . TTODO lookup other language files

  fclose(fp);
  return Vector_split;
}

std::vector<std::string> CreateCompleteRow_US(FILE* fpp, const char* text, std::string language) {
  printf("+++++++ start CreateCompleteRow_US\n");
    std::vector<std::string> complete_List;
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

        // TODO: recursive search for other included Configuration files
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

void extract_difference(std::vector<std::vector<std::vector<std::string>  >  > US_vector, std::string lang_Other ) 
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

  std::string US_language    = "us";
  const char* text_us        = "xkb_symbols \"intl\"";

  std::string other_language = "--";
  // ToDo get other/current language from XKB
  // e.g. other_language = 2;

  std::vector<std::vector<std::vector<std::string> >  > All_Vector = write_US_ToVector(US_language, text_us);
  extract_difference(All_Vector,other_language);

//------------------------------------------




  gdk_display_close(display);

  return 0;
}
