#if !defined (__XKBMAP_H)
#define __XKBMAP_H
#include <string>
#include <vector>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/XKBlib.h>
#include <X11/extensions/XKBfile.h>
#include <X11/extensions/XKBconfig.h>
#include <X11/extensions/XKBrules.h>


class Xkbmap
{
private:
    Display *		dpy;

    enum svSources {
		FROM_SERVER	= 0,
		FROM_RULES	= 1,
		FROM_CMD_LINE	= 2,
    };
    
    enum valueIndices {
		RULES_NDX	= 0,
		DISPLAY_NDX	= 1,
		LOCALE_NDX	= 2,
		MODEL_NDX	= 3,
		LAYOUT_NDX	= 4,
		VARIANT_NDX	= 5,
		KEYCODES_NDX	= 6,
		TYPES_NDX	= 7,
		COMPAT_NDX	= 8,
		SYMBOLS_NDX	= 9,
		GEOMETRY_NDX	= 10,
		KEYMAP_NDX	= 11,
		NUM_STRING_VALS	= 12,
    };
	const std::string defaultlayout;
	const std::string unknownsymbols;
    svSources svSrc[NUM_STRING_VALS];
    char * svValue[NUM_STRING_VALS];
    
    XkbRF_VarDefsRec	rdefs;
    
    std::vector < std::string > options;    
    std::vector < std::string > inclPath;
    
    
	void clearValues(void);
	void trySetString(valueIndices which, const char *newVal, svSources src);
	Bool getDisplay(void);
	Bool getServerValues(void);
	void addStringToOptions(char *opt_str);
	char * stringFromOptions(char *orig);
	Bool applyRules(void);
	Bool checkName(char *name, const char* string);
	Bool applyComponentNames(void);

public:
    Xkbmap();
    ~Xkbmap();
	std::string getCurrentLayout(void);
	void setLayout(const std::string & layout);
	std::string getCurrentSymbols(void);
	void setSymbols(const std::string & symbols);
};
#endif
/*
vi:ts=4:nowrap:ai:expandtab
*/
