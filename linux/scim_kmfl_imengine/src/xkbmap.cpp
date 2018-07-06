/*
 * KMFL Input Method for SCIM (Smart Common Input Method)
 *
 * Copyright (C) 2005 SIL International
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <iomanip>
#include <list>
#include <X11/Xatom.h>
#include <X11/XKBlib.h>
#include <X11/extensions/XKM.h>
#include <X11/extensions/XKBfile.h>
#include "xkbmap.h"
#include "stringtok.h"

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif

#ifndef DFLT_XKB_CONFIG_ROOT
#define	DFLT_XKB_CONFIG_ROOT "/usr/X11R6/lib/X11/xkb"
#endif
#ifndef DFLT_XKB_RULES_FILE
#define	DFLT_XKB_RULES_FILE "xfree86"
#endif
#ifndef DFLT_XKB_LAYOUT
#define	DFLT_XKB_LAYOUT "us"
#endif
#ifndef DFLT_XKB_MODEL
#define	DFLT_XKB_MODEL "pc101"
#endif

using namespace std;

Xkbmap::Xkbmap(void) : defaultlayout("us"), unknownsymbols("(unknown")
{
	memset( &rdefs, 0 , sizeof(XkbRF_VarDefsRec));
	for (int i=0; i < NUM_STRING_VALS; i++) {
		svSrc[i] = FROM_SERVER;
		svValue[i] = NULL;
	}		
	inclPath.push_back(string("."));
	inclPath.push_back(string(DFLT_XKB_CONFIG_ROOT));
}

Xkbmap::~Xkbmap(void)
{
	clearValues();
	if (rdefs.options != NULL) {
		free(rdefs.options);
	}
}

void Xkbmap::clearValues(void)
{
	for (int i = RULES_NDX; i < NUM_STRING_VALS; i++) {
		svSrc[i] = FROM_SERVER;

		if (svValue[i] != NULL) {
			free(svValue[i]);
			svValue[i]= NULL;
		}
	}
}
void Xkbmap::trySetString(valueIndices which, const char *newVal, svSources src)
{
    if (svValue[which] != NULL) {
		if (svSrc[which]>=src) {
			return;
		} else {
			free(svValue[which]);
			svValue[which]= NULL;
		}
	}

    svSrc[which]= src;
    svValue[which]= strdup(newVal);
    return;
}

Bool Xkbmap::getDisplay(void)
{
	int	major,minor,why;

    major= XkbMajorVersion;
    minor= XkbMinorVersion;
    dpy= XkbOpenDisplay(svValue[DISPLAY_NDX],NULL,NULL,&major,&minor,&why);
	
    if (!dpy) {
		if (svValue[DISPLAY_NDX] == NULL)	{
			svValue[DISPLAY_NDX]= strdup(getenv("DISPLAY"));
		}
		if (svValue[DISPLAY_NDX] == NULL) {
			svValue[DISPLAY_NDX]= strdup("default display");
		}
		switch (why) {
			case XkbOD_BadLibraryVersion:
				cerr << "scim_kmfl_server was compiled with XKB version " << 
					XkbMajorVersion << "." << setfill('0') << setw(2) << XkbMinorVersion << endl;
				cerr << "Xlib supports incompatible version " << 
					major << "." << setfill('0') << setw(2) << minor << endl;
				break;
			case XkbOD_ConnectionRefused:
				cerr << "Cannot open display " << svValue[DISPLAY_NDX] << endl;
				break;
			case XkbOD_NonXkbServer:
				cerr << "XKB extension not present on " << svValue[DISPLAY_NDX] << endl;
				break;
			case XkbOD_BadServerVersion:
				cerr << "scim_kmfl_server was compiled with XKB version " <<
					XkbMajorVersion << "." << setfill('0') << setw(2) << XkbMinorVersion << endl;
				cerr << "Server %s uses incompatible version " << svValue[DISPLAY_NDX]
					<< major << "." << setfill('0') << setw(2) << minor << endl;
				break;
			default:
				cerr << "Unknown error " << why << " from XkbOpenDisplay" << endl;
				break;
		}
		return False;
    }

    return True;
}

Bool  Xkbmap::getServerValues(void)
{
	XkbRF_VarDefsRec vd;
	char * tmp= NULL;

    if (!XkbRF_GetNamesProp(dpy,&tmp,&vd) || !tmp) {
		trySetString(RULES_NDX,DFLT_XKB_RULES_FILE,FROM_SERVER);
		trySetString(MODEL_NDX,DFLT_XKB_MODEL,FROM_SERVER);
		trySetString(LAYOUT_NDX,DFLT_XKB_LAYOUT,FROM_SERVER);
    } else {
		trySetString(RULES_NDX,tmp,FROM_SERVER);
		
		XFree(tmp);

		if (vd.model) {
			trySetString(MODEL_NDX,vd.model,FROM_SERVER);
			XFree(vd.model);
		}
		if (vd.layout) {
			trySetString(LAYOUT_NDX,vd.layout,FROM_SERVER);
			XFree(vd.layout);
		}
		if (vd.variant) {
			trySetString(VARIANT_NDX,vd.variant,FROM_SERVER);
			XFree(vd.variant);
		}
		if (vd.options) {
			addStringToOptions(vd.options);
			XFree(vd.options);
		}
	}
    return True;
}

void Xkbmap::addStringToOptions(char *opt_str)
{
	list<string> optionlist;
	
   	stringtok (optionlist, opt_str, ",");
	for (list<string>::const_iterator i = optionlist.begin();
        i != optionlist.end(); i++) {
		options.push_back(*i);
   } 
}

char * Xkbmap::stringFromOptions(char *orig)
{
	string newoptions;
	if (orig)
	{
		newoptions=orig;
	}
	for (vector<string>::iterator i = options.begin(); i < options.end(); i++) {
		if (newoptions.length() > 0) {
			newoptions+=",";
		}
		newoptions+= *i;
	}
	if (orig) {
		orig= (char*)realloc(orig, newoptions.length() + 1);
		
		if (orig)
			strcpy(orig, newoptions.c_str());
	} else {
		orig=strdup(newoptions.c_str());
	}
	return orig;
}

Bool Xkbmap::applyRules(void)
{
	char * rfName;
    XkbRF_RulesPtr	rules;
	XkbComponentNamesRec rnames;
	
	rules=NULL;
	
    if (svSrc[MODEL_NDX] != FROM_SERVER ||svSrc[LAYOUT_NDX] != FROM_SERVER ||svSrc[VARIANT_NDX] != FROM_SERVER || !options.empty()) {

        if(svSrc[VARIANT_NDX] < svSrc[LAYOUT_NDX]) {
            if (svValue[VARIANT_NDX] != NULL) {
				free(svValue[VARIANT_NDX]);
				svValue[VARIANT_NDX] = NULL;
			}				
		}

		rdefs.model= svValue[MODEL_NDX];
		rdefs.layout= svValue[LAYOUT_NDX];
		rdefs.variant= svValue[VARIANT_NDX];
		if (!options.empty()) {
			rdefs.options=stringFromOptions(rdefs.options);
		}
	
		if (svSrc[RULES_NDX]) {
			rfName= svValue[RULES_NDX];
		} else {
			rfName= DFLT_XKB_RULES_FILE;
		}
	
		if (rfName[0]=='/') {
			rules= XkbRF_Load(rfName,svValue[LOCALE_NDX],True,True);
		} else {
			
			for (vector<string>::const_iterator i=inclPath.begin(); i< inclPath.end() ;i++) {
				char buf[PATH_MAX];
				if (((*i).length()+strlen(rfName)+8)>PATH_MAX) {
					continue;
				}
				sprintf(buf,"%s/rules/%s",(*i).c_str(),svValue[RULES_NDX]);
				rules= XkbRF_Load(buf,svValue[LOCALE_NDX],True,True);				
				if (rules) {
					break;
				}
					
			}
		}
		if (!rules) {
			cerr << "Couldn't find rules file (" << svValue[RULES_NDX] << ")" << endl;
			return False;
		}
		
		XkbRF_GetComponents(rules,&rdefs,&rnames);
		
		if (rnames.keycodes) {
			trySetString(KEYCODES_NDX,rnames.keycodes,FROM_RULES);
			XFree(rnames.keycodes);
			rnames.keycodes= NULL;
		}
		if (rnames.symbols) {
			trySetString(SYMBOLS_NDX,rnames.symbols,FROM_RULES);
			XFree(rnames.symbols);
			rnames.symbols= NULL;
		}
		if (rnames.types) {
			trySetString(TYPES_NDX,rnames.types,FROM_RULES);
			XFree(rnames.types);
			rnames.types= NULL;
		}
		if (rnames.compat) {
			trySetString(COMPAT_NDX,rnames.compat,FROM_RULES);
			XFree(rnames.compat);
			rnames.compat= NULL;
		}
		if (rnames.geometry) {
			trySetString(GEOMETRY_NDX,rnames.geometry,FROM_RULES);
			XFree(rnames.geometry);
			rnames.geometry= NULL;
		}
		if (rnames.keymap) {
			trySetString(KEYMAP_NDX,rnames.keymap,FROM_RULES);
			XFree(rnames.keymap);
			rnames.keymap= NULL;
		}
		
		XkbRF_Free(rules,True);
    }
    return True;
}

/* Primitive sanity check - filter out 'map names' (inside parenthesis) */
/* that can confuse xkbcomp parser */
Bool Xkbmap::checkName(char *name, const char* str)
{
   char *i = name, *opar = NULL;
   Bool ret = True;

   if(!name)
      return True;

   while (*i){
    	if (opar == NULL) {
			if (*i == '(') {
			opar = i;
			}
	    } else {
			if ((*i == '(') || (*i == '|') || (*i == '+')) {
				ret = False;
				break;
			}
			if (*i == ')') {
				opar = NULL;
			}
      }
      i++;
	}
	if (opar) {
		ret = False;
	}
	if (!ret) {
		char c;
		int n = 1;
		for(i = opar+1; *i && n; i++) {
			if (*i == '(') {
				n++;
			}
			if (*i == ')') {
				n--;
			}
		}
		if (*i) {
			i++;
		}
    	c = *i;
    	*i = '\0';
      	cerr << "Illegal map name '" <<  opar << "' ";
      	*i = c;
      	cerr << "in " << str << "name '" << name << "'" << endl;;
   }
   return ret;
}

Bool Xkbmap::applyComponentNames(void)
{
    XkbDescPtr		xkb;
	
    if(!checkName(svValue[TYPES_NDX],    "types")) {
		return False;
	}
    if(!checkName(svValue[COMPAT_NDX],   "compat")) {
		return False;
	}
    if(!checkName(svValue[SYMBOLS_NDX],  "symbols")) {
		return False;
	}
    if(!checkName(svValue[KEYCODES_NDX], "keycodes")) {
		return False;
	}
    if(!checkName(svValue[GEOMETRY_NDX], "geometry")) {
		return False;
	}
    if(!checkName(svValue[KEYMAP_NDX],   "keymap")) {
		return False;
	}

    if (dpy) {
		XkbComponentNamesRec	cmdNames;
		cmdNames.types= svValue[TYPES_NDX];
		cmdNames.compat= svValue[COMPAT_NDX];
		cmdNames.symbols= svValue[SYMBOLS_NDX];
		cmdNames.keycodes= svValue[KEYCODES_NDX];
		cmdNames.geometry= svValue[GEOMETRY_NDX];
		cmdNames.keymap= svValue[KEYMAP_NDX];
		xkb= XkbGetKeyboardByName(dpy,XkbUseCoreKbd,&cmdNames,
				XkbGBN_AllComponentsMask&(~XkbGBN_GeometryMask), 
				XkbGBN_AllComponentsMask&(~XkbGBN_GeometryMask),
				True);
		if (!xkb) {
			cerr << "Error loading new keyboard description" << endl;
			return False;
		}
				
		if (svValue[RULES_NDX] != NULL && (rdefs.model != NULL || rdefs.layout!= NULL)) {
			if (!XkbRF_SetNamesProp(dpy,svValue[RULES_NDX],&rdefs)) {
				cerr <<"Error updating the XKB names property" << endl;
			}
		}

		XkbFreeNames(xkb, XkbAllNamesMask,True);
		XkbFreeKeyboard(xkb,XkbAllControlsMask,True);
    }
    return True;
}

string Xkbmap::getCurrentLayout(void)
{
	Atom 			rules_atom;
	Atom 			actual_type;
	int				fmt;
	int				revert_to_return;
	Window			focuswindow;
	unsigned long	nitems;
	unsigned long	bytes_after;
	char            *data;
	char			*out;
	Status			rtrn;
	string			currentlayout(defaultlayout);
	
	if (!getDisplay()) {
		return defaultlayout;
	}
	
    rules_atom= XInternAtom(dpy,_XKB_RF_NAMES_PROP_ATOM,True);
	
    if (rules_atom==None)	/* property cannot exist */
		return defaultlayout; 
	XGetInputFocus(dpy, &focuswindow, &revert_to_return);


    rtrn= XGetWindowProperty(dpy,focuswindow,rules_atom,
                                0L,_XKB_RF_NAMES_PROP_MAXLEN,False,
                                XA_STRING,&actual_type,
                                &fmt,&nitems,&bytes_after,
                                (unsigned char **)&data);
 		
	if (rtrn!=Success)
	{
		if (data) {
			XFree(data);
		}
		return defaultlayout;
	}
	
    if ((bytes_after>0) || (actual_type!=XA_STRING) || (fmt!=8)) {
		if (data) {
			XFree(data);
		}
		return defaultlayout;
    }
	
	// skip over file
    out= data;
    out+=strlen(out)+1;

	// skip over model
    if ((unsigned long)(out-data)<nitems) {
		out+=strlen(out)+1;
    }

	// get layout
    if ((unsigned long)(out-data)<nitems && *out) {
	    currentlayout = out;
	}
	
	XFree(data);
	
	return currentlayout;
}

string Xkbmap::getCurrentSymbols(void)
{
	XkbDescPtr xkb;
	char *sName= NULL;
    Atom sAtom;
	string sreturn(unknownsymbols);
	
	if (!getDisplay()) {
		return unknownsymbols;
	}	
	xkb= XkbGetMap(dpy,XkbAllMapComponentsMask,XkbUseCoreKbd);

	if (xkb==NULL)
	{
		return unknownsymbols;
	}
	
	if (XkbGetNames(dpy,XkbSymbolsNameMask,xkb)!=Success)
	{
		XkbFreeClientMap(xkb, 0, 1);
		return unknownsymbols;
	}
	
	if (xkb->names!=NULL)
		sAtom= xkb->names->symbols;
	else                        
		 sAtom= None;
	
	if (sAtom!=None)
		sName= XkbAtomGetString(dpy,sAtom);
	
	if (xkb->names!=NULL)
		XkbFreeNames(xkb,XkbSymbolsNameMask,True);

	if (sName==NULL)
	{
		sreturn= unknownsymbols;
	} else {
		sreturn=sName;
		free(sName);
	}
	
	XkbFreeKeyboard(xkb,XkbAllControlsMask,True);
	return sreturn;
}

void Xkbmap::setSymbols(const string & symbols)
{
	if (!getDisplay()) {
		return;
	}
	clearValues();
	options.clear();
    trySetString(LAYOUT_NDX, symbols.c_str(), FROM_CMD_LINE);
    svValue[LOCALE_NDX]= strdup(setlocale(LC_ALL,svValue[LOCALE_NDX]));
    svSrc[LOCALE_NDX]= FROM_SERVER;

    if (dpy) {
        getServerValues();
	}
	
    if (!applyRules()) {
		return;
	}
	
	trySetString(SYMBOLS_NDX,symbols.c_str(),FROM_CMD_LINE);

    if (!applyComponentNames()) {
		return;
	}
	
    if (dpy) {
		XCloseDisplay(dpy);
	}	
}

void Xkbmap::setLayout(const string & layout)
{
	if (!getDisplay()) {
		return;
	}
	clearValues();
	options.clear();
    trySetString(LAYOUT_NDX, layout.c_str(), FROM_CMD_LINE);
    svValue[LOCALE_NDX]= strdup(setlocale(LC_ALL,svValue[LOCALE_NDX]));
    svSrc[LOCALE_NDX]= FROM_SERVER;

    if (dpy) {
        getServerValues();
	}
	
    if (!applyRules()) {
		return;
	}
	
    if (!applyComponentNames()) {
		return;
	}
	
    if (dpy) {
		XCloseDisplay(dpy);
	}	
}
