/************************************************************
 Copyright (c) 1996 by Silicon Graphics Computer Systems, Inc.

 Permission to use, copy, modify, and distribute this
 software and its documentation for any purpose and without
 fee is hereby granted, provided that the above copyright
 notice appear in all copies and that both that copyright
 notice and this permission notice appear in supporting
 documentation, and that the name of Silicon Graphics not be 
 used in advertising or publicity pertaining to distribution 
 of the software without specific prior written permission.
 Silicon Graphics makes no representation about the suitability 
 of this software for any purpose. It is provided "as is"
 without any express or implied warranty.
 
 SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS 
 SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY 
 AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
 GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
 DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, 
 DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
 OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
 THE USE OR PERFORMANCE OF THIS SOFTWARE.

 ********************************************************/
/* $XFree86: xc/programs/setxkbmap/setxkbmap.c,v 3.7 2003/01/20 04:15:08 dawes Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <limits.h>
#include <ctype.h>
#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/XKBlib.h>
#include <X11/extensions/XKBfile.h>
#include <X11/extensions/XKBconfig.h>
#include <X11/extensions/XKBrules.h>

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

#define	FROM_IMENGINE	0
#define	FROM_RULES	1
#define	FROM_CMD_LINE	2


#define	RULES_NDX	0
#define	DISPLAY_NDX	1
#define	LOCALE_NDX	2
#define	MODEL_NDX	3
#define	LAYOUT_NDX	4
#define	VARIANT_NDX	5
#define KEYCODES_NDX	6
#define	TYPES_NDX	7
#define	COMPAT_NDX	8
#define	SYMBOLS_NDX	9
#define	GEOMETRY_NDX	10
#define	KEYMAP_NDX	11
#define	NUM_STRING_VALS	12

Display *		dpy;

int	svSrc[NUM_STRING_VALS];
char *	svValue[NUM_STRING_VALS];

XkbConfigFieldsRec	cfgDflts;
XkbConfigRtrnRec	cfgResult;

XkbRF_RulesPtr		rules= NULL;
XkbRF_VarDefsRec	rdefs;

Bool			clearOptions= False;
int			szOptions= 0;
int			numOptions= 0;
char **			options= NULL;

int			szInclPath= 0;
int			numInclPath= 0;
char **			inclPath= NULL;

XkbDescPtr		xkb= NULL;

/***====================================================================***/

#define	streq(s1,s2)	(strcmp(s1,s2)==0)
#define	strpfx(s1,s2)	(strncmp(s1,s2,strlen(s2))==0)

#define	MSG(s)		printf(s)
#define	MSG1(s,a)	printf(s,a)
#define	MSG2(s,a,b)	printf(s,a,b)
#define	MSG3(s,a,b,c)	printf(s,a,b,c)

#define	ERR(s)		fprintf(stderr,s)
#define	ERR1(s,a)	fprintf(stderr,s,a)
#define	ERR2(s,a,b)	fprintf(stderr,s,a,b)
#define	ERR3(s,a,b,c)	fprintf(stderr,s,a,b,c)

/***====================================================================***/

Bool addToList ( int *sz, int *num, char ***listIn, char *newVal );
void trySetString ( int which, char * newVal, int src );
Bool getDisplay ( void );
Bool getIMEngineValues ( void );
Bool addStringToOptions ( char * opt_str, int * sz_opts, int * num_opts, char *** opts );
char * stringFromOptions ( char * orig, int numNew, char ** newOpts );
Bool applyConfig ( char * name );
Bool applyRules ( void );
Bool applyComponentNames ( void );

/***====================================================================***/

Bool
addToList(int *sz,int *num,char ***listIn,char *newVal)
{
register int i;
char **list;

    if ((!newVal)||(!newVal[0])) {
	*num= 0;
	return True;
    }
    list= *listIn;
    for (i=0;i<*num;i++) {
	if (streq(list[i],newVal))
	    return True;
    }
    if ((list==NULL)||(*sz<1)) {
	*num= 0;
	*sz= 4;
	list= (char **)calloc(*sz,sizeof(char *));
	*listIn= list;
    }
    else if (*num>=*sz) {
	*sz*= 2;
	list= (char **)realloc(list,(*sz)*sizeof(char *));
	*listIn= list;
    }
    if (!list) {
	ERR("Internal Error! Allocation failure in add to list!\n");
	ERR("                Exiting.\n");
	exit(-1);
    }
    list[*num]= strdup(newVal);
    (*num)= (*num)+1;
    return True;
}

/***====================================================================***/

void
trySetString(int which,char *newVal,int src)
{
    if (svValue[which]!=NULL) {
	if (svSrc[which]==src) {
	    return;
	}
	else if (svSrc[which]>src) {
	    return;
	}
    }
    svSrc[which]= src;
    svValue[which]= newVal;
    return;
}

/***====================================================================***/
void initializePath()
{
    addToList(&szInclPath,&numInclPath,&inclPath,".");
    addToList(&szInclPath,&numInclPath,&inclPath,DFLT_XKB_CONFIG_ROOT);
}

Bool
getDisplay()
{
int	major,minor,why;

    major= XkbMajorVersion;
    minor= XkbMinorVersion;
    dpy= XkbOpenDisplay(svValue[DISPLAY_NDX],NULL,NULL,&major,&minor,&why);
    if (!dpy) {
	if (svValue[DISPLAY_NDX]==NULL)	
	    svValue[DISPLAY_NDX]= getenv("DISPLAY");
	if (svValue[DISPLAY_NDX]==NULL)
	    svValue[DISPLAY_NDX]= "default display";
	switch (why) {
	    case XkbOD_BadLibraryVersion:
		ERR2("scim_kmfl_imengine was compiled with XKB version %d.%02d\n",
					XkbMajorVersion,XkbMinorVersion);
		ERR2("Xlib supports incompatible version %d.%02d\n",
					major,minor);
		break;
	    case XkbOD_ConnectionRefused:
                ERR1("Cannot open display \"%s\"\n",svValue[DISPLAY_NDX]);
		break;
	    case XkbOD_NonXkbIMEngine:
		ERR1("XKB extension not present on %s\n",svValue[DISPLAY_NDX]);
		break;
	    case XkbOD_BadIMEngineVersion:
                ERR2("scim_kmfl_imengine was compiled with XKB version %d.%02d\n",
				XkbMajorVersion,XkbMinorVersion);
		ERR3("IMEngine %s uses incompatible version %d.%02d\n",
				svValue[DISPLAY_NDX],major,minor);
		break;
	    default:
		ERR1("Unknown error %d from XkbOpenDisplay\n",why);
		break;
	}
	return False;
    }

    return True;
}

/***====================================================================***/

Bool
getIMEngineValues(void)
{
XkbRF_VarDefsRec 	vd;
char *			tmp= NULL;

    if (!XkbRF_GetNamesProp(dpy,&tmp,&vd) || !tmp) {
        tmp = DFLT_XKB_RULES_FILE;
        vd.model = DFLT_XKB_MODEL;
        vd.layout = DFLT_XKB_LAYOUT;
    }
    if (tmp)
	trySetString(RULES_NDX,tmp,FROM_IMENGINE);
    if (vd.model)
	trySetString(MODEL_NDX,vd.model,FROM_IMENGINE);
    if (vd.layout)
	trySetString(LAYOUT_NDX,vd.layout,FROM_IMENGINE);
    if (vd.variant)
	trySetString(VARIANT_NDX,vd.variant,FROM_IMENGINE);
    if ((vd.options)&&(!clearOptions)) {
	addStringToOptions(vd.options,&szOptions,&numOptions,&options);
	XFree(vd.options);
    }
    return True;
}

/***====================================================================***/

Bool
addStringToOptions(char *opt_str,int *sz_opts,int *num_opts,char ***opts)
{
char 	*tmp,*str,*next;
Bool	ok= True;

    if ((str= malloc(strlen(opt_str)+1))!=NULL)
	 strcpy(str,opt_str);
    else return False;
    for (tmp= str,next=NULL;(tmp && *tmp!='\0')&&ok;tmp=next) {
	next= strchr(str,',');
	if (next) {
	    *next= '\0';
	    next++;
	}
	ok= addToList(sz_opts,num_opts,opts,tmp)&&ok;
    }
    free(str);
    return ok;
}

/***====================================================================***/

char *
stringFromOptions(char *orig,int numNew,char **newOpts)
{
int	len,i,nOut;

    if (orig)	len= strlen(orig)+1;
    else	len= 0;
    for (i=0;i<numNew;i++) {
	if (newOpts[i])
	    len+= strlen(newOpts[i])+1;
    }
    if (len<1)
	return NULL;
    if (orig) {
	orig= (char *)realloc(orig,len);
	nOut= 1;
    }
    else {
	orig= (char *)calloc(len,1);
	nOut= 0;
    }
    for (i=0;i<numNew;i++) {
	if (!newOpts[i])
	    continue;
	if (nOut>0) {
	     strcat(orig,",");
	     strcat(orig,newOpts[i]);
	}
	else strcpy(orig,newOpts[i]);
	nOut++;
    }
    return orig;
}

/***====================================================================***/



Bool
applyRules(void)
{
int	i;
char *	rfName;

    if (svSrc[MODEL_NDX]||svSrc[LAYOUT_NDX]||svSrc[VARIANT_NDX]||options) {
	char 			buf[PATH_MAX];
	XkbComponentNamesRec	rnames;

        if(svSrc[VARIANT_NDX] < svSrc[LAYOUT_NDX])
            svValue[VARIANT_NDX] = NULL;

	rdefs.model= svValue[MODEL_NDX];
	rdefs.layout= svValue[LAYOUT_NDX];
	rdefs.variant= svValue[VARIANT_NDX];
	if (options)
	    rdefs.options=stringFromOptions(rdefs.options,numOptions,options);

	if (svSrc[RULES_NDX])
	     rfName= svValue[RULES_NDX];
	else rfName= DFLT_XKB_RULES_FILE;

	if (rfName[0]=='/') {
	    rules= XkbRF_Load(rfName,svValue[LOCALE_NDX],True,True);
	}
	else {
	    for (i=0;(i<numInclPath)&&(!rules);i++) {
		if ((strlen(inclPath[i])+strlen(rfName)+8)>PATH_MAX) {
		    continue;
		}
		sprintf(buf,"%s/rules/%s",inclPath[i],svValue[RULES_NDX]);
		rules= XkbRF_Load(buf,svValue[LOCALE_NDX],True,True);
	    }
	}
	if (!rules) {
	    ERR1("Couldn't find rules file (%s) \n",svValue[RULES_NDX]);
	    return False;
	}
	XkbRF_GetComponents(rules,&rdefs,&rnames);
	if (rnames.keycodes) {
	    trySetString(KEYCODES_NDX,rnames.keycodes,FROM_RULES);
	    rnames.keycodes= NULL;
	}
	if (rnames.symbols) {
	    trySetString(SYMBOLS_NDX,rnames.symbols,FROM_RULES);
	    rnames.symbols= NULL;
	}
	if (rnames.types) {
	    trySetString(TYPES_NDX,rnames.types,FROM_RULES);
	    rnames.types= NULL;
	}
	if (rnames.compat) {
	    trySetString(COMPAT_NDX,rnames.compat,FROM_RULES);
	    rnames.compat= NULL;
	}
	if (rnames.geometry) {
	    trySetString(GEOMETRY_NDX,rnames.geometry,FROM_RULES);
	    rnames.geometry= NULL;
	}
	if (rnames.keymap) {
	    trySetString(KEYMAP_NDX,rnames.keymap,FROM_RULES);
	    rnames.keymap= NULL;
	}
    }
    return True;
}

/* Primitive sanity check - filter out 'map names' (inside parenthesis) */
/* that can confuse xkbcomp parser */
Bool
checkName(char *name, char* string)
{
   char *i = name, *opar = NULL;
   Bool ret = True;

   if(!name)
      return True;

   while (*i){
      if (opar == NULL) {
         if (*i == '(')
         opar = i;
      } else {
         if ((*i == '(') || (*i == '|') || (*i == '+')) {
             ret = False;
             break;
         }
         if (*i == ')')
             opar = NULL;
      }
      i++;
   }
   if (opar)
      ret = False;
   if (!ret) {
      char c;
      int n = 1;
      for(i = opar+1; *i && n; i++) {
         if (*i == '(') n++;
         if (*i == ')') n--;
      }
      if (*i) i++;
      c = *i;
      *i = '\0';
      ERR1("Illegal map name '%s' ", opar);
      *i = c;
      ERR2("in %s name '%s'\n", string, name);
   }
   return ret;
}

Bool
applyComponentNames(void)
{
    if(!checkName(svValue[TYPES_NDX],    "types"))
	return False;
    if(!checkName(svValue[COMPAT_NDX],   "compat"))
	return False;
    if(!checkName(svValue[SYMBOLS_NDX],  "symbols"))
	return False;
    if(!checkName(svValue[KEYCODES_NDX], "keycodes"))
	return False;
    if(!checkName(svValue[GEOMETRY_NDX], "geometry"))
	return False;
    if(!checkName(svValue[KEYMAP_NDX],   "keymap"))
	return False;

    if (dpy) {
	XkbComponentNamesRec	cmdNames;
	cmdNames.types= svValue[TYPES_NDX];
	cmdNames.compat= svValue[COMPAT_NDX];
	cmdNames.symbols= svValue[SYMBOLS_NDX];
	cmdNames.keycodes= svValue[KEYCODES_NDX];
	cmdNames.geometry= svValue[GEOMETRY_NDX];
	cmdNames.keymap= svValue[KEYMAP_NDX];
	xkb= XkbGetKeyboardByName(dpy,XkbUseCoreKbd,&cmdNames,
			XkbGBN_AllComponentsMask, 
			XkbGBN_AllComponentsMask&(~XkbGBN_GeometryMask),
			True);
	if (!xkb) {
	    ERR("Error loading new keyboard description\n");
	    return False;
	}
	if (svValue[RULES_NDX] && (rdefs.model || rdefs.layout)) {
	    if (!XkbRF_SetNamesProp(dpy,svValue[RULES_NDX],&rdefs)) {
		MSG("Error updating the XKB names property\n");
	    }
	}
    }
    return True;
}


int
main(int argc,char **argv)
{
    initializePath();
    if (!getDisplay())
	exit(-1);
    trySetString(LAYOUT_NDX,argv[1],FROM_CMD_LINE);
    svValue[LOCALE_NDX]= setlocale(LC_ALL,svValue[LOCALE_NDX]);
    svSrc[LOCALE_NDX]= FROM_IMENGINE;

    if (dpy)
        getIMEngineValues();
    if (!applyRules())
	exit(-4);
    if (!applyComponentNames())
	exit(-5);
    if (dpy)
	XCloseDisplay(dpy);
    exit(0);
}
