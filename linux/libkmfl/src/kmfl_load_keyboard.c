/* kmfl_load_keyboard.c
 * Copyright (C) 2005 SIL International
 *
 * This file is part of the KMFL library.
 *
 */

/*

	Keystroke interpreter for keyboard mapping for Linux project

		Version 1.000, January 2004, John Durdin, Tavultesoft
	
	Keyboard and loading and instance management
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <setjmp.h>

#ifndef _WIN32
	#include <unistd.h>
#endif

#include <kmfl/kmfl.h>
#include <kmfl/kmflcomp.h>
#include <kmfl/kmflutfconv.h>
#include "libkmfl.h"

// Globally loaded keyboards and instances
XKEYBOARD *p_installed_kbd[MAX_KEYBOARDS]={NULL};
char * keyboard_filename[MAX_KEYBOARDS];

KMSI *p_first_instance={NULL};
unsigned int n_keyboards=0;

// Create a new keyboard mapping server instance
KMSI *kmfl_make_keyboard_instance(void *connection)
{
	KMSI *p_kmsi, *p;

	if((p_kmsi=(KMSI *)malloc(sizeof(KMSI))))
	{
		p_kmsi->history = (ITEM *)malloc((MAX_HISTORY+2)*sizeof(ITEM));
		if(p_kmsi->history) 
		{ 
			p_kmsi->connection = connection;
			*p_kmsi->kbd_name = 0;
			p_kmsi->keyboard = NULL;
			p_kmsi->groups = NULL;	
			p_kmsi->rules = NULL;
			p_kmsi->stores = NULL;
			p_kmsi->strings = NULL;
			p_kmsi->nhistory = 0;

			// Link to other keyboard instances
			if(p_first_instance == NULL)
			{
				p_first_instance = p_kmsi;
				p_kmsi->last = NULL;
			}
			else
			{
				for(p=p_first_instance; p->next!=NULL; p=p->next);
				p->next = p_kmsi;
				p_kmsi->last = p;
			}
			p_kmsi->next = NULL;
			DBGMSG(1,"Keyboard instance created\n");				
			return p_kmsi;
		}
		else
		{
			free(p_kmsi); 
		}
	}
	DBGMSG(1,"Unable to create keyboard instance!\n");
	return NULL; 
}
	
// Delete a keyboard mapping server instance
int kmfl_delete_keyboard_instance(KMSI *p_kmsi)
{
	KMSI *p1, *p2;

	// Remove this instance from chain of linked instances
	p1 = p_kmsi->last; p2 = p_kmsi->next;
	if(p1) p1->next = p2; else p_first_instance = p2;
	if(p2) p2->last = p1;

	// Free allocated memory
	if(p_kmsi->history) free(p_kmsi->history);
	free(p_kmsi);
	
	DBGMSG(1,"Keyboard instance deleted\n");
	return 0;
}

// Delete all server instances
int kmfl_delete_all_keyboard_instances(void)
{
	KMSI *p, *p1;
	for(p=p_first_instance; p!=NULL; p=p1)
	{
		p1 = p->next;
		kmfl_delete_keyboard_instance(p);
	}

	return 0;
}

// Attach a keyboard to a server instance
int kmfl_attach_keyboard(KMSI *p_kmsi, int keyboard_number)
{
	XKEYBOARD *p_kbd=NULL;
	XGROUP *gp;
	unsigned int n, nrules;
	
	if (p_installed_kbd[keyboard_number] == NULL) {
		DBGMSG(1,"Invalid keyboard number\n");
		return -1;
	}

	p_kbd=p_installed_kbd[keyboard_number];
	p_kmsi->keyboard = p_kbd;
	p_kmsi->keyboard_number = keyboard_number;

	// Fill group, rule, store and string pointers
	p_kmsi->stores = (XSTORE *)(p_kbd+1);
	p_kmsi->groups = (XGROUP *)(p_kmsi->stores+p_kbd->nstores);
	p_kmsi->rules = (XRULE *)(p_kmsi->groups+p_kbd->ngroups);

	for(n=nrules=0,gp=p_kmsi->groups; n<p_kbd->ngroups; n++, gp++)
	{
		nrules += gp->nrules;
	}

	p_kmsi->strings = (ITEM *)(p_kmsi->rules+nrules);

	// Initialize history unless keyboard hasn't changed
	if(strcmp(p_kbd->name,p_kmsi->kbd_name) != 0)
	{
		strncpy(p_kmsi->kbd_name,p_kbd->name, NAMELEN);
		p_kmsi->kbd_name[NAMELEN]=0;
		*p_kmsi->history = 0;
		p_kmsi->nhistory = 0;
	}
	
	DBGMSG(1,"Keyboard %s attached\n",p_kbd->name);
	return 0;
}

// Detach a keyboard from a server instance
int kmfl_detach_keyboard(KMSI *p_kmsi)
{
	if(p_kmsi) 
		DBGMSG(1,"Keyboard %s detached\n",p_kmsi->kbd_name);
	
	*p_kmsi->kbd_name = 0;
	p_kmsi->keyboard = NULL;
	p_kmsi->groups = NULL;
	p_kmsi->rules = NULL;
	p_kmsi->stores = NULL;
	p_kmsi->strings = NULL;
	return 0;
}

XKEYBOARD * kmfl_load_keyboard_from_file(const char *filename)
{
	XKEYBOARD *p_kbd;
	FILE *fp;
	char version_string[6]={0};
	unsigned int filelen, kbver=0;
	struct stat fstat;
	const char * extension;
    int errcode;

	DBGMSG(1,"DAR: kmfl_load_keyboard_from_file %s\n",filename);

    extension = strrchr(filename, '.');
    
    if (extension && (strcmp(extension, ".kmn") == 0))
    {
        
     	errcode = setjmp(fatal_error_buf);
    	
        if (errcode == 0)
        {       
            compile_keyboard_to_buffer(filename, (void *) &p_kbd);
    		memcpy(version_string,p_kbd->version,3); // Copy to ensure terminated
    		kbver = (unsigned)atoi(version_string);
        } 
        else
        {
            return NULL;
        } 
    } 
    else
    {    
    	// Get the file size
    	if(stat(filename,&fstat) != 0) 
    	   return NULL;
    	filelen = fstat.st_size;

    	// Allocate memory for the installed keyboard
    	if((p_kbd=(XKEYBOARD *)malloc(filelen)) == NULL) 
			return NULL;

    	// Open the file
    	if((fp=fopen(filename,"rb")) != NULL) 
    	{
    		if (fread(p_kbd, 1, filelen, fp) != filelen)
    		{
	    		fclose(fp);
	    		return NULL;
	    	}
    		fclose(fp);
    		memcpy(version_string,p_kbd->version,3); // Copy to ensure terminated
    		kbver = (unsigned)atoi(version_string);
    	}
    }
	// Check the loaded file is valid and has the correct version
	if((memcmp(p_kbd->id,"KMFL",4) != 0) 
		|| (p_kbd->version[3] != *FILE_VERSION)
		|| (kbver < (unsigned)atoi(BASE_VERSION))
		|| (kbver > (unsigned)atoi(LAST_VERSION)))
	{
		DBGMSG(1, "Invalid version\n");
		free(p_kbd); 
		return NULL;
	}

	DBGMSG(1,"DAR: kmfl_load_keyboard_from_file - %s loaded\n",filename);
	
	return p_kbd;
}

// Load the keyboard table into memory and assign it to an empty keyboard slot
int kmfl_load_keyboard(const char *file) 
{
	XKEYBOARD *p_kbd;
	int keyboard_number;
	
	// Check number of installed keyboards
	if(n_keyboards >= MAX_KEYBOARDS) return -1;
	
	// initialize the installed keyboards array
	if(n_keyboards == 0)
		memset(p_installed_kbd, 0, sizeof(XKEYBOARD *) * MAX_KEYBOARDS);
	
	p_kbd = kmfl_load_keyboard_from_file(file);

	if (p_kbd == NULL)
		return -1;

	// Find an empty slot
	for (keyboard_number=0;keyboard_number < MAX_KEYBOARDS; keyboard_number++)
		if (p_installed_kbd[keyboard_number] == NULL)
			break;
		
	// Sanity check
	if (keyboard_number == MAX_KEYBOARDS) {
		DBGMSG(1, "Could not find an empty keyboard slot even though there was supposed to be one\n");
		free(p_kbd);
		return -1;
	}
	
	// Copy pointer and increment number of installed keyboards
	p_installed_kbd[keyboard_number] = p_kbd;
	keyboard_filename[keyboard_number]=strdup(file);
	
	n_keyboards++;
	DBGMSG(1,"Keyboard %s loaded\n",p_kbd->name);

	return keyboard_number;	
}

// Check that a keyboard file is valid
int kmfl_check_keyboard(const char *file) 
{
	XKEYBOARD xkb;
	FILE *fp;
	char version_string[6]={0};
	unsigned int kbver=0;

	// Open the file
	if((fp=fopen(file,"rb")) == NULL) 
		return(-1);
	
	if (fread(&xkb, 1, sizeof(XKEYBOARD), fp) != sizeof(XKEYBOARD))
	{
		fclose(fp);
		return(-1);
	}
	
	fclose(fp);
	
	memcpy(version_string,xkb.version,3);	// Copy to ensure terminated
	kbver = (unsigned)atoi(version_string);

	// Check the loaded file is valid and has the correct version
	if(memcmp(xkb.id,"KMFL",4) != 0) 
		return(-2);
	if(xkb.version[3] != *FILE_VERSION) 
		return(-2);
	if(kbver < (unsigned)atoi(BASE_VERSION)) 
		return(-3);
	if(kbver > (unsigned)atoi(LAST_VERSION)) 
		return(-4);
	
	return 0;	// file appears to be valid
}

// Reload a keyboard from a file
int kmfl_reload_keyboard(int keyboard_number)
{
	KMSI *p;
	XKEYBOARD *p_kbd;
	XKEYBOARD *p_newkbd;
	
	p_kbd =p_installed_kbd[keyboard_number];

	if (p_kbd == NULL) 
		return -1;
	
	// Detach any instances of this keyboard
	for(p=p_first_instance; p; p=p->next)
	{
		if(p->keyboard_number == keyboard_number) 
			kmfl_detach_keyboard(p);
	}

	p_newkbd=kmfl_load_keyboard_from_file(keyboard_filename[keyboard_number]);

	if (p_newkbd == NULL)
		return -1;
	
	p_installed_kbd[keyboard_number]=p_newkbd;

	free(p_kbd);

	// reattach this keyboard to instances using this keyboard
	for(p=p_first_instance; p; p=p->next)
	{
		if(p->keyboard_number == keyboard_number) 
			kmfl_attach_keyboard(p, keyboard_number);
	}
	
	return 0;	
}

// Reload all keyboards
int kmfl_reload_all_keyboards(void)
{
	int n;

	for(n=0; n < MAX_KEYBOARDS; n++) 
	{
		if(p_installed_kbd[n] != NULL)
			kmfl_reload_keyboard(n);
	}
	return 0;
}

// Unload a keyboard that has been installed
int kmfl_unload_keyboard(int keyboard_number) 
{
	KMSI *p;
	XKEYBOARD *p_kbd=p_installed_kbd[keyboard_number];
	
	if (p_kbd == NULL) 
		return -1;
	
	// Enumerate instances and ensure that no instances are using this keyboard
	for(p=p_first_instance; p; p=p->next)
	{
		if(p->keyboard_number == keyboard_number) return 1;
	}

		
	// Remove keyboard from list and free memory
	DBGMSG(1,"Keyboard %s unloaded\n",p_kbd->name);
	free(keyboard_filename[keyboard_number]);
	free(p_kbd);
	
	p_installed_kbd[keyboard_number]=NULL;
	
	n_keyboards--;
	
	return 0;
}

// Unload all keyboards from memory
int kmfl_unload_all_keyboards(void)
{
	int n;

	for(n=0; n < MAX_KEYBOARDS; n++) 
	{
		if(p_installed_kbd[n] != NULL)
			kmfl_unload_keyboard(n);
	}
	n_keyboards = 0;
	return 0;
}

// Get the number of an installed keyboard from the name
int kmfl_keyboard_number(char *name)
{
	unsigned int n;

	for(n=0; n<MAX_KEYBOARDS; n++)
	{
		if(p_installed_kbd[n] != NULL && 
		   strcmp(p_installed_kbd[n]->name,name) == 0) 
			return n;
	}
	return UNDEFINED;
}

// Get the name of an installed keyboard from the number
const char *kmfl_keyboard_name(int keyboard_number)
{
	if(p_installed_kbd[keyboard_number] != NULL)
		return p_installed_kbd[keyboard_number]->name;
	else
		return NULL;
}

const char *kmfl_icon_file(int keyboard_number)
{
	XKEYBOARD *p_kbd;
	XSTORE *stores;
	XGROUP *groups, *gp;
	XRULE *rules;
	ITEM * strings;
	UTF32 *p32;
	UTF8 *p8;
	unsigned int n, nrules;
	static char icon_name[256];

	*icon_name = 0;

	if((p_kbd=p_installed_kbd[keyboard_number]) != NULL) 
	{
		stores = (XSTORE *)(p_kbd+1);
		groups = (XGROUP *)(stores+p_kbd->nstores);
		rules = (XRULE *)(groups+p_kbd->ngroups);

		for(n=nrules=0,gp=groups; n<p_kbd->ngroups; n++, gp++)
		{
			nrules += gp->nrules;
		}

		strings = (ITEM *)(rules+nrules);

		if(stores[SS_BITMAP].len >= 0) 
		{
			p32 = strings + stores[SS_BITMAP].items;
			p8 = icon_name;
			IConvertUTF32toUTF8((const UTF32**)&p32,p32+stores[SS_BITMAP].len,&p8,p8+255);
			*p8 = 0;
		}
	}
	return icon_name;
}
