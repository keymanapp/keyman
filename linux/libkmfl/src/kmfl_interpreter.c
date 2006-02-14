/* kmfl_interpreter.c
 * Copyright (C) 2005 SIL International and Tavultesoft Pty Ltd
 *
 * This file is part of the KMFL library.
 *
 * The KMFL library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * The KMFL library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with the KMFL library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

/*

	Keystroke interpreter for keyboard mapping for Linux project

		Version 1.000, January 2004, John Durdin, Tavultesoft
	
	Main interpreter module

	Notes:
		History is a reverse ordered array of (Unicode) characters passed to the
		output and deadkeys that are pending, most recent first. For groups that
		process using the current keystroke, the keysym and state for that keystroke
		are prepended to the history before matching rules.

		The main entry point is the routine kmfl_interpret() which will return a value of
		zero if the current keystroke is not matched or output, one if it is either 
		matched or passed directly to the output routine, and a negative value if any
		error is detected.

		Each store and group offset from the start of the table is saved, using two 
		arrays in the table header.  The first MAX_HEADER_STORES are reserved for 
		headers, but the references may be null.  The number of stores must always be 
		at least MAX_HEADER_STORES.  
		
		Based on the "K_TTY" console-based keyboard mapping project 
		developed by David Gardner, but completely rewritten for use 
		with X-applications.
*/

#include <stdio.h>
#include <string.h>

#include <kmfl/kmfl.h>
#include "libkmfl.h"
#include "ConvertUTF.h"

// Macros to find index offsets and referenced stores or groups
#define INDEX_OFFSET(x)		(((x)>>16)&0xff)
#define STORE_NUMBER(x)		((x)&0x0000ffff)
#define GROUP_NUMBER(x)		((x)&0x0000ffff)
#define CONTEXT_CHAR(x)		((x)&0x0000ffff)

// Macro to return a string from the string table
#define ITEMSTRING(x)		(strings+(x))

int process_group(KMSI *p_kmsi, XGROUP *gp);
int match_rule(KMSI *p_kmsi, XRULE *rp, ITEM *any_index, int usekeys);
int process_rule(KMSI *p_kmsi, XRULE *rp, ITEM *any_index, int usekeys);
UINT modified_state(UINT state);
UINT compare_state(ITEM rule_key, ITEM keystroke);

void output_item(void *connection, ITEM x);
void add_to_history(KMSI *p_kmsi,ITEM key);
void delete_from_history(KMSI *p_kmsi,UINT nchars);
void clear_history(KMSI *p_kmsi);

ITEM *store_content(KMSI *p_kmsi, UINT nstore);
UINT store_length(KMSI *p_kmsi, UINT nstore);

// External routines
void output_string(void *connection, char *p);
void output_char(void *connection, BYTE q);
void output_beep(void *connection);
void forward_keyevent(void *connection, UINT key, UINT state);

void erase_char(void *connection);

int kmfl_interpret(KMSI *p_kmsi, UINT key, UINT state) 
{
	XKEYBOARD *p_kbd;
	XGROUP *p_group1;
	ITEM keysym;
	int matched;

	// Test first for modifier key keystrokes and do nothing
	switch(key) 
	{
	case 0xff67:		// menu
	case 0xff7f:		// Num Lock
	case 0xffe1:		// L shift
	case 0xffe2:		// R shift
	case 0xffe3:		// L ctrl
	case 0xffe4:		// R ctrl
	case 0xffe5:		// Caps Lock
	case 0xffe9:		// L alt
	case 0xffea:		// R alt
	case 0xffeb:		// L win
	case 0xffec:		// R win
		return 0;		
	}

	if(p_kmsi == NULL || p_kmsi->keyboard == NULL) return 0;

	// Pack the state bits into a single byte
	state = modified_state(state);

	// Get the memory address of the keyboard header
	p_kbd = p_kmsi->keyboard;
	p_group1 = p_kmsi->groups+p_kbd->group1;

	// Place the current keystroke at the start of the history array
	keysym = (key & 0xffff) | (state<<16);	
	keysym = MAKE_ITEM(ITEM_KEYSYM,keysym);
	p_kmsi->history[0] = keysym;

	// Pass control to the first group for processing, and return if key was matched
	if((matched=process_group(p_kmsi, p_group1)) > 0) 
		return 1;

	// Now try without shift state
	if ((state & KS_SHIFT) != 0) 
	{
		keysym &= ~((unsigned long)KS_SHIFT<<16);
		p_kmsi->history[0] = keysym;
		if((matched=process_group(p_kmsi, p_group1)) > 0) 
			return 1;
	}

	/* need some kind of error notification if error value returned */

	// If the keystroke is a valid Unicode character, and not a control or alt combination, 
	// output it and add it to the history 
	if(((key & 0xff00) == 0) && ((state & 0xcc) == 0))
	{
		add_to_history(p_kmsi,(ITEM)key);
		output_item(p_kmsi->connection,(ITEM)key);
		return 1;
	}

	// Handle special case keystrokes
	switch(key) 
	{

	case 0xff08:		// backspace - erase last character from history
		delete_from_history(p_kmsi,1);
		erase_char(p_kmsi->connection);
		return 1;
	case 0xff09:		// tab - clear history, let app handle key
	case 0xff0d:		// return - clear history, let app handle key
		clear_history(p_kmsi);
		return 0;
	case 0xff1b:		// escape - add to history, let app handle key
		add_to_history(p_kmsi,(ITEM)0x1b);
		forward_keyevent(p_kmsi->connection, key, state);
		return 1;
	default:
		clear_history(p_kmsi);
		return 0;		// let application handle key, but erase history 
	}
}

// Process a keystroke with a given group of rules
int process_group(KMSI *p_kmsi, XGROUP *gp) 
{
	UINT nrules, n, nhistory;
	XRULE *rp, trule;
	ITEM any_index[MAX_HISTORY+2];
	int matched, result=0, usekeys, enable_global_matching;

	if(p_kmsi->nhistory > MAX_HISTORY) p_kmsi->nhistory = MAX_HISTORY;
	usekeys = ((gp->flags & GF_USEKEYS) != 0);
	nhistory = p_kmsi->nhistory;
	if(usekeys) nhistory++;
	p_kmsi->history[nhistory+1-usekeys] = 0;

	nrules = gp->nrules;

	// Match rules until either a match is found or all rules have been tried
	for(n=0,rp=p_kmsi->rules+gp->rule1; n<nrules; n++,rp++) 
	{
		// Check rule length before matching
		if((rp->ilen > nhistory+1) || ((rp->ilen == nhistory+1)
			&& (ITEM_TYPE(*(p_kmsi->strings+rp->lhs)) != ITEM_NUL))) continue;

		// Compare the current rule with the history
		if((matched=match_rule(p_kmsi,rp,any_index,usekeys)))
		{			
			// Then determine the output for this rule
			result = process_rule(p_kmsi,rp,any_index,usekeys);
			break;
		}
	}

	// Determine if we need to consider processing match or nomatch rules
	if((gp->flags & GF_USEKEYS) != 0)
	{
		enable_global_matching = ((*p_kmsi->history & 0xff00) != 0xff00);
	}
	else
	{
		enable_global_matching = 1;
	}

	// Conditionally process nomatch and match rules here, if result = 0 or 1 respectively
	if((result == 0) && (gp->nmrlen > 0) && enable_global_matching)
	{
		trule.ilen = 0;
		trule.olen = gp->nmrlen;
		trule.rhs = gp->nomatch;
		result = process_rule(p_kmsi,&trule,any_index,usekeys);
	}
	else if((result == 1) && (gp->mrlen > 0) && enable_global_matching)
	{
		trule.ilen = 0;
		trule.olen = gp->mrlen;
		trule.rhs = gp->match;
		result = process_rule(p_kmsi,&trule,any_index,usekeys);
		if(result == 0) result = 1;		// matched already, do not return 0 
	}

	return result;
}

// Match the history with each rule in turn and return if a match is found
int match_rule(KMSI *p_kmsi, XRULE *rp, ITEM *any_index, int usekeys) 
{
	
	UINT k, m, n, nmax, rulelen, nhistory;
	ITEM *pr, *ph, *ps, mask;

	rulelen = rp->ilen;
	pr = p_kmsi->strings+rp->lhs;
	ph = p_kmsi->history+rulelen - (usekeys ? 1 : 0);
	nhistory = p_kmsi->nhistory + (usekeys ? 1 : 0);	
	
	for(m=0; m<rp->ilen; pr++,ph--,m++) 
	{
		switch(ITEM_TYPE(*pr)) 
		{
		case ITEM_CHAR:
			if(*pr != *ph) return 0;
			break;				// matched - continue matching string

		case ITEM_KEYSYM:
			if((*pr & 0xffff) != (*ph & 0xffff)) return 0;
			if(compare_state(*pr,*ph)) return 0;
			break;				// matched - continue matching string

		case ITEM_DEADKEY:	
			if(*pr != *ph) return 0;
			break;				// matched - continue matching string

		case ITEM_ANY:	// will need to allow for matching keysyms in any()
			ps = store_content(p_kmsi,STORE_NUMBER(*pr));
			nmax = store_length(p_kmsi,STORE_NUMBER(*pr));
			if(m == rp->ilen-1) mask = 0xffffff; else mask = 0xffffffff;
			for(n=0; n<nmax; ps++,n++) 
			{
				if(((*ps) & mask) == ((*ph) & mask)) // ignore keysym id
				{
					any_index[m] = n;	// save offset for use with index
					break;
				}
			}
			if(n == nmax) return 0;		// no match
			break;				// matched - continue matching string

		case ITEM_INDEX:		// indexes start from 1, not 0
			ps = store_content(p_kmsi,STORE_NUMBER(*pr));
			if(*pr != *(ps + any_index[INDEX_OFFSET(*pr)-1])) return 0;	
			break;				// matched - continue matching string


		case ITEM_CONTEXT:
			k = CONTEXT_CHAR(*pr);
			if(k == m+1) break;				// wild-card match of input
			if((k == 0) || (k > rulelen)	// arg cannot be 0 (on LHS of rule)
				|| (*ph != *(ph+m+1-k))) return 0;		// no match 			
			break;				// matched - continue matching string

		case ITEM_NUL:			// nhistory (+ key) must be equal to the rule length
			if(rp->ilen != nhistory+usekeys) return 0;
			break;

//		case ITEM_OUTS:			// not allowed on LHS of rule - must be expanded by compiler
//			return 0;

//		case ITEM_MATCH:		// match and nomatch rules are be dealt with elsewhere
//		case ITEM_NOMATCH:
//			return 0;
		
//		case ITEM_BEEP:			// all other commands are only allowed on RHS of rule
//		case ITEM_USE:
//		case ITEM_CALL:
//			return 0;

		default:				// all other commands are only allowed on RHS of rule
			return 0;
		}
	}
	return 1;	// Return 1 if history matches rule
}

// Process a matched rule, return codes are:
//		1	rule processed
//		2	rule processed, return encountered (in rule or in subgroup rule)
//		-1	error - pass keystroke transparently
int process_rule(KMSI *p_kmsi, XRULE *rp, ITEM *any_index, int usekeys) 
{
	XGROUP *gp;
	UINT i, k, m, n, nout, itp;
	ITEM *p, *pr, *ps, output[MAX_OUTPUT+1], history[MAX_HISTORY], *it;
	int erase, result, retCode=1, nhistory;

	DBGMSG(1, "DAR - libkmfl - process_rule\n");
	pr = p_kmsi->strings+rp->rhs;	// Pointer to start of output rule

	// Make a temporary copy of the history before any modifications are made
	nhistory = p_kmsi->nhistory;
	for(i=0; (int)i<nhistory+1; i++)
	{
		history[i] = p_kmsi->history[i];
	}

	// Must erase the number of characters matched,
	// excluding the keystroke, and deadkeys, and nul, match or nomatch items  
	erase = rp->ilen - (usekeys ? 1 : 0);
	for(p=p_kmsi->strings+rp->lhs; erase>0 && p_kmsi->history>0; erase--,p++) 
	{
		itp = ITEM_TYPE(*p);
		switch(itp)
		{
		case ITEM_NUL:
		case ITEM_MATCH:
		case ITEM_NOMATCH:
			break;
		default:
			if(itp != ITEM_DEADKEY) erase_char(p_kmsi->connection);	
			for(i=1; i<p_kmsi->nhistory; i++) p_kmsi->history[i] = p_kmsi->history[i+1];
			p_kmsi->nhistory--;
			break;
		}
	}

	// Assemble the output needed (to replace the matched string)
	for(m=0,p=output; m<rp->olen; m++,pr++) 
	{
		switch(ITEM_TYPE(*pr)) 
		{
		case ITEM_CHAR:
			*p++ = *pr;
			break;

		case ITEM_INDEX:	// note that indexes start from 1, not 0
			ps = store_content(p_kmsi,STORE_NUMBER(*pr));
			it=ps + any_index[INDEX_OFFSET(*pr)-1];
			if (ITEM_TYPE(*it) == ITEM_BEEP)
			{
	                        DBGMSG(1, "DAR -libkmfl - *** index beep*** \n");
        	                output_beep(p_kmsi->connection);
			} else {
				*p++ = *it;
			}
			
			break;

		case ITEM_OUTS:		// only allowed on RHS of rule
			ps = store_content(p_kmsi,STORE_NUMBER(*pr));
			nout = store_length(p_kmsi,STORE_NUMBER(*pr));
			if(p-output+nout >= MAX_OUTPUT) return -2;
				
			for(n=0; n<nout; n++) *p++ = *ps++;
			break;

		case ITEM_DEADKEY:	// deadkeys are copied here, but won't actually be output
			*p++ = *pr;
			break;

		case ITEM_CONTEXT:	
			if((k=CONTEXT_CHAR(*pr)) == 0)
			{
				for(i=rp->ilen; i>(UINT)usekeys; i--)
				{
					*p++ = history[i-usekeys]; // was [i-1], corrected 2004/04/23
				}
			}
			else if(k <= rp->ilen-usekeys)
			{
				*p++ = history[rp->ilen-k+1-usekeys];
			}
			else return -1;	// this should be prevented by the compiler
			break;

		case ITEM_NUL:		// output nothing
			break;

		case ITEM_RETURN:	// return as soon as the output from this rule is finished
			retCode = 2;	// set return code to prevent processing match, nomatch, or use
			break;

		case ITEM_BEEP:		// output an audible signal
			DBGMSG(1, "DAR -libkmfl - ***beep*** \n");
			output_beep(p_kmsi->connection);
			break;

		case ITEM_USE:		// process another rule group then return here
			if(retCode == 2) break;	// do not process subgroup rules if return encountered
			
			gp = p_kmsi->groups+GROUP_NUMBER(*pr);
			if((result=process_group(p_kmsi,gp)) < 0) 
			{
				return -1;	// error processing subgroup rules
			}
			else if(result == 2)
			{
				retCode = 2;// return in subgroup: finish this rule, then exit
			}
			break;			

		case ITEM_CALL:		// not implemented, but not illegal
			break;

		case ITEM_KEYSYM:	
			*p++ = *pr;
			break;

//		case ITEM_ANY: // should be prevented by compiler
//		case ITEM_MATCH:
//		case ITEM_NOMATCH:
//			return (-1);		
		
		default:
			return -1;
		}
		
		// Get length of output string for this element
		nout = (UINT)(p-output);

		// Then output the output string (excepting deadkeys), and add it to the history
		for(n=0, p=output; n<nout; n++, p++) 
		{	
			if(ITEM_TYPE(*p) == ITEM_DEADKEY) 
			{
                add_to_history(p_kmsi,*p);
			}
			else
			{
	            if (ITEM_TYPE(*p) == ITEM_KEYSYM)
                {
					UINT key, state;
					key = (*p) & 0xFFFF;
					state = ((*p) >> 16) & 0xFF;
					DBGMSG(1, "DAR - libkmfl - ITEM_KEYSYM key:%x, state: %x\n", key, state);
                    forward_keyevent(p_kmsi->connection, key, state);
                    clear_history(p_kmsi);
                } 
                else
                {
                	output_item(p_kmsi->connection,*p);
                	add_to_history(p_kmsi,*p);
               	}
            }
		}

		// Reset pointer and continue
		p = output;
	}

	return retCode;	// Return 1 (or 2) to indicate that the keystroke has been matched and processed
}

// Add a character item (or deadkey) to the start of the history stack (to item 1)
void add_to_history(KMSI *p_kmsi,ITEM item) 
{
	ITEM *ip,*ip0=p_kmsi->history+1;
	UINT n=p_kmsi->nhistory;

	if(n >= MAX_HISTORY) n = MAX_HISTORY-1;
	p_kmsi->nhistory = n+1;

	for(ip=ip0+n-1; n>0; n--,ip--) *(ip+1) = *ip;

	*ip0 = item;

}

// Delete items (or deadkeys) from the start of the history stack (from item 1)
void delete_from_history(KMSI *p_kmsi,UINT nitems) 
{
	UINT nleft;

	if(p_kmsi->nhistory > MAX_HISTORY) p_kmsi->nhistory = MAX_HISTORY;

	if(nitems > p_kmsi->nhistory) nitems = p_kmsi->nhistory;
	nleft = p_kmsi->nhistory - nitems;
	if(nleft > 0 && nitems > 0)
	{
		memmove(p_kmsi->history+1,p_kmsi->history+nitems+1,nleft*sizeof(ITEM));
	}
	p_kmsi->nhistory = nleft;
//	*(p_kmsi->history+1+nleft) = 0; 
}

// Clear history
void clear_history(KMSI *p_kmsi) 
{
	p_kmsi->nhistory = 0;
}

// Output a Unicode character (as a multi-byte string)
void output_item(void *connection, ITEM x)
{
	UTF32 utfin[2]={0}, *pin;
	UTF8 utfout[16]={0}, *pout;
	ConversionResult result;
	utfin[0] = x; pin = &utfin[0]; pout = &utfout[0];

	result = ConvertUTF32toUTF8((const UTF32 **)&pin,utfin+1,&pout,utfout+15, 0);
	*pout = 0;
	output_string(connection, (char *)utfout);
}

// Return the address of the referenced store
ITEM *store_content(KMSI *p_kmsi, UINT nstore) 
{
	XSTORE *sp;

	sp = p_kmsi->stores + nstore;
	return(p_kmsi->strings+sp->items);
}

// Return the length of the referenced store
UINT store_length(KMSI *p_kmsi, UINT nstore) 
{
	XSTORE *sp;

	sp = p_kmsi->stores + nstore;
	return(sp->len);
}

// Translate the state integer received from scim_kmfl_server
UINT modified_state(UINT state)
{
	UINT right;

	right = (state & 0x0d00) >> 8;	// mask out all except RShift, RCtrl, RAlt
	state &= 0x0f;					// and all except LShift, Caps, LCtrl, LAlt
	state &= ~right;					// clear left bit if corresponding right bit set
	state |= (right << 4);			// and merge right bits

	return state;
}

// Compare the shift state with the state required by the rule (return 0 if matched)
UINT compare_state(ITEM rule_key, ITEM input_key)
{
	if((rule_key & (KS_SHIFT<<16)) == (KS_SHIFT<<16))	// test Shift first
	{
		if((input_key & (KS_SHIFT<<16)) == 0) return 1;	
	}
	else												// then LShift/RShift
	{
		if((rule_key & (KS_SHIFT<<16)) != (input_key & (KS_SHIFT<<16))) return 2;
	}

	if((rule_key & (KS_CTRL<<16)) == (KS_CTRL<<16))		// test Ctrl 
	{
		if((input_key & (KS_CTRL<<16)) == 0) return 3;
	}
	else												// then LCtrl/RCtrl
	{
		if((rule_key & (KS_CTRL<<16)) != (input_key & (KS_CTRL<<16))) return 4;
	}

	if((rule_key & (KS_ALT<<16)) == (KS_ALT<<16))		// test Alt first
	{
		if((input_key & (KS_ALT<<16)) == 0) return 5;
	}
	else												// then LAlt/RAlt
	{
		if((rule_key & (KS_ALT<<16)) != (input_key & (KS_ALT<<16))) return 6;
	}

	if((rule_key & (KS_CAPS<<16)) == (KS_CAPS<<16))		// test Caps
	{
		if((input_key & (KS_CAPS<<16)) == 0) return 7;
	}

	if((rule_key & (KS_NCAPS<<16)) == (KS_NCAPS<<16))	// and NCaps
	{
		if((input_key & (KS_CAPS<<16)) != 0) return 8;
	}

	return 0;		// input state matches state in rule
}

// Return a header referenced by special header ID number 
int kmfl_get_header(KMSI *p_kmsi,int hdrID,char *buf,int buflen)
{
	UTF32 *p32;
	UTF8 *p8;

	int nitems;

	if(!p_kmsi) return -1;

	if(hdrID < 0 || hdrID > SS_AUTHOR) return -2;

	p32 = (UTF32 *)store_content(p_kmsi,hdrID);
	p8 = (UTF8 *)buf;

	if(p32 == NULL) return -3;

	nitems = store_length(p_kmsi,hdrID);
	if(nitems == 0) return -4;
	
	memset(buf,0,buflen);
	return ConvertUTF32toUTF8((const UTF32**)&p32,p32+nitems,&p8,p8+buflen-1,0);
}

