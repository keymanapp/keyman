
// LIBKMFL.H: Header for interpreter for Keyboard Mapping for Linux

#ifndef LIBKMFL_H

#ifdef	__cplusplus
extern "C" {
#endif
	
int kmfl_interpret(KMSI *p_kmsi, UINT key, UINT state);

int kmfl_load_keyboard(const char *file);
int kmfl_check_keyboard(const char *file);
int kmfl_unload_keyboard(int keyboard_number);
int kmfl_unload_all_keyboards(void);

KMSI *kmfl_make_keyboard_instance(void *connection);
int kmfl_delete_keyboard_instance(KMSI *p_kmsi);
int kmfl_delete_all_keyboard_instances(void);

int kmfl_attach_keyboard(KMSI *p_kmsi, int keyboard_number);
int kmfl_detach_keyboard(KMSI *p_kmsi);

int kmfl_keyboard_number(char *name);
const char *kmfl_keyboard_name(int keyboard_number);
const char *kmfl_icon_file(int keyboard_number);

int kmfl_get_header(KMSI *p_kmsi,int hdrID,char *buf,int buflen);

void DBGMSG(int debug,char *fmt,...);
void *ERRMSG(char *fmt,...);
void clear_history(KMSI *p_kmsi);

extern int kmfl_debug;

#ifdef	__cplusplus
}
#endif	

#endif /* *** end of LIBKMFL.H *** */
