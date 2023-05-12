#ifndef __SERVERMETHODS_H__
#define __SERVERMETHODS_H__

#ifdef __cplusplus
extern "C" {
#endif

void*    get_kbd_devices();
void     close_kbd_devices(void *data);
void     set_caps_lock_indicator_on_devices(void *data, void *user_data);
uint32_t get_caps_lock_indicator_on_devices(void *data);

#ifdef __cplusplus
}
#endif

#endif // __SERVERMETHODS_H__
