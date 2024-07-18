/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KeySender.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-04-17.
 * 
 */

NS_ASSUME_NONNULL_BEGIN

extern const CGKeyCode kKeymanEventKeyCode;

@interface KeySender : NSObject

- (instancetype)init;
- (void)sendBackspaceforEventSource:(CGEventSourceRef)eventSource;
- (void)sendKeymanKeyCodeForEvent:(NSEvent *)event;

@end

NS_ASSUME_NONNULL_END
