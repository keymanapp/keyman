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
- (void)sendKeyDown:(NSUInteger)keyCode forSourceEvent:(NSEvent *)event includeKeyUp:(BOOL)includeKeyUpEvent;
- (void)sendBackspaceforSourceEvent:(NSEvent *)event;
- (void)sendKeymanKeyCodeForEvent:(NSEvent *)event;

@end

NS_ASSUME_NONNULL_END
