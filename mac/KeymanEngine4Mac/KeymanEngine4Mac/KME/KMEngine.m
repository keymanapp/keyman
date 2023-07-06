//
//  KMEngine.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMEngine.h"
#import "KMBinaryFileFormat.h"
#import "KMCompStore.h"
#import "KMCompGroup.h"
#import "KMCompKey.h"
#import "NSArray+Action.h"
#import "NSString+XString.h"
#import "WindowsVKCodes.h"
#import "MacVKCodes.h"
#import "CoreWrapper.h"
@import Carbon;

NSString *const Q_STR = @"Q_STR";
NSString *const Q_BACK = @"Q_BACK";
NSString *const Q_DEADKEY = @"Q_DEADKEY";
NSString *const Q_NUL = @"Q_NUL";
NSString *const Q_BEEP = @"Q_BEEP";
NSString *const Q_RETURN = @"Q_RETURN";
NSString *const Q_SAVEOPT = @"Q_SAVEOPT";

DWORD VKMap[0x80];

@interface KMEngine ()
+ (NSRegularExpression *)regexPlatform;
@property (strong, nonatomic) NSMutableString *tmpCtxBuf;
@property (strong, nonatomic) NSMutableArray *indexStack;
@property (readonly) CoreHelper *coreHelper;
@property (nonatomic, retain) CoreWrapper * keymanCore;
@end

@implementation KMEngine

+ (NSRegularExpression *)regexPlatform {
    static NSRegularExpression *regex = nil;
    if (regex == nil) {
        regex = [NSRegularExpression regularExpressionWithPattern:@"(\\b(((mac(os)?)x?)|(native)|(hardware)|(desktop)) *)*" options:NSRegularExpressionCaseInsensitive error:nil];
    }
    return regex;
}

NSMutableString* _easterEggForSentry = nil;
const NSString* kEasterEggText = @"Sentrycrash#KME";
const NSString* kEasterEggKmxName = @"EnglishSpanish.kmx";

- (id)initWithKMX:(KMXFile *)kmx contextBuffer:(NSString *)ctxBuf {
    self = [super init];
    if (self) {
#ifdef DEBUG
        self.debugMode = YES;
#else
        self.debugMode = NO;
#endif
        _kmx = kmx;
        _coreHelper = [[CoreHelper alloc] init];
      
      if (kmx) {
        [self loadCoreWrapperFromKmxFile:self.kmx.filePath];
        [self.keymanCore setContext:ctxBuf];
      }

        _tmpCtxBuf = [[NSMutableString alloc] initWithString:ctxBuf];
        [_tmpCtxBuf removeAllNullChars];
        if (_tmpCtxBuf.length)
            [_tmpCtxBuf replaceOccurrencesOfString:@"\0" withString:@"" options:0 range:NSMakeRange(0, 1)];
        [self setVKMapping];
    }

    return self;
}

-(void)loadCoreWrapperFromKmxFile:(NSString *)kmxFilePath {
  NSLog(@"**SGS loading wrapper from kmx file: %@", kmxFilePath);
  
  @try {
    _keymanCore = [[CoreWrapper alloc] initWithHelper:_coreHelper kmxFilePath:kmxFilePath];
    NSLog(@"**SGS CoreWrapper id = %@", [self.keymanCore keyboardId]);
  }
  @catch (NSException *exception) {
    NSLog(@"**SGS failed to create keyboard for path '%@' with exception: %@", kmxFilePath, exception.description);
  }
}

-(void)setKmx:(KMXFile*) kmxFile {
  if (_kmx!=kmxFile) {
    _kmx = kmxFile;
    // TODO: is it valid to set kmx to nil? do we then dispose of the wrapper?
    if (kmxFile != nil) {
      [self loadCoreWrapperFromKmxFile:kmxFile.filePath];
    }
  }
}

- (void)setUseVerboseLogging:(BOOL)useVerboseLogging {
    self.debugMode = useVerboseLogging;

    if (useVerboseLogging) {
        NSLog(@"KME - Turning verbose logging on");
        // In Keyman Engine if "debugMode" is turned on (explicitly) with "English plus Spanish" as the current keyboard and you type "Sentrycrash#KME",
        // it will force a simulated crash to test reporting to sentry.keyman.com.
        NSString * kmxName = [[_kmx filePath] lastPathComponent];
        NSLog(@"Sentry - KME: _kmx name = %@", kmxName);
        if ([kEasterEggKmxName isEqualToString:kmxName]) {
            NSLog(@"Sentry - KME: Preparing to detect Easter egg.");
            _easterEggForSentry = [[NSMutableString alloc] init];
        }
        else
            _easterEggForSentry = nil;
    }
    else
        NSLog(@"KME - Turning verbose logging off");
}

- (void)setCoreContext:(NSString *)context {
  [self.keymanCore setContext:context];
}

- (void)setContextBuffer:(NSString *)ctxBuf {
    _tmpCtxBuf = [[NSMutableString alloc] initWithString:ctxBuf];
    [_tmpCtxBuf removeAllNullChars];
    if (_tmpCtxBuf.length)
        [_tmpCtxBuf replaceOccurrencesOfString:@"\0" withString:@"" options:0 range:NSMakeRange(0, 1)];
}

- (NSString *)contextBuffer {
    return [NSString stringWithString:self.tmpCtxBuf];
}

- (NSMutableString *)tmpCtxBuf {
    if (_tmpCtxBuf == nil) {
        _tmpCtxBuf = [[NSMutableString alloc] initWithString:@""];
    }

    return _tmpCtxBuf;
}

- (void)setStore:(DWORD)storeID withValue:(NSString *)value {
    KMCompStore *store = [self.kmx.store objectAtIndex:storeID];
    KMCompStore *storeSaved = [self.kmx.storeSaved objectAtIndex:storeID];
    store.string = [[NSString alloc] initWithString:value];
    storeSaved.string = [[NSString alloc] initWithString:value];
}

/*
 * Returns an NSArray of NSDictionary objects, one dictionary per action. Returns nil if no actions result.
 */
- (NSArray *)processEvent:(NSEvent *)event {
  if (!self.kmx)
      return nil;

  if (self.keymanCore) {
    NSLog(@"**SGS KME processEvent using CoreWrapper");
    // CoreWrapper returns an array of CoreAction objects
    NSArray *coreActions = [self.keymanCore processEvent:event];
    // TODO: make this nil assignment part of the conversion method
    if ([coreActions count] == 0) {
      return nil;
    } else {
      NSLog(@"**SGS KME processEvent current context according to Core: %@", self.keymanCore.context);
      // update context from core
      [self.tmpCtxBuf setString:self.keymanCore.context];
      return coreActions;
      // TODO: integrate the different contexts
      // -- probably ignore/trash tmpCtxBuf because we update local context using actions
    }
  } else {
    NSArray *actions = nil;
    NSInteger startIndex = [[self.kmx.startGroup objectAtIndex:1] integerValue];
    if (startIndex < 0 || startIndex >= [self.kmx.group count])
        startIndex = [[self.kmx.startGroup objectAtIndex:0] integerValue];
    if (startIndex < 0 || startIndex >= [self.kmx.group count])
        return nil;

    KMCompGroup *gp = [self.kmx.group objectAtIndex:startIndex];
    actions = [self processGroup:gp event:event];

    if (actions.count == 0 && _easterEggForSentry != nil) {
        [self processPossibleEasterEggCharacterFrom:[event characters]];
    }

    return [[actions mutableCopy] optimise];
  }
}

/*
 * For unit testing purposes to control whether executing against KME code or Core code
 */
// TODO: remove
- (NSArray *)experimentallyProcessEventForUnitTestingOnly:(NSEvent *)event usingCore:(BOOL)useCore {
  if (!self.kmx)
      return nil;

  if (self.keymanCore && useCore) {
    NSLog(@"**SGS KME processEvent using CoreWrapper");
    // CoreWrapper returns an array of CoreAction objects
    NSArray *coreActions = [self.keymanCore processEvent:event];
    // TODO: make this nil assignment part of the conversion method
    if ([coreActions count] == 0) {
      return nil;
    } else {
      // update context from core
     [self.tmpCtxBuf setString:self.keymanCore.context];
      return coreActions;
    }
  } else {
    NSArray *actions = nil;
    NSInteger startIndex = [[self.kmx.startGroup objectAtIndex:1] integerValue];
    if (startIndex < 0 || startIndex >= [self.kmx.group count])
        startIndex = [[self.kmx.startGroup objectAtIndex:0] integerValue];
    if (startIndex < 0 || startIndex >= [self.kmx.group count])
        return nil;

    KMCompGroup *gp = [self.kmx.group objectAtIndex:startIndex];
    actions = [self processGroup:gp event:event];

    if (actions.count == 0 && _easterEggForSentry != nil) {
        [self processPossibleEasterEggCharacterFrom:[event characters]];
    }

    return [[actions mutableCopy] optimise];
  }
}


- (void) processPossibleEasterEggCharacterFrom:(NSString *)characters {
    NSUInteger len = [_easterEggForSentry length];
    NSLog(@"Sentry - KME: Processing character(s): %@", characters);
    if ([characters length] == 1 && [characters characterAtIndex:0] == [kEasterEggText characterAtIndex:len]) {
        NSString *characterToAdd = [kEasterEggText substringWithRange:NSMakeRange(len, 1)];
        NSLog(@"Sentry - KME: Adding character to Easter Egg code string: %@", characterToAdd);
        [_easterEggForSentry appendString:characterToAdd];
        if ([kEasterEggText isEqualToString:_easterEggForSentry]) {
            NSLog(@"Sentry - KME: Forcing crash now!");
            // Both of the following approaches do throw an exception that causes control to exit this method,
            // but at least in my debug builds locally, neither one seems to get picked up by Sentry in a
            // way that results in a new report on sentry.keyman.com

#ifndef USE_ALERT_SHOW_HELP_TO_FORCE_EASTER_EGG_CRASH_FROM_ENGINE
            //#1
            @throw ([NSException exceptionWithName:@"SentryForce" reason:@"Easter egg hit" userInfo:nil]);

            //#2
            //    NSDecimalNumber *i = [NSDecimalNumber decimalNumberWithDecimal:[@(1) decimalValue]];
            //    NSDecimalNumber *o = [NSDecimalNumber decimalNumberWithDecimal:[@(0) decimalValue]];
            //    // Divide by 0 to throw an exception
            //    NSDecimalNumber *x = [i decimalNumberByDividingBy:o];

#else
            //#3 The following DOES work, but it's really lame because the crash actually gets forced in the IM
            // via this bogus call to a protocol method implemented in the IM's App Delegate just for the
            // purpose of enabling the engine to force a crash.
            [(NSObject <NSAlertDelegate> *)[NSApp delegate] alertShowHelp:[NSAlert alertWithMessageText:@"Forcing an error" defaultButton:nil alternateButton:nil otherButton:nil informativeTextWithFormat:@"Forcing an Easter egg error from KME!"]];
#endif

            NSLog(@"Sentry - KME: You should not be seeing this line!");
        }
    }
    else if (len > 0) {
        NSLog(@"Sentry - KME: Clearing Easter Egg code string.");
        [_easterEggForSentry setString:@""];
    }
}

- (NSArray *)processGroup:(KMCompGroup *)gp event:(NSEvent *)event {
    if (!gp)
        return nil;

    if (event.type != NSKeyDown || ![event respondsToSelector:@selector(keyCode)])
        return nil;

    if (([event modifierFlags] & NSEventModifierFlagCommand) == NSEventModifierFlagCommand)
        return nil; // Engine should NEVER attempt to process characters when the Command key is pressed.

    // REVIEW: Might need to use charactersIgnoringModifiers instead of characters to avoid
    // getting Mac predefined subsitutions for Option + ??? keystrokes. But at least for the normal
    // case of shifted characters, what we have is what we want.
    NSString *characters = [event characters];
    //NSString *characters = [event charactersIgnoringModifiers];
    if (self.debugMode) {
        NSLog(@"####### ENGINE INFO #######");
        NSLog(@"%lu characters = %@", [[event characters] length], characters);
        NSLog(@"%lu characters ignoring modifiers = %@", [[event charactersIgnoringModifiers] length], [event charactersIgnoringModifiers]);
    }

    NSArray *actions = nil;

    if (gp.fUsingKeys) {
        // Begin group using keys
        unsigned short keyCode = [event keyCode];
        unichar vChar = [characters characterAtIndex:0];
        DWORD vkCode = VKMap[keyCode];
        NSMutableArray *mKeys = [NSMutableArray arrayWithCapacity:0];
        for (KMCompKey *kmKey in gp.keys) {
            DWORD flags = kmKey.shiftFlags;
            if (![self.kmx isMnemonic] && ((flags & VIRTUALCHARKEY) == VIRTUALCHARKEY))
                continue;

            if ((flags & (ISVIRTUALKEY | VIRTUALCHARKEY)) == (ISVIRTUALKEY | VIRTUALCHARKEY)) {
                NSString *chars = [self getCharsFromKeyCode:[event keyCode]];
                if (chars.length && ((kmKey.key == [chars characterAtIndex:0]) || (kmKey.key == [[chars uppercaseString] characterAtIndex:0])))
                    [mKeys addObject:kmKey];
            }
            // TODO: Need tests remainder of this logic
            else if ((flags & ISVIRTUALKEY) == ISVIRTUALKEY) {
                if (kmKey.key == vkCode)
                    [mKeys addObject:kmKey];
            }
            else if (kmKey.key == vChar) {
                [mKeys addObject:kmKey];
            }
        }

        if (self.debugMode) {
            NSLog(@"Matching keys = %@", mKeys);
            NSLog(@"[event modifierFlags] = %lX", [event modifierFlags]);
        }
        DWORD mask = 0;
        if (([event modifierFlags] & NSEventModifierFlagShift) == NSEventModifierFlagShift) {
            if (self.debugMode)
                NSLog(@"Adding SHIFT flag to mask.");
            mask |= K_SHIFTFLAG;
        }

        if (([event modifierFlags] & NSEventModifierFlagCapsLock) == NSEventModifierFlagCapsLock) {
            if (self.debugMode)
                NSLog(@"Adding CAPITAL flag to mask.");
            mask |= CAPITALFLAG;
        }

        if (([event modifierFlags] & MK_LEFT_ALT_MASK) == MK_LEFT_ALT_MASK) {
            if (self.debugMode)
                NSLog(@"Adding LEFT ALT flag to mask.");
            mask |= LALTFLAG;
        }
        else if (([event modifierFlags] & MK_RIGHT_ALT_MASK) == MK_RIGHT_ALT_MASK) {
            if (self.debugMode)
                NSLog(@"Adding RIGHT ALT flag to mask.");
            mask |= RALTFLAG;
        }
        else if (([event modifierFlags] & NSEventModifierFlagOption) == NSEventModifierFlagOption) {
            if (self.debugMode)
                NSLog(@"Adding ALT flag to mask.");
            mask |= K_ALTFLAG;
        }
        if (([event modifierFlags] & MK_LEFT_CTRL_MASK) == MK_LEFT_CTRL_MASK) {
            if (self.debugMode)
                NSLog(@"Adding LCTRL flag to mask.");
            mask |= LCTRLFLAG;
        }
        else if (([event modifierFlags] & MK_RIGHT_CTRL_MASK) == MK_RIGHT_CTRL_MASK) {
            if (self.debugMode)
                NSLog(@"Adding RCTRL flag to mask.");
            mask |= RCTRLFLAG;
        }
        else if (([event modifierFlags] & NSEventModifierFlagControl) == NSEventModifierFlagControl) {
            if (self.debugMode)
                NSLog(@"Adding CTRL flag to mask.");
            mask |= K_CTRLFLAG;
        }
        if (self.debugMode)
            NSLog(@"Mask = %X", mask);

        KMCompKey *mKey = nil;
        for (KMCompKey *key in mKeys) {
            if (key.shiftFlags & ISVIRTUALKEY) {
                DWORD kMask = mask;
                if (self.debugMode)
                    NSLog(@"Has shift flags: %X", key.shiftFlags);
                if ((kMask & CAPITALFLAG) == 0)
                    kMask |= NOTCAPITALFLAG;
                if ((key.shiftFlags & K_CTRLFLAG) && (kMask & LCTRLFLAG)) {
                    kMask |= K_CTRLFLAG;
                    kMask ^= LCTRLFLAG;
                }
                if ((key.shiftFlags & K_CTRLFLAG) && (kMask & RCTRLFLAG)) {
                    kMask |= K_CTRLFLAG;
                    kMask ^= RCTRLFLAG;
                }
                if ((key.shiftFlags & K_ALTFLAG) && (kMask & LALTFLAG)) {
                    kMask |= K_ALTFLAG;
                    kMask ^= LALTFLAG;
                }
                if ((key.shiftFlags & K_ALTFLAG) && (kMask & RALTFLAG)) {
                    kMask |= K_ALTFLAG;
                    kMask ^= RALTFLAG;
                }

                DWORD mFlag = key.shiftFlags & K_MODIFIERFLAG;
                DWORD mMask = kMask & K_MODIFIERFLAG;
                if (self.debugMode) {
                    //NSLog(@"flags = %X", flags);
                    NSLog(@"mFlag = %X", mFlag);
                    NSLog(@"mMask = %X", mMask);
                }
                // If key.shiftFlags has neither CAPITALMASK nor NOTCAPITALMASK,
                // we ignore Caps state, otherwise Caps state must match
                // precisely
                if ((key.shiftFlags & K_CAPITALMASK) == 0 || 
                        (key.shiftFlags & K_CAPITALMASK) == (kMask & K_CAPITALMASK)) {
                    if (mMask == mFlag) {
                        if (!key.context.length) {
                            mKey = key;
                            break;
                        }
                        else if ([self contextMatch:key]) {
                            mKey = key;
                            break;
                        }
                    }
                }
            }
            else if (!(mask & K_MODIFIERFLAG) || mask == K_SHIFTFLAG) {
                //if (self.debugMode)
                //NSLog(@"No shift flags!");
                if (!key.context.length) {
                    mKey = key;
                    break;
                }
                else if ([self contextMatch:key]) {
                    mKey = key;
                    break;
                }
            }
        }

        /*
         // Fall back if no match
         if (!mKey && mKeys.count == 1) {
         mKey = [mKeys objectAtIndex:0];
         }
         else if (!mKey && mKeys.count > 1) {
         for (KMCompKey *key in mKeys) {
         if (!key.context.length) {
         mKey = key;
         }
         else {
         if ([self contextMatch:key]) {
         mKey = key;
         break;
         }
         }
         }
         }*/

        if (self.debugMode)
            NSLog(@"mKey = %@", mKey);
        if (mKey == nil)
            actions = [self processNoMatch:gp.noMatch event:event usingKeys:YES];
        else {
            actions = [self processKey:mKey event:event];

            NSArray *mActions = [self processMatch:gp.match event:event];
            if (mActions) {
                NSMutableArray *nActions = [NSMutableArray arrayWithArray:actions];
                for (NSDictionary *action in mActions)
                    [nActions addAction:action];
                actions = [NSArray arrayWithArray:nActions];
            }
        }
    }
    else {
        // Begin group not using keys
        KMCompKey *mKey = nil;
        for (KMCompKey *key in gp.keys) {
            if ([self contextMatch:key]) {
                mKey = key;
                break;
            }
        }

        if (self.debugMode)
            NSLog(@"mKey! = %@", mKey);
        if (mKey == nil)
            actions = [self processNoMatch:gp.noMatch event:event usingKeys:NO];
        else {
            actions = [self processKey:mKey event:event];

            NSArray *mActions = [self processMatch:gp.match event:event];
            if (mActions) {
                NSMutableArray *nActions = [NSMutableArray arrayWithArray:actions];
                for (NSDictionary *action in mActions)
                    [nActions addAction:action];
                actions = [NSArray arrayWithArray:nActions];
            }
        }
    }

    return actions;
}

- (NSArray *)processKey:(KMCompKey *)key event:(NSEvent *)event {
    if (!key)
        return nil;

    NSMutableArray *actions = nil;
    NSUInteger cx = [self contextMatch:key]%1000;
    NSString *ctx = @"";
    if (cx) {
        if (self.debugMode)
            NSLog(@"cx = %lu", (unsigned long)cx);
        ctx = [self.tmpCtxBuf lastNChars:cx];
        [self.tmpCtxBuf deleteLastNChars:cx];
        NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:[NSNumber numberWithUnsignedInteger:cx], Q_BACK, nil];
        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
    }

    NSString *outKey = key.output;
    if (!outKey || !outKey.length)
        return nil;

    BOOL handledWithoutActions = NO;

    for (int i = 0; i < outKey.length;) {
        unichar c = [outKey characterAtIndex:i];
        if (c == UC_SENTINEL) {
            c = [outKey characterAtIndex:i+1];
            switch (c) {
                case CODE_INDEX: {
                    DWORD x1 = [outKey characterAtIndex:i+2];
                    DWORD x2 = [outKey characterAtIndex:i+3];
                    KMCompStore *store = [self.kmx.store objectAtIndex:x1 - 1];
                    NSUInteger n = [[self.indexStack objectAtIndex:x2 - 1] unsignedIntegerValue];
                    if (self.debugMode)
                        NSLog(@"indexStack = %@", self.indexStack);
                    NSString *outChar = [NSString stringWithFormat:@"%C",
                                         [store.string characterAtIndex:n]];
                    if (self.debugMode)
                        NSLog(@"%@[%lu] = %@", store.string, n, outChar);
                    [self.tmpCtxBuf appendString:outChar];
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:outChar, Q_STR, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];

                    i+=4;
                    break;
                }
                case CODE_CONTEXT: {
                    if (self.debugMode)
                        NSLog(@"code_context()");
                    [self.tmpCtxBuf appendString:ctx];
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:ctx, Q_STR, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];

                    i+=2;
                    break;
                }
                case CODE_CONTEXTEX: {
                    DWORD index = [outKey characterAtIndex:i+2];
                    NSString *outChar = [NSString stringWithFormat:@"%C",
                                         [ctx characterAtIndex:index - 1]];
                    if (self.debugMode)
                        NSLog(@"contextex(%d) = %@", index, outChar);
                    [self.tmpCtxBuf appendString:outChar];
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:outChar, Q_STR, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];

                    i+=3;
                    break;
                }
                case CODE_DEADKEY: {
                    DWORD index = [outKey characterAtIndex:i+2];
                    [self.tmpCtxBuf appendDeadkey:index];
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:[NSNumber numberWithUnsignedInteger:index], Q_DEADKEY, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];
                    i+=3;
                    break;
                }
                case CODE_NUL: {
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_NUL, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];
                    i+=2;
                    break;
                }
                case CODE_USE: {
                    DWORD index = [outKey characterAtIndex:i+2];
                    KMCompGroup *gp = [self.kmx.group objectAtIndex:index-1];
                    NSArray *nActions = [self processGroup:gp event:event];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithArray:nActions];
                    else {
                        for (NSDictionary *action in nActions)
                            [actions addAction:action];
                    }

                    i+=3;
                    break;
                }
                case CODE_BEEP: {
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_BEEP, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];

                    i+=2;
                    break;
                }
                case CODE_RETURN: {
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_RETURN, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];

                    i+=2;
                    break;
                }
                case CODE_SETOPT: {
                    DWORD x1 = [outKey characterAtIndex:i+2]-1;
                    DWORD x2 = [outKey characterAtIndex:i+3]-1;
                    KMCompStore *store1 = [self.kmx.store objectAtIndex:x1];
                    KMCompStore *store2 = [self.kmx.store objectAtIndex:x2];
                    store1.string = [[NSString alloc] initWithString:store2.string];
                    handledWithoutActions = YES;

                    i+=4;
                    break;
                }

                case CODE_RESETOPT: {
                    DWORD x1 = [outKey characterAtIndex:i+2]-1;
                    KMCompStore *store = [self.kmx.store objectAtIndex:x1];
                    KMCompStore *storeSaved = [self.kmx.storeSaved objectAtIndex:x1];
                    store.string = [[NSString alloc] initWithString:storeSaved.string];
                    handledWithoutActions = YES;
                    i+=3;
                    break;
                }
                case CODE_SAVEOPT: {
                    DWORD x1 = [outKey characterAtIndex:i+2]-1;
                    KMCompStore *store = [self.kmx.store objectAtIndex:x1];
                    KMCompStore *storeSaved = [self.kmx.storeSaved objectAtIndex:x1];
                    storeSaved.string = [[NSString alloc] initWithString:store.string];

                    NSDictionary *actionObj = [[NSDictionary alloc] initWithObjectsAndKeys:[[NSString alloc] initWithString:store.string], [NSNumber numberWithUnsignedInteger:x1], nil];
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:actionObj, Q_SAVEOPT, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];

                    i+=3;
                    break;
                }
                case CODE_SETSYSTEMSTORE:
                    i+=4;
                    break;
                default:
                    i+=2;
                    break;
            }
        }
        else {
            NSString *outChar = [NSString stringWithFormat:@"%C", c];
            [self.tmpCtxBuf appendString:outChar];
            NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:outChar, Q_STR, nil];
            if (!actions)
                actions = [[NSMutableArray alloc] initWithObjects:action, nil];
            else
                [actions addAction:action];

            i++;
        }

        if (self.debugMode)
            NSLog(@"tmpCtxBuf = \"%@\"", [self.tmpCtxBuf codeString]);
    }

    if (!actions && handledWithoutActions) {
        NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_NUL, nil];
        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
    }

    return actions;
}

- (NSArray *)processMatch:(NSString *)match event:(NSEvent *)event {
    if (!match || !match.length)
        return nil;

    if (self.debugMode)
        NSLog(@"Processing match rule = %@", [match codeString]);

    NSMutableArray *actions = nil;

    for (int i = 0; i < match.length;) {
        unichar c = [match characterAtIndex:i];
        if (c == UC_SENTINEL) {
            c = [match characterAtIndex:i+1];
            switch (c) {
                case CODE_USE: {
                    DWORD index = [match characterAtIndex:i+2];
                    KMCompGroup *gp = [self.kmx.group objectAtIndex:index-1];
                    NSArray *nActions = [self processGroup:gp event:event];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithArray:nActions];
                    else {
                        for (NSDictionary *action in nActions)
                            [actions addAction:action];
                    }

                    i+=3;
                    break;
                }
                case CODE_RETURN: {
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_RETURN, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];
                    i+=2;
                    break;
                }
                case CODE_BEEP: {
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_BEEP, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];
                    i+=2;
                    break;
                }
                default:
                    i+=2;
                    break;
            }
        }
        else {
            NSString *outChar = [NSString stringWithFormat:@"%C", c];
            [self.tmpCtxBuf appendString:outChar];
            NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:outChar, Q_STR, nil];
            if (!actions)
                actions = [[NSMutableArray alloc] initWithObjects:action, nil];
            else
                [actions addAction:action];

            i++;
        }

        if (self.debugMode)
            NSLog(@"tmpCtxBuf = \"%@\"", [self.tmpCtxBuf codeString]);
    }

    return actions;
}

- (NSArray *)processNoMatch:(NSString *)noMatch event:(NSEvent *)event usingKeys:(BOOL)usingKeys {
    // * return nil if the current keystroke does not produce a character in normal use
    if (!noMatch || !noMatch.length || (VKMap[[event keyCode]] < 32))
        return nil;

    if (self.debugMode)
        NSLog(@"Processing NoMatch rule = %@", [noMatch codeString]);

    NSMutableArray *actions = nil;

    for (int i = 0; i < noMatch.length;) {
        unichar c = [noMatch characterAtIndex:i];
        if (c == UC_SENTINEL) {
            c = [noMatch characterAtIndex:i+1];
            switch (c) {
                case CODE_USE: {
                    DWORD index = [noMatch characterAtIndex:i+2];
                    KMCompGroup *gp = [self.kmx.group objectAtIndex:index-1];
                    NSArray *nActions = [self processGroup:gp event:event];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithArray:nActions];
                    else {
                        for (NSDictionary *action in nActions)
                            [actions addAction:action];
                    }

                    i+=3;
                    break;
                }
                case CODE_RETURN: {
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_RETURN, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];
                    i+=2;
                    break;
                }
                case CODE_BEEP: {
                    NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:@"", Q_BEEP, nil];
                    if (!actions)
                        actions = [[NSMutableArray alloc] initWithObjects:action, nil];
                    else
                        [actions addAction:action];
                    i+=2;
                    break;
                }
                default:
                    i+=2;
                    break;
            }
        }
        else {
            NSString *outChar = [NSString stringWithFormat:@"%C", c];
            [self.tmpCtxBuf appendString:outChar];
            NSDictionary *action = [[NSDictionary alloc] initWithObjectsAndKeys:outChar, Q_STR, nil];
            if (!actions)
                actions = [[NSMutableArray alloc] initWithObjects:action, nil];
            else
                [actions addAction:action];

            i++;
        }

        if (self.debugMode)
            NSLog(@"tmpCtxBuf = \"%@\"", [self.tmpCtxBuf codeString]);
    }

    return actions;
}

- (NSUInteger)contextMatch:(KMCompKey *)cpKey {
    NSString *keyCtx = cpKey.context;
    //if (/*!self.tmpCtxBuf.length ||*/ !keyCtx.length)
    //    return 0;

    // Context match length
    NSUInteger cx = keyCtx.xLength;
    NSUInteger cxIf = keyCtx.xLengthIncludingIf;
    if (self.tmpCtxBuf.xLength < cx)
        return 0;

    //NSString *ctx = [ctxBuf substringFromIndex:ctxBuf.length - cx];
    NSString *ctx = [self.tmpCtxBuf lastNChars:cx];
    //if (self.debugMode)
        //NSLog(@"contextMatch \"%@\" ? \"%@\"", keyCtx.codeString, ctx.codeString);
    self.indexStack = [[NSMutableArray alloc] initWithCapacity:cxIf];
    for (int i = 0; i < cxIf; i++)
        [self.indexStack addObject:[NSNumber numberWithUnsignedInteger:0]];

    BOOL checkNul = NO;
    BOOL checkPlatform = NO;
    int sx = 0; // index stack counter
    int px = 0; // context buffer position counter
    for (int i = 0; i < keyCtx.length;) {
        unichar c = [keyCtx characterAtIndex:i];
        if (c == UC_SENTINEL) {
            c = [keyCtx characterAtIndex:i+1];
            switch (c) {
                case CODE_ANY: {
                    DWORD index = [keyCtx characterAtIndex:i+2];
                    KMCompStore *store = [self.kmx.store objectAtIndex:index-1];
                    NSString *strKey = [NSString stringWithFormat:@"%C", [ctx characterAtIndex:px]];
                    NSCharacterSet *cset = [NSCharacterSet characterSetWithCharactersInString:strKey];
                    NSUInteger loc = [store.string rangeOfCharacterFromSet:cset].location;
                    //NSUInteger loc = [store.string rangeOfString:strKey].location;
                    if (loc == NSNotFound)
                        return 0;
                    else
                        [self.indexStack setObject:[NSNumber numberWithUnsignedInteger:loc] atIndexedSubscript:sx];

                    px++;
                    sx++;
                    i+=3;
                    break;
                }
                case CODE_NOTANY: {
                    DWORD index = [keyCtx characterAtIndex:i+2];
                    KMCompStore *store = [self.kmx.store objectAtIndex:index-1];
                    NSString *strKey = [NSString stringWithFormat:@"%C",
                                        [ctx characterAtIndex:px]];
                    NSCharacterSet *cset = [NSCharacterSet characterSetWithCharactersInString:strKey];
                    NSUInteger loc = [store.string rangeOfCharacterFromSet:cset].location;
                    //NSUInteger loc = [store.string rangeOfString:strKey].location;
                    if (loc != NSNotFound)
                        return 0;

                    px++;
                    sx++;
                    i+=3;
                    break;
                }
                case CODE_INDEX: {
                    DWORD x1 = [keyCtx characterAtIndex:i+2];
                    DWORD x2 = [keyCtx characterAtIndex:i+3];
                    KMCompStore *store = [self.kmx.store objectAtIndex:x1 - 1];
                    NSUInteger n = [[self.indexStack objectAtIndex:x2 - 1] unsignedIntegerValue];
                    if ([ctx characterAtIndex:px] != [store.string characterAtIndex:n])
                        return 0;

                    px++;
                    sx++;
                    i+=4;
                    break;
                }
                case CODE_CONTEXTEX: {
                    DWORD index = [keyCtx characterAtIndex:i+2];
                    c = [ctx characterAtIndex:index - 1];
                    if ([ctx characterAtIndex:px] != c)
                        return 0;

                    px++;
                    sx++;
                    i+=3;
                    break;
                }
                case CODE_DEADKEY: {
                    DWORD index = [keyCtx characterAtIndex:i+2];
                    if ([ctx characterAtIndex:px] == UC_SENTINEL && [ctx characterAtIndex:px+1] == CODE_DEADKEY) {
                        px+=2;
                        c = [[NSString stringWithFormat:@"%C", index] characterAtIndex:0];
                        unichar c2 = [ctx characterAtIndex:px];
                        if (c != c2)
                            return 0;
                    }
                    else
                        return 0;

                    px++;
                    sx++;
                    i+=3;
                    break;
                }
                case CODE_IFOPT: {
                    DWORD x1 = [keyCtx characterAtIndex:i+2]-1;
                    DWORD x2 = [keyCtx characterAtIndex:i+3]-1;
                    DWORD x3 = [keyCtx characterAtIndex:i+4]-1;
                    KMCompStore *store1 = [self.kmx.store objectAtIndex:x1];
                    KMCompStore *store2 = [self.kmx.store objectAtIndex:x3];

                    BOOL match = [store1.string isEqualToString:store2.string];
                    if ((match && x2 == EQUAL) || (!match && x2 == NOT_EQUAL)) {
                        cx+=1000;
                    }
                    else {
                        return 0;
                    }

                    sx++;
                    i+=5;
                    break;
                }
                case CODE_IFSYSTEMSTORE: {
                    DWORD x1 = [keyCtx characterAtIndex:i+2]-1;
                    DWORD x2 = [keyCtx characterAtIndex:i+3]-1;
                    DWORD x3 = [keyCtx characterAtIndex:i+4]-1;
                    KMCompStore *store = [self.kmx.store objectAtIndex:x3];
                    if (x1 == TSS_PLATFORM) {
                        BOOL platformMatches = [self checkPlatform:store.string];
                        if ((platformMatches && (x2 == EQUAL)) ||
                            (!platformMatches && (x2 == NOT_EQUAL))) {
                            checkPlatform = YES;
                        }
                        else {
                            return 0;
                        }
                    }
                    else {
                        return 0; // CODE_IFSYSTEMSTORE is not supported except TSS_PLATFORM
                    }

                    sx++;
                    i+=5;
                    break;
                }
                case CODE_NUL:
                    if (i == 0)
                        checkNul = YES;

                    i+=2;
                    break;
                default:
                    return 0;
            }
        }
        else {
            if ([ctx characterAtIndex:px++] != c)
                return 0;

            sx++;
            i++;
        }
    }

    if (checkNul) {
        if (![self.tmpCtxBuf isEqualToString:ctx])
            return 0;
        else
            cx+=1000;
    }

    if (checkPlatform)
        cx+=1000;

    return cx;
}

- (NSString *)getCharsFromKeyCode:(UInt16)keyCode {
    TISInputSourceRef currentKeyboard = TISCopyCurrentKeyboardLayoutInputSource();
    CFDataRef uchr = (CFDataRef)TISGetInputSourceProperty(currentKeyboard, kTISPropertyUnicodeKeyLayoutData);
    const UCKeyboardLayout *keyboardLayout = (const UCKeyboardLayout*)CFDataGetBytePtr(uchr);

    if (keyboardLayout) {
        UInt32 deadKeyState = 0;
        UniCharCount maxStringLength = 255;
        UniCharCount actualStringLength = 0;
        UniChar unicodeString[maxStringLength];

        OSStatus status = UCKeyTranslate(keyboardLayout,
                                         keyCode, kUCKeyActionDown, 0,
                                         LMGetKbdType(), 0,
                                         &deadKeyState,
                                         maxStringLength,
                                         &actualStringLength, unicodeString);

        if (actualStringLength == 0 && deadKeyState) {
            status = UCKeyTranslate(keyboardLayout,
                                    kVK_Space, kUCKeyActionDown, 0,
                                    LMGetKbdType(), 0,
                                    &deadKeyState,
                                    maxStringLength,
                                    &actualStringLength, unicodeString);
        }

        if (actualStringLength > 0 && status == noErr)
            return [NSString stringWithCharacters:unicodeString length:(NSUInteger)actualStringLength];
    }

    return nil;
}

- (BOOL)checkPlatform:(NSString *)platform {
    NSRange wholeString = NSMakeRange(0, [platform length]);
    NSRange firstMatchRange = [[KMEngine regexPlatform] rangeOfFirstMatchInString:platform options:NSMatchingAnchored range:wholeString];
    return (firstMatchRange.length == wholeString.length);
}

// Creates a VK map to convert Mac VK codes to Windows VK codes
- (void)setVKMapping {
    VKMap[MVK_A] = VK_KEY_A;                        // A
    VKMap[MVK_S] = VK_KEY_S;                        // S
    VKMap[MVK_D] = VK_KEY_D;                        // D
    VKMap[MVK_F] = VK_KEY_F;                        // F
    VKMap[MVK_H] = VK_KEY_H;                        // H
    VKMap[MVK_G] = VK_KEY_G;                        // G
    VKMap[MVK_Z] = VK_KEY_Z;                        // Z
    VKMap[MVK_X] = VK_KEY_X;                        // X
    VKMap[MVK_C] = VK_KEY_C;                        // C
    VKMap[MVK_V] = VK_KEY_V;                        // V
    //    0x0A  = nil
    VKMap[MVK_B] = VK_KEY_B;                        // B
    VKMap[MVK_Q] = VK_KEY_Q;                        // Q
    VKMap[MVK_W] = VK_KEY_W;                        // W
    VKMap[MVK_E] = VK_KEY_E;                        // E
    VKMap[MVK_R] = VK_KEY_R;                        // R
    VKMap[MVK_Y] = VK_KEY_Y;                        // Y
    VKMap[MVK_T] = VK_KEY_T;                        // T
    VKMap[MVK_1] = VK_KEY_1;                        // 1
    VKMap[MVK_2] = VK_KEY_2;                        // 2
    VKMap[MVK_3] = VK_KEY_3;                        // 3
    VKMap[MVK_4] = VK_KEY_4;                        // 4
    VKMap[MVK_6] = VK_KEY_6;                        // 6
    VKMap[MVK_5] = VK_KEY_5;                        // 5
    VKMap[MVK_EQUAL] = VK_EQUAL;                    // =
    VKMap[MVK_9] = VK_KEY_9;                        // 9
    VKMap[MVK_7] = VK_KEY_7;                        // 7
    VKMap[MVK_MINUS] = VK_MINUS;                    // -
    VKMap[MVK_8] = VK_KEY_8;                        // 8
    VKMap[MVK_0] = VK_KEY_0;                        // 0
    VKMap[MVK_RIGHT_BRACKET] = VK_RIGHT_BRACKET;    // ]
    VKMap[MVK_O] = VK_KEY_O;                        // O
    VKMap[MVK_U] = VK_KEY_U;                        // U
    VKMap[MVK_LEFT_BRACKET] = VK_LEFT_BRACKET;      // [
    VKMap[MVK_I] = VK_KEY_I;                        // I
    VKMap[MVK_P] = VK_KEY_P;                        // P
    VKMap[MVK_ENTER] = VK_ENTER;                    // <Enter>
    VKMap[MVK_L] = VK_KEY_L;                        // L
    VKMap[MVK_J] = VK_KEY_J;                        // J
    VKMap[MVK_QUOTE] = VK_QUOTE;                    // '
    VKMap[MVK_K] = VK_KEY_K;                        // K
    VKMap[MVK_SEMICOLON] = VK_SEMICOLON;            // ;
    VKMap[MVK_BACKSLASH] = VK_BACKSLASH;            // '\'
    VKMap[MVK_COMMA] = VK_COMMA;                    // ,
    VKMap[MVK_SLASH] = VK_SLASH;                    // '/'
    VKMap[MVK_N] = VK_KEY_N;                        // N
    VKMap[MVK_M] = VK_KEY_M;                        // M
    VKMap[MVK_PERIOD] = VK_PERIOD;                  // .
    VKMap[MVK_TAB] = VK_TAB;                        // <Tab>
    VKMap[MVK_SPACE] = VK_SPACE;                    // <Space>
    VKMap[MVK_GRAVE] = VK_GRAVE;                    // `
    VKMap[MVK_BACKSPACE] = VK_BACKSPACE;            // <Backspace>
    VKMap[MVK_OEM102] = VK_OEM_102;                 // 102nd key (ISO keyboards)

    // Num Pad
    VKMap[0x41] = VK_NUMPAD_DECIMAL;                // Decimal .
    VKMap[0x43] = VK_NUMPAD_MULTIPLY;               // Multiply *
    VKMap[0x45] = VK_NUMPAD_ADD;                    // Add +
    VKMap[0x47] = 0x00;                             // <Clear>      *** Undefined ***
    VKMap[0x4B] = VK_NUMPAD_DIVIDE;                 // Divide /
    VKMap[0x4C] = 0x00;                             // <Enter>      *** Undefined ***
    VKMap[0x4E] = VK_NUMPAD_SUBTRACT;               // Subtract -
    VKMap[0x51] = 0x00;                             // Equal =      *** Undefined ***
    VKMap[0x52] = VK_NUMPAD0;                       // 0
    VKMap[0x53] = VK_NUMPAD1;                       // 1
    VKMap[0x54] = VK_NUMPAD2;                       // 2
    VKMap[0x55] = VK_NUMPAD3;                       // 3
    VKMap[0x56] = VK_NUMPAD4;                       // 4
    VKMap[0x57] = VK_NUMPAD5;                       // 5
    VKMap[0x58] = VK_NUMPAD6;                       // 6
    VKMap[0x59] = VK_NUMPAD7;                       // 7
    VKMap[0x5B] = VK_NUMPAD8;                       // 8
    VKMap[0x5C] = VK_NUMPAD9;                       // 9
}

// Not used at the moment
/*
- (int)incIndex:(NSString *)c index:(int)index {
    int x = index;
    if ([c characterAtIndex:x] == 0) return x;
    if ([c characterAtIndex:x] != UC_SENTINEL) {
        if([c characterAtIndex:x] >= 0xD800 && [c characterAtIndex:x] <= 0xDBFF
           && [c characterAtIndex:x+1] >= 0xDC00 && [c characterAtIndex:x+1] <= 0xDFFF) return x+2;
        return x+1;
    }

    x+=2;
    switch([c characterAtIndex:x-1])
    {
        case CODE_ANY:              return x+1;
        case CODE_NOTANY:           return x+1;
        case CODE_INDEX:            return x+2;
        case CODE_USE:              return x+1;
        case CODE_DEADKEY:          return x+1;
        case CODE_EXTENDED:     x += 2; while([c characterAtIndex:x] != UC_SENTINEL_EXTENDEDEND) x++;           return x+1;
        case CODE_CLEARCONTEXT:     return x+1;
        case CODE_CALL:             return x+1;
        case CODE_CONTEXTEX:        return x+1;
        case CODE_IFOPT:            return x+3;
        case CODE_IFSYSTEMSTORE:    return x+3;
        case CODE_SETOPT:           return x+2;
        case CODE_SETSYSTEMSTORE:   return x+2;
        case CODE_RESETOPT:         return x+1;
        case CODE_SAVEOPT:          return x+1;
        default:                    return x;
    }
    return 0;
}
*/

@end
