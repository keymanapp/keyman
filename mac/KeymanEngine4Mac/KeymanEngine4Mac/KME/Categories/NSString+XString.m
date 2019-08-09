//
//  NSString+XString.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 8/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "NSString+XString.h"

@implementation NSString (XString)

// For calculating offset including system stores
- (NSUInteger)xLengthIncludingIf {
  NSUInteger cx = 0;
  for (int i = 0; i < self.length;) {
    unichar c = [self characterAtIndex:i];
    if (c == UC_SENTINEL) {
      c = [self characterAtIndex:i+1];
      switch (c) {
        case CODE_ANY:
        case CODE_NOTANY:
        case CODE_CONTEXTEX:
        case CODE_DEADKEY:
        case CODE_NULLCHAR:
          cx++;
          i+=3;
          break;
        case CODE_INDEX:
          cx++;
          i+=4;
          break;
        case CODE_IFOPT:
        case CODE_IFSYSTEMSTORE:
          cx++;
          i+=5;
          break;
        case CODE_NUL:
        default:
          i+=2;
          break;
      }
    }
    else {
      cx++;
      i++;
    }
  }

  return cx;
}

- (NSUInteger)xLength {
    NSUInteger cx = 0;
    for (int i = 0; i < self.length;) {
        unichar c = [self characterAtIndex:i];
        if (c == UC_SENTINEL) {
            c = [self characterAtIndex:i+1];
            switch (c) {
                case CODE_ANY:
                case CODE_NOTANY:
                case CODE_CONTEXTEX:
                case CODE_DEADKEY:
                case CODE_NULLCHAR:
                    cx++;
                    i+=3;
                    break;
                case CODE_INDEX:
                    cx++;
                    i+=4;
                    break;
                case CODE_IFOPT:
                case CODE_IFSYSTEMSTORE:
                    i+=5;
                    break;
                case CODE_NUL:
                default:
                    i+=2;
                    break;
            }
        }
        else {
            cx++;
            i++;
        }
    }
    
    return cx;
}

- (NSUInteger)deadKeyCount {
    NSUInteger dk = 0;
    for (int i = 0; i < self.length;) {
        unichar c = [self characterAtIndex:i];
        if (c == UC_SENTINEL) {
            c = [self characterAtIndex:i+1];
            switch (c) {
                case CODE_DEADKEY:
                    dk++;
                    i+=3;
                    break;
                case CODE_NULLCHAR:
                case CODE_ANY:
                case CODE_NOTANY:
                case CODE_CONTEXTEX:
                    i+=3;
                    break;
                case CODE_INDEX:
                    i+=4;
                    break;
                case CODE_IFOPT:
                case CODE_IFSYSTEMSTORE:
                    i+=5;
                    break;
                case CODE_NUL:
                default:
                    i+=2;
                    break;
            }
        }
        else {
            i++;
        }
    }
    
    return dk;
}

- (NSString *)lastNChars:(NSUInteger)n {
    if (n == 0)
        return @"";
    
    if (self.length >= 3) {
        NSInteger len = 0;
        NSInteger chars = 0;
        for (NSInteger i = self.length - 1; i >= 0;) {
            NSInteger index = i - 2;
            if (index >= 0) {
                unichar c1 = [self characterAtIndex:index];
                unichar c2 = [self characterAtIndex:index+1];
                if (c1 == UC_SENTINEL && (c2 == CODE_DEADKEY || c2 == CODE_NULLCHAR)) {
                    i -= 3;
                    len += 3;
                }
                else {
                    i--;
                    len++;
                }
            }
            else {
                i--;
                len++;
            }
            
            chars++;
            if (chars == n)
                break;
        }
        
        return [NSString stringWithString:[self substringFromIndex:self.length - len]];
    }
    else {
        if (n >= self.length)
            return [NSString stringWithString:self];
        else
            return [NSString stringWithString:[self substringFromIndex:self.length - n]];
    }
}

- (NSString *)codeString {
    if (self == nil)
        return nil;
    
    NSMutableString *mStr = [NSMutableString stringWithString:@""];
    for (int i=0; i < self.length;) {
        unichar c = [self characterAtIndex:i];
        if (c == UC_SENTINEL) {
            if ((i+1) < self.length)
                c = [self characterAtIndex:i+1];
            else {
                [mStr appendString:@"sentinel(INVALID)"];
                return mStr;
            }

            switch (c) {
                case CODE_ANY: {
                    if ((i+2) < self.length) {
                        DWORD index = [self characterAtIndex:i+2];
                        [mStr appendString:[NSString stringWithFormat:@"any(%d)", index]];
                    }
                    else {
                        [mStr appendString:@"any(INVALID)"];
                    }
                    
                    i+=3;
                    break;
                }
                case CODE_NOTANY: {
                    if ((i+2) < self.length) {
                        DWORD index = [self characterAtIndex:i+2];
                        [mStr appendString:[NSString stringWithFormat:@"notany(%d)", index]];
                    }
                    else {
                        [mStr appendString:@"notany(INVALID)"];
                    }

                    i+=3;
                    break;
                }
                case CODE_INDEX: {
                    if ((i+3) < self.length) {
                        DWORD x1 = [self characterAtIndex:i+2];
                        DWORD x2 = [self characterAtIndex:i+3];
                        [mStr appendString:[NSString stringWithFormat:@"index(%d,%d)", x1, x2]];
                    }
                    else {
                        [mStr appendString:@"index(INVALID)"];
                    }
                    
                    i+=4;
                    break;
                }
                case CODE_NUL:
                    [mStr appendString:@"nul()"];
                    i+=2;
                    break;
                case CODE_DEADKEY: {
                    if ((i+2) < self.length) {
                        c = [self characterAtIndex:i+2];
                        [mStr appendString:[NSString stringWithFormat:@"dk(%d)", c]];
                    }
                    else {
                        [mStr appendString:@"dk(INVALID)"];
                    }

                    i+=3;
                    break;
                }
                case CODE_NULLCHAR: {
                    if ((i+2) < self.length) {
                        c = [self characterAtIndex:i+2];
                        [mStr appendString:[NSString stringWithFormat:@"nc(%d)", c]];
                    }
                    else {
                        [mStr appendString:@"nc(INVALID)"];
                    }

                    i+=3;
                    break;
                }
                case CODE_CONTEXT: {
                    [mStr appendString:[NSString stringWithFormat:@"ctx()"]];
                    i+=2;
                    break;
                }
                case CODE_CONTEXTEX: {
                    if ((i+2) < self.length) {
                        c = [self characterAtIndex:i+2];
                        [mStr appendString:[NSString stringWithFormat:@"ctxex(%d)", c]];
                    }
                    else {
                        [mStr appendString:@"ctxex(INVALID)"];
                    }
                    
                    i+=3;
                    break;
                }
                case CODE_USE: {
                    if ((i+2) < self.length) {
                        c = [self characterAtIndex:i+2];
                        [mStr appendString:[NSString stringWithFormat:@"use(%d)", c]];
                    }
                    else {
                        [mStr appendString:@"use(INVALID)"];
                    }
                    
                    i+=3;
                    break;
                }
                case CODE_BEEP: {
                    [mStr appendString:[NSString stringWithFormat:@"beep()"]];
                    i+=2;
                    break;
                }
                case CODE_RETURN: {
                    [mStr appendString:[NSString stringWithFormat:@"return()"]];
                    i+=2;
                    break;
                }
                case CODE_IFOPT: {
                    if ((i+4) < self.length) {
                        DWORD x1 = [self characterAtIndex:i+2];
                        DWORD x2 = [self characterAtIndex:i+3];
                        DWORD x3 = [self characterAtIndex:i+4];
                        [mStr appendString:[NSString stringWithFormat:@"ifopt(%d %d %d)", x1, x2, x3]];
                    }
                    else {
                        [mStr appendString:@"ifopt(INVALID)"];
                    }
                    
                    i+=5;
                    break;
                }
                case CODE_IFSYSTEMSTORE: {
                    if ((i+4) < self.length) {
                        DWORD x1 = [self characterAtIndex:i+2];
                        DWORD x2 = [self characterAtIndex:i+3];
                        DWORD x3 = [self characterAtIndex:i+4];
                        [mStr appendString:[NSString stringWithFormat:@"ifsys(%d %d %d)", x1, x2, x3]];
                    }
                    else {
                        [mStr appendString:@"ifsys(INVALID)"];
                    }
                    
                    i+=5;
                    break;
                }
                default:
                    [mStr appendString:@"other()"];
                    i+=2;
                    break;
            }
        }
        else {
            [mStr appendString:[NSString stringWithFormat:@"%C", c]];
            i++;
        }
    }
    
    return mStr;
}

- (BOOL)isValidCode {
    if (self == nil)
        return NO;
    
    for (int i=0; i < self.length;) {
        unichar c = [self characterAtIndex:i];
        if (c == UC_SENTINEL) {
            if ((i+1) < self.length)
                c = [self characterAtIndex:i+1];
            else
                return NO;
            
            switch (c) {
                case CODE_ANY:
                case CODE_NOTANY:
                case CODE_DEADKEY:
                case CODE_NULLCHAR:
                case CODE_CONTEXTEX:
                case CODE_USE: {
                    if ((i+2) >= self.length)
                        return NO;
                    
                    i+=3;
                    break;
                }
                case CODE_INDEX: {
                    if ((i+3) >= self.length)
                        return NO;
                    
                    i+=4;
                    break;
                }
                case CODE_IFOPT:
                case CODE_IFSYSTEMSTORE: {
                    if ((i+4) >= self.length)
                        return NO;
                    
                    i+=5;
                    break;
                }
                case CODE_NUL:
                case CODE_CONTEXT:
                case CODE_BEEP:
                case CODE_RETURN:
                default:
                    i+=2;
                    break;
            }
        }
        else {
            i++;
        }
    }
    
    return YES;
}

+ (NSString *)nullChar {
    return [NSString stringWithFormat:@"%C%C%d", UC_SENTINEL, CODE_NULLCHAR, 1];
}

@end

@implementation NSMutableString (XString)

- (void)appendDeadkey:(NSUInteger)index {
    NSString *dk = [NSString stringWithFormat:@"%C%C%C", UC_SENTINEL, CODE_DEADKEY, index];
    [self appendString:dk];
}

- (void)appendNullChar {
    [self appendString:[[self class] nullChar]];
}

- (NSUInteger)deleteLastDeadkeys {
    NSUInteger dkc = 0;
    if (self.length >= 3) {
        unichar c1 = [self characterAtIndex:self.length - 3];
        unichar c2 = [self characterAtIndex:self.length - 2];
        while (c1 == UC_SENTINEL && c2 == CODE_DEADKEY) {
            NSRange range = NSMakeRange(self.length - 3, 3);
            [self deleteCharactersInRange:range];
            dkc++;
            if (self.length < 3)
                break;
            c1 = [self characterAtIndex:self.length - 3];
            c2 = [self characterAtIndex:self.length - 2];
        }
    }
    
    return dkc;
}

- (NSUInteger)deleteLastNullChars {
    NSUInteger dsc = 0;
    if (self.length >= 3) {
        unichar c1 = [self characterAtIndex:self.length - 3];
        unichar c2 = [self characterAtIndex:self.length - 2];
        while (c1 == UC_SENTINEL && c2 == CODE_NULLCHAR) {
            NSRange range = NSMakeRange(self.length - 3, 3);
            [self deleteCharactersInRange:range];
            dsc++;
            if (self.length < 3)
                break;
            c1 = [self characterAtIndex:self.length - 3];
            c2 = [self characterAtIndex:self.length - 2];
        }
    }
    
    return dsc;
}

- (NSUInteger)removeAllNullChars {
    return [self replaceOccurrencesOfString:[[self class] nullChar] withString:@"" options:NSLiteralSearch range:NSMakeRange(0, self.length)];
}

- (void)deleteLastNChars:(NSUInteger)n {
    if (n > 0) {
        if (self.length >= 3) {
            NSInteger len = 0;
            NSInteger chars = 0;
            for (NSInteger i = self.length - 1; i >= 0;) {
                NSInteger index = i - 2;
                if (index >= 0) {
                    unichar c1 = [self characterAtIndex:index];
                    unichar c2 = [self characterAtIndex:index+1];
                    if (c1 == UC_SENTINEL && (c2 == CODE_DEADKEY || c2 == CODE_NULLCHAR)) {
                        i -= 3;
                        len += 3;
                    }
                    else {
                        i--;
                        len++;
                    }
                }
                else {
                    i--;
                    len++;
                }
                
                chars++;
                if (chars == n)
                    break;
            }
            
            [self deleteCharactersInRange:NSMakeRange(self.length - len, len)];
        }
        else {
            if (self.length >= n)
                [self deleteCharactersInRange:NSMakeRange(self.length - n, n)];
            else
                [self deleteCharactersInRange:NSMakeRange(0, self.length)];
        }
    }
}

@end
