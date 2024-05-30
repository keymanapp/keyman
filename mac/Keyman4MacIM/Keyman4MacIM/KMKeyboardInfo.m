/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMKeyboardInfo.m
 * Keyman
 *
 * Created by Shawn Schantz on 2021/11/25.
 *
 * Value object describing a keyboard element of a Keyman package.
 */

#import "KMKeyboardInfo.h"

@implementation KMKeyboardInfoBuilder
- (instancetype)init {
  self = [super init];
  if (self) {
    _name = nil;
    _identifier = nil;
    _version = nil;
    _oskFont = nil;
    _displayFont = nil;
    _languages = nil;
  }
  return self;
}
@end


@implementation KMLanguageInfo

- (instancetype)initWithName:(NSString*)name
                  identifier:(NSString*)identifier
{
  self = [super init];
  if (self) {
    _name = [name copy];
    _identifier = [identifier copy];
  }
  return self;
}

@end

@implementation KMKeyboardInfo

- (instancetype)initWithBuilder:(KMKeyboardInfoBuilder *)builder {
  self = [super init];
  if (self) {
    _name = builder.name;
    _identifier = builder.identifier;
    _version = builder.version;
    _oskFont = builder.oskFont;
    _displayFont = builder.displayFont;
    _languages = builder.languages;
  }
  return self;
}

- (instancetype)initWithName:(NSString*)name
                  identifier:(NSString*)identifier
                     version:(NSString*)version
                     oskFont:(NSString*)oskFont
                 displayFont:(NSString*)displayFont
                   languages:(NSArray*)languages
{
  self = [super init];
  if (self) {
    _name = [name copy];
    _identifier = [identifier copy];
    _version = [version copy];
    _oskFont = [oskFont copy];
    _displayFont = [displayFont copy];
    _languages = [languages copy];
  }
  return self;
}

@end
