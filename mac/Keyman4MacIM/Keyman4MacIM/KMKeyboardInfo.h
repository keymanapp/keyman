//
//  KMKeyboardInfo.h
//  Keyman
//
//  Created by Shawn Schantz on 11/25/21.
//  Copyright © 2021 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMLanguageInfo : NSObject
@property (nonatomic,readonly) NSString* name;
@property (nonatomic,readonly) NSString* identifier;

- (instancetype)initWithName:(NSString*)name
                  identifier:(NSString*)identifier;
@end

@interface KMKeyboardInfo : NSObject
@property (nonatomic,readonly) NSString* name;
@property (nonatomic,readonly) NSString* identifier;
@property (nonatomic,readonly) NSString* oskFont;
@property (nonatomic,readonly) NSString* displayFont;
@property (nonatomic,readonly) NSArray* languages;

- (instancetype)initWithName:(NSString*)name
              identifier:(NSString*)identifier
                      oskFont:(NSString*)oskFont
                 displayFont:(NSString*)displayFont
                   languages:(NSArray*)languages;

@end

NS_ASSUME_NONNULL_END
