//
//  OSKKey.h
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 1/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface OSKKey : NSObject

@property (nonatomic, strong) NSString *caption;
@property (nonatomic, assign) NSUInteger keyCode;
@property (nonatomic, assign) CGFloat scale;

- (id)initWithKeyCode:(NSUInteger)keyCode caption:(NSString *)caption scale:(CGFloat)scale;

@end
