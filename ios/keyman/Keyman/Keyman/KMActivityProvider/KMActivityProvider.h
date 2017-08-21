//
//  KMActivityProvider.h
//  Keyman for iPhone and iPad
//
//  Created by Serkan Kurt on 14/11/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface KMActivityProvider : UIActivityItemProvider <UIActivityItemSource> {
    NSString *text;
    UIFont *font;
}

@property (strong, nonatomic) NSString *text;
@property (strong, nonatomic) UIFont *font;

@end

@interface KMActivity : UIActivity

@end
