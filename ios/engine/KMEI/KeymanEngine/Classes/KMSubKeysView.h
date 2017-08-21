//
//  KMSubKeysView.h
//  KeymanEngine
//
//  Created by Serkan Kurt on 15/08/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface KMSubKeysView : UIView

@property (nonatomic, strong, readonly) UIView *containerView;

- (id)initWithKeyFrame:(CGRect)keyFrame subKeys:(NSArray *)subKeys;

@end
