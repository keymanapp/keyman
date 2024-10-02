//
//  KeyLabel.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 7/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyLabel.h"
#import "KeyLabelCell.h"

@implementation KeyLabel

- (id)initWithFrame:(NSRect)frameRect {
  [KeyLabel setCellClass:[KeyLabelCell class]];
  self = [super initWithFrame:frameRect];
  if (self) {
    //
  }
  
  return self;
}

@end
