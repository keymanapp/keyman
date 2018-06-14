//
//  KMPackage.h
//  Keyman
//
//  Created by tom on 2/9/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef KMPackage_h
#define KMPackage_h

#import <Cocoa/Cocoa.h>
#import "KMInputMethodAppDelegate.h"

@interface KMPackage : NSDocument
-(NSString *) getOrigKmpFilename;
-(NSString *) getTempKmpFilename;

-(void) releaseTempKMPFile;
@end

#endif /* KMPackage_h */
