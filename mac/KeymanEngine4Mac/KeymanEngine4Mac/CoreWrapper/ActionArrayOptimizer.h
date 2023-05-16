/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * ActionArrayOptimizer.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-02-23.
 * 
 */

#import <Foundation/Foundation.h>
#import "CoreAction.h"

NS_ASSUME_NONNULL_BEGIN

@interface ActionArrayOptimizer : NSObject
-(NSArray*)optimizeForLegacyArray:(NSArray*)actionArray;
-(NSArray*)optimize:(NSArray*)actionArray;
@end

NS_ASSUME_NONNULL_END
