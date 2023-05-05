/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KMCoreActionHandler.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-04-21.
 * 
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMCoreActionHandler : NSObject
@property int backspaceEventsToGenerate;
@property int backspacesToInsert;
@property NSString *textToInsert;
@property NSString *textEventsToGenerate;
-(instancetype)initWithActions:(NSArray*)actions event: (NSEvent *)event client:(id) client;
-(BOOL)handleActions;
@end

NS_ASSUME_NONNULL_END
