/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreKeyOutput.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-10-31.
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

typedef enum {
  Unchanged,
  Off,
  On
} CapsLockState;

@interface CoreKeyOutput : NSObject
@property (nonatomic, readonly) NSUInteger codePointsToDeleteBeforeInsert;
@property (strong, nonatomic, readonly) NSString *textToInsert;
@property (strong, nonatomic, readonly) NSString *textToDelete;
@property (strong, nonatomic, readonly) NSDictionary *optionsToPersist;
@property (nonatomic, readonly) BOOL alert;
@property (nonatomic, readonly) BOOL emitKeystroke;
@property (nonatomic, readonly) CapsLockState capsLockState;
-(instancetype)init:(NSUInteger)codePointsToDelete textToDelete:(NSString*)deletedText textToInsert:(NSString*)text optionsToPersist:(NSDictionary*)options alert:(BOOL)alert emitKeystroke:(BOOL)emit capsLockState:(CapsLockState)capsLock;
-(NSString *)description;
-(BOOL)hasCodePointsToDelete;
-(BOOL)hasTextToInsert;
-(BOOL)isDeleteOnlyScenario;
-(BOOL)isInsertOnlyScenario;
-(BOOL)isDeleteAndInsertScenario;
@end

NS_ASSUME_NONNULL_END
