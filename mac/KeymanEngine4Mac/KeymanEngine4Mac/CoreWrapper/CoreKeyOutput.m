/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * CoreKeyOutput.m KeymanEngine4Mac
 *
 * Created by Shawn Schantz on 2023-10-31.
 *
 * A value object representing one or more actions returned by Keyman Core. A
 * struct describing the actions is returned by Core when processing a key down
 * event. These actions are then applied by the platform code to the text client
 * to produce the desired output corresponding to the user's keystroke.
 *
 */

#import "CoreKeyOutput.h"

@implementation CoreKeyOutput

-(instancetype)init:(NSUInteger)codePointsToDelete textToDelete:(NSString*)deletedText textToInsert:(NSString*)text optionsToPersist:(NSDictionary*)options alert:(BOOL)alert emitKeystroke:(BOOL)emit capsLockState:(CapsLockState)capsLock {
  self = [super init];
  if (self) {
    self->_codePointsToDeleteBeforeInsert = codePointsToDelete;
    self->_textToDelete = deletedText;
    self->_textToInsert = text;
    self->_optionsToPersist = options;
    self->_alert = alert;
    self->_emitKeystroke = emit;
    self->_capsLockState = capsLock;
  }
  return self;
}

-(NSString *)description
{
  NSData* deleteData = [self.textToDelete dataUsingEncoding:NSUTF16LittleEndianStringEncoding];
  NSData* insertData = [self.textToInsert dataUsingEncoding:NSUTF16LittleEndianStringEncoding];

  return [[NSString alloc] initWithFormat: @"codePointsToDeleteBeforeInsert: %li, textToDelete: '%@' [data: '%@'], textToInsert: '%@' [data: '%@], optionsToPersist: %@, alert: %d, emitKeystroke: %d, capsLockState: %d ", self.codePointsToDeleteBeforeInsert, self.textToDelete, deleteData, self.textToInsert, insertData, self.optionsToPersist, self.alert, self.emitKeystroke, self.capsLockState];
}

-(BOOL)hasCodePointsToDelete {
  return self.codePointsToDeleteBeforeInsert > 0;
}

-(BOOL)hasTextToInsert {
  return self.textToInsert.length > 0;
}

-(BOOL)isDeleteOnlyScenario {
  return (self.hasCodePointsToDelete && !self.hasTextToInsert);
}

-(BOOL)isInsertOnlyScenario {
  return (!self.hasCodePointsToDelete && self.hasTextToInsert);
}

-(BOOL)isDeleteAndInsertScenario {
  return (self.hasCodePointsToDelete && self.hasTextToInsert);
}


@end
