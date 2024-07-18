/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * TextApiCompliance.h
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-05-05.
 * 
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface TextApiCompliance : NSObject
@property (readonly) NSString *clientApplicationId;

-(instancetype)initWithClient:(id) client applicationId:(NSString *)appId;
-(void)checkCompliance:(id) client;
-(void) checkComplianceAfterInsert:(id) client delete:(NSString *)textToDelete insert:(NSString *)textToInsert;
-(BOOL)isComplianceUncertain;
-(BOOL)canReadText;
-(BOOL)mustBackspaceUsingEvents;
@end

NS_ASSUME_NONNULL_END
