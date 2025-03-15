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
@property (readonly, copy) NSString *clientApplicationId;

-(instancetype)initWithClient:(nullable id) client applicationId:(nullable NSString *)appId;
-(BOOL)isMatchingClient:(nullable id) otherClient applicationId:(nullable NSString *)otherAppId;
-(NSString *)description;
-(void)checkCompliance;
-(void) checkComplianceAfterInsert:(NSString *)insertedText deleted:(NSString *)deletedText;
-(BOOL)isComplianceUncertain;
-(BOOL)canReadText;
-(BOOL)mustBackspaceUsingEvents;
@end

NS_ASSUME_NONNULL_END
