---
title: KMManager.getMaySendCrashReport()
---

## Summary
The **getMaySendCrashReport()** method returns whether Keyman Engine is allowed to send crash reports over the network to sentry.keyman.com.

## Syntax
```java
KMManager.getMaySendCrashReport()
```

### Returns
Returns `true` if crash reports can be sent over the network to sentry.keyman.com, `false` otherwise.

## Description
Use this method to check if Keyman Engine will be accessing the network to send crash reports.

## History
Added syntax in Keyman Engine for Android 14.0.

## See also
* [setMaySendCrashReport](setMaySendCrashReport)
