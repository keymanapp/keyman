---
title: KMManager.setMaySendCrashReport()
---

## Summary
The **setMaySendCrashReport()** enables or disables whether Keyman Engine can send crash reports over the 
network to sentry.keyman.com.

## Syntax
```java
KMManager.setMaySendCrashReport(boolean value)
```

### Parameters
value

Set `true` to enable crash reports to be sent, `false` to disable.

## Description
Use this method to enable or disable whether Keyman Engine can send crash reports over the network. 
By default sending crash reports is enabled.

If you don't want crash reports to be sent, your app must disable this before `KMManager.initialize()`.

## Examples

### Example: Using setMaySendCrashReport()
The following script illustrates the use of `setMaySendCrashReport()`: 
```java
    // Disable crash reports from being sent
    KMManager.setMaySendCrashReport(false);
    // Initialize KMManager
    KMManager.initialize(getApplicationContext(), KeyboardType.KEYBOARD_TYPE_INAPP);
```

## History
Added syntax in Keyman Engine for Android 14.0.

## See also
* [getMaySendCrashReport](getMaySendCrashReport)
