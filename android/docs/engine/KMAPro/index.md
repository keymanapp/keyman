---
title: Keyman for Android APIs
---

## Summary

The **`Keyman for Android`** app has the following APIs for integration:


## Description

The Keyman for Android app installs as an Input Method Extension. When selected as the active system keyboard, it provides the following APIs:


## Intents

When the Keyman system keyboard changes the associated font name, the following Intent is broadcast so the integrating application can match the font:

```java
intent = new Intent("com.tavultesoft.kmapro.keyboard_changed");
intent.putExtra("fontName", fontName);
sendBroadcast(intent);
```
