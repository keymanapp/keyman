## engine/events

This submodule provides definitions for utilized by KMW event handling:

1. Our documented API events from KMW 16.0 and before have a different specification
   from that of EventEmitter, which started seeing use for internal events in KMW 12.0.
2. The OSK features UI elements that are displayed conditionally based on if there
   are event handlers for specific events; this needs special handling, even when
   basing the events on EventEmitter.