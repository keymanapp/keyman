# Issue — OSK sticky modifier stranded machine-wide by a keyman.exe crash or kill

Draft, not filed. Filing is an OPTIONAL step a maintainer may take; the PR that
carries this draft does not open it, so nothing here is waiting on one. See
[../README.md](../README.md), *Not covered here*.

**This one is not fixed, on any build.** It is the single producer in this area
that the #8064 OSK change leaves UNMITIGATED.

---

**Title:** OSK sticky modifier can be stranded machine-wide by a keyman.exe crash or kill, with no watchdog or restore-on-start

**Body:**

A modifier "clicked sticky" on the on-screen keyboard is deliberately held via a
real, chiral `keybd_event` KEYDOWN with no matching KEYUP queued anywhere —
release only happens when Keyman itself runs `ResetShiftStates` (OSK dismissal,
tab switch, or normal process shutdown).

If keyman.exe is terminated abnormally — Task Manager "End task",
`TerminateProcess`, or the Sentry crash handler, which sets `sceaTerminate`
specifically so destructors don't run — that release never happens. There is no
persisted record of the outstanding modifier across the process boundary, and no
watchdog checks or reconciles global modifier state on the next launch.

**User impact:** a modifier (potentially Right Ctrl/Alt, unclearable on hardware
without that physical key) stays asserted machine-wide until the user presses the
same physical key themselves, successfully reopens the OSK and dismisses it, or
reboots. Note that reopening the OSK does not by itself repopulate
`FCachedShiftState` for a modifier stranded by a previous process, so that route
only works if the user re-clicks the same modifier first.

**Explicitly rejected mitigation:** releasing all managed modifiers
unconditionally at Keyman startup, since that would release a modifier the user
is genuinely, physically holding down at the moment Keyman launches — a new bug
in the same family.

**Scope for a fix:** either (a) a small persisted "outstanding sticky modifier"
record written when injected and cleared when released, checked once at startup
and reconciled only against live `GetAsyncKeyState` (never blindly), or (b) a
supervising watchdog process. Out of scope for the #8064 fix: nothing in-process
can run after `TerminateProcess`, and a persisted latch record with startup
reconciliation is a design change rather than a bug fix.
