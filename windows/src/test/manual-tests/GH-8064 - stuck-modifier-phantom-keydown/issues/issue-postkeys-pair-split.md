# Issue 4 — `AIWin2000Unicode::PostKeys` can split a KEYDOWN/KEYUP pair under queue truncation (row `8`)

Draft, not filed. Producer row `8` in
[`../MODIFIER-PRODUCERS.md`](../MODIFIER-PRODUCERS.md). Filing is an OPTIONAL
step a maintainer may take; no PR is opened by the change that wrote this draft, so nothing here is
waiting on one. See [README.md](./README.md).

---

**Title:** `PostKeys`'s modifier KEYDOWN/KEYUP pair can be split by three unguarded truncation points

**Body:**

`QIT_VKEYDOWN` (`windows/src/engine/keyman32/appint/aiWin2000Unicode.cpp:138-153`)
writes a synthesized KEYDOWN for a VK carried in `Queue[n].dwData & 0xFF`. Its
matching release only exists if a separate `QIT_VKEYUP` action follows in the
same queue. The one production producer of this pair (`kmprocess.cpp:181-182`)
queues both together, but three separate points can silently drop the second half
without either queuing the pair atomically or reporting the drop:

- `QueueAction` returns `FALSE` at `MAXACTIONQUEUE` and the caller ignores the
  result.
- `SignalServer` silently clamps the outgoing count to 256
  (`serialkeyeventclient.cpp:87-90`).
- The output-key copy in `PrepareInjectedInputBatch` stops short of
  `MAX_KEYEVENT_INPUTS` to reserve room for the modifier restore half, with
  nothing preventing a `QIT_VKEYDOWN`/`QIT_VKEYUP` pair from straddling that
  boundary.

**`PostKeys` itself is on the hot path — 245 calls in a single five-iteration
probe run.** What is narrow is the *split*, not reaching the function. Please do
not read "contrived" below as "hardly ever runs".

**The split's reachability is narrow but not zero.** `aiTIP.cpp:189-206` returns
early for `VK_MENU` and `VK_CONTROL` before the VK is assigned, but **`VK_SHIFT`
falls through** — so in practice this can only emit `VK_SHIFT`, which maps to Left
Shift and is releasable on every keyboard by a physical keypress, unlike the
chiral Right-side cases elsewhere in this document. That is why this row is
`UNMITIGATED (contrived)` rather than a top field-severity concern, and why no
runtime observation has confirmed it: it requires a legacy/ANSI target,
`use(final)`, 248 or more output events in one batch, and a rule that emits
`VK_SHIFT` specifically — source alone cannot establish these are co-reachable.

**Ask:** either guard the three truncation points so a split pair cannot happen
(e.g. reject or flush atomically rather than silently clamping), or run the
runtime observation described in `MODIFIER-PRODUCERS.md` Finding 3 (`debug=1`, a
keyboard whose Shift rule outputs 250+ characters, watch for
`"Too many INPUT events for queue"` immediately followed by an unmatched
`VK_SHIFT` KEYDOWN) to establish real-world reachability before prioritising a
fix.
