# Keyman Developer Onboarding

Welcome. This folder is the entry point for newcomers to the Keyman
ecosystem. There are two distinct audiences here:

* **People building tools *around* Keyman from external repos** — AI
  extensions for keyboard authoring, submission-triage bots for
  [keymanapp/keyboards](https://github.com/keymanapp/keyboards),
  validation tools, instrumented test rigs. You're consuming Keyman's
  formats and CLI/library surface; you're not modifying Keyman itself.
  Start at [keyboard-anatomy.md](keyboard-anatomy.md) and
  [external-tooling.md](external-tooling.md).

* **People contributing to Keyman itself** — runtime engines, the
  developer tooling, KeymanWeb, the Delphi-removal effort. You'll
  modify code in this repo. Start at [repository-map.md](repository-map.md)
  and [migration-guide.md](migration-guide.md).

The canonical user-facing documentation is at
[help.keyman.com](https://help.keyman.com). The canonical build
instructions are under [docs/build/](../docs/build/). This `dev-docs/`
folder is *only* for the "where is everything, what shape is it, and
why" questions.

## The docs

### For external tool builders

> **Strategic frame, read this first.** The CLDR LDML keyboard format
> is the future-canonical Keyman keyboard format. The spec is complete
> for both desktop and touch keyboards. Keyman's *read* side for LDML
> touch isn't finished yet — runtime still drives off the legacy
> multi-file setup — but LDML is forward-compatible, so the right move
> for a generation tool is to emit LDML XML as the canonical source and
> only produce the transitional `.kmn`+`.kvks`+`.keyman-touch-layout`
> bundle when the deliverable has to run on touch *today*. See
> [external-tooling.md § The strategic frame](external-tooling.md#the-strategic-frame-for-tools-generating-keyboards)
> and [migration-guide.md § LDML status](migration-guide.md#status--what-works-today-vs-whats-still-in-flight)
> for the full picture.

1. **[keyboard-anatomy.md](keyboard-anatomy.md)** — Reference for what
   files make up a Keyman keyboard project (`.kpj`, `.kmn`, LDML XML,
   `.kvks`, `.keyboard_info`, `.kps`, `.kmp`, ...), what each does, and
   where the JSON schemas live for validation.

2. **[external-tooling.md](external-tooling.md)** — How to drive the
   Keyman compiler and engine programmatically: the `kmc` CLI, the
   `@keymanapp/kmc-*` npm package surface, schema-based validation,
   the cross-file consistency contract that any generator must enforce
   today, triage criteria for keyboard submissions, and the approach
   for instrumented forks of the compiler or engine.

### For Keyman contributors

3. **[repository-map.md](repository-map.md)** — A guided tour of the
   top-level directory structure, what each major component does, and
   how they connect. Includes Mermaid diagrams of the component graph,
   build dependencies, and the cross-platform matrix. External tool
   builders should read this too if they want to understand what their
   tool is hooking *into*.

4. **[migration-guide.md](migration-guide.md)** — Keyman has been
   actively modernizing for years, replacing Delphi components with
   C++ (Keyman Core), TypeScript (the kmc compiler family and
   KeymanWeb), and embracing LDML as the canonical keyboard format.
   This guide explains what has already moved, what's still in flight,
   and where the long-running migration branches live.

## Then look at the build docs

Once you've internalized the layout, the standard build instructions live at:

* [docs/build/windows.md](../docs/build/windows.md) — the canonical Windows
  flow (Delphi 10.3 / Pro)
* [docs/build/windows-d12.md](../docs/build/windows-d12.md) — fallback for
  contributors stuck on Delphi 12 Community Edition (the only free Delphi
  tier today); covers IDE-based workflow plus helper scripts
* `linux/`, `mac/`, `android/`, `ios/` — platform-specific READMEs in each
  top-level directory

## Help / Getting Stuck

* [SIL/Keyman Community Forum](https://community.software.sil.org/c/keyman)
* [Keyman roadmap blog](https://blog.keyman.com/category/roadmap/) — the
  long-form context for the migrations described in `migration-guide.md`
* [Issue tracker](https://github.com/keymanapp/keyman/issues) — search for
  the component or migration branch you're touching before filing new issues
* [CONTRIBUTING.md](../CONTRIBUTING.md) (at repo root) for PR conventions
