# LLM Usage Policy
This is a moderation policy for how LLMs are used in `keymanapp` repositories.

For additional information about the policy itself, see [the appendix](#appendix).

## Overview
Using LLMs while working on `keymanapp` repositories is conditionally allowed, when done with care.
LLMs are not a substitute for thought,
and we do not allow them to be used in ways that risk losing our shared social and technical understanding of the project,
nor in ways that hurt our goals of creating a strong community.

We are aware that many clauses in this policy are unenforceable.
Our goal is *not* to catch every violation (see "It's not your job to play detective", below).
Instead, our goal is to remove plausible deniability: to force a choice between following the policy and intentionally violating it.

The policy's guidelines are roughly as follows:

> It's fine to use LLMs to answer questions, analyze, distill, refine, check, suggest, review. But not to **create**.

> LLMs work best when used as a tool to write *better*, not *faster*.

## Rules
### Legend
- ✅ Allowed.
- ❌ Banned.
- ⚠️ Allowed with caveats. Must disclose that an LLM was used.
- ℹ️ Adds additional detail to the policy. These bullets are normative.
- 🔨 Violating this clause counts as a violation of the Code of Conduct.

### Summary
- ✅ Allowed: Private use.
- ❌ Banned: LLM-created comments, docs, or diagnostics. Replacing human judgement with LLM judgement. Requiring people to use an LLM to contribute.
- ⚠️ Conditionally allowed: Trivial changes, machine translation, LLM reviews and review bots, LLM-created code *under the experiment rules*.
- 🔨 Carries a moderation penalty: Lying.

### Non-exhaustive policy

This policy does not aim to be exhaustive.
If you have a use of LLMs in mind that isn't on this list, talk to people about it, and judge it in the spirit of this overview:
- Using an LLM for your own personal use is likely allowed ✅
- Showing LLM output to another human without solicitation is likely banned ❌
- Making a decision that affects others based on LLM output requires disclosure ⚠️

### ✅ Allowed
The following are allowed.
- Any use of an LLM where you are the only one who sees the output. For example:
  - Asking an LLM questions about an existing codebase.
  - Asking an LLM to summarize comments on an issue or PR.
      - ℹ️ This does not allow reposting the summary publicly. This only includes your own personal use.
  - Asking an LLM to privately review your code or prose.
      - ℹ️ This does not apply to public comments by the LLM. See "review bots" under ⚠️ below.
  - Writing dev-tools for your own personal use using an LLM.
  - Using an LLM to generate possible solutions to an issue, learning from them, and then writing something from scratch in your own style.

### ❌ Banned
The following are banned.
- Comments from a personal user account that are originally created by an LLM.
    - ℹ️ This also applies to issue bodies and PR descriptions.
    - ℹ️ This also applies to voice/video content originally created or scripted by an LLM.
    - ℹ️ This does not apply if the LLM content is clearly quoted and marked; you can post that.
         However, the content of the comment must stand on its own even without the LLM content; it's not a substitute for your own words.
    - ℹ️ See also "machine-translation" in ⚠️ below.
    - ℹ️ See also "Scope" in the appendix below.
- Documentation that is originally created by an LLM.
    - ℹ️ This includes non-trivial source comments, such as doc-comments, safety comments, or multiple paragraphs of non-doc-comments.
    - ℹ️ This includes compiler diagnostics.
         LLMs are conditionally allowed to assist with the *logic* surrounding a diagnostic (see "Experiment: LLM-created code changes" below),
         but they must not be used to create the message itself.
    - ℹ️ This does not include "trivial" changes (see ⚠️ below).
- Policies or processes that are written such that an LLM is required to execute them.
    - Documentation must be authored for humans primarily, and LLM documentation may only summarize it, not add new detail.
- Treating an LLM review as a sufficient condition to merge or reject a change. (see ⚠️ below).

### ⚠️ Allowed with caveats
These uses are allowed on a case-by-case basis, under the rules below.
If you are a new contributor, you should expect to be scrutinized more heavily than existing contributors,
since you haven't yet established trust with your reviewers.

All uses under "⚠️ Allowed with caveats" **must** disclose that an LLM was used.

- Using machine-translation (e.g. Google Translate) from your native language without posting your original message is allowed but discouraged.
  Doing so can introduce new miscommunications that weren't there originally, and prevents someone who speaks the language from providing a better translation.
    - ℹ️ Posting both your original message and the translated version is always ok, but you must still disclose that machine-translation was used.
- "Trivial" code or prose changes.
    - ℹ️ Changes are trivial if there is no other way to write them, or the other ways to write them are nearly identical. For example, the following are all trivial:
        - Typo fixes
        - Markdown links
        - Changing a word to a synonym
    - ℹ️ Be cautious about PRs that consist solely of trivial changes.
- Using an LLM to discover bugs, as long as you personally verify the bug.
    - ℹ️ This also includes reviewers who use LLMs to discover flaws in unmerged code.
    - ℹ️ See also "Comments from a personal user account" under ❌ above.
- LLM reviews, if enabled, **must** be advisory-only, and should be performed _after_ a human review.
    - ℹ️ An LLM review does not substitute for self-review. Authors are expected to review their own code before posting and after each change.

## Appendix

### Source

This policy has been adapted from a [draft policy on rust-lang/rust](https://github.com/rust-lang/rust-forge/pull/1040).
It was copied and adapted on 14 July 2026.

### Scope

This policy applies to repositories in the `keymanapp` org.

### Motivation and guiding principles

There is not a consensus within the Keyman project—and likely never will be—about when/how/where it is acceptable to use AI-based tools.
Many members of the Keyman project and community find value in AI;
many others feel that its negative impact on society and the climate are severe enough that no use is acceptable.
Still others are working out their opinion.

Despite these differences, there are many values we all share:

- Building a community of deep experts in our collective projects.
- Building an inclusive community where all feel welcome and respected.

And many facts we agree on:

- Many people find LLM-generated code and writing deeply unpleasant to read or review.
- Many people find LLMs to be a significant aid to learning and discovery.
- LLMs are a new technology, and we are still learning how to use, moderate, and improve them.

With those facts and values in mind, the policy is designed with the following goals:

- Build an intentionally conservative policy that lets us maintain the standard of quality that Keyman is known for.
- Limit LLM contributions to the very highest standard of quality, to show that our guideline of "better, not faster" isn't just words.
- Make the policy enforceable and easy to moderate.
- Make the policy consistent and easy to understand and summarize, even for people who haven't read it in detail.
- Avoid making LLMs a requirement to contribute, to be inclusive of contributors who choose not to use them.

### Moderation policy
#### It's not your job to play detective
["The optimal amount of fraud is not zero"](https://www.bitsaboutmoney.com/archive/optimal-amount-of-fraud/).
Don't try to be the police for whether someone has used an LLM.
You are not required to "actively look" for whether an LLM was involved.

Style is not evidence, and English-as-a-second-language speakers, neurodivergent people, and over-explainers are the most likely to be accused of writing like an LLM.

If it's clear someone's broken the rules, point them to this policy;
otherwise, discuss with team leads on #internal on Slack.

#### Be honest
Conversely, you're expected to be honest about your use of LLMs and the extent of that use.
If you are not sure where something you would like to do falls in this policy, please ask on the [Community Site](https://community.software.sil.org/c/keyman).
Don't try to hide it.

Deliberately misrepresenting your use of LLMs is not welcome and may result in moderation action.

#### Penalties
The policies marked with a 🔨 follow the same guidelines as the code of conduct:
Violations will first result in a warning, and repeated violations may result in a ban.
- 🔨 Violations of the "Be honest" section

Other violations are left up to the discretion of reviewers and moderators.
For minor violations we recommend telling the author that we can't review the PR until it complies with the policy, with pointers to exactly what they need to do.
For major violations or extractive PRs, we recommend closing the PR or issue.

It is **not** ok to harass a contributor for using an LLM.
All contributors must be treated with respect.
The Code of Conduct applies to *all* conversations in the Keyman project.

### Responsibility

Your contributions are your responsibility; you cannot place any blame on an LLM.
- ℹ️ This includes when asking people to address review comments originally created by an LLM. See "review bots" under ⚠️ above.

### The meaning of "originally created by an LLM"

This document uses the phrase "originally created by an LLM" to mean "text that was generated by an LLM (and then possibly edited by a human)".
No amount of editing can change how it was originally created;
the origin sets the initial style, and that style is very hard to change once it's set.

For more background about analogous reasoning, see ["What Colour are your bits?"](https://ansuz.sooke.bc.ca/entry/23).

This policy makes no distinction between LLM output that comes from a chat interface and output that comes from editor auto-completion.
In most cases the output is "trivial" (see above under ⚠️), but regardless, it is not treated specially by this policy.

### Conditions for modification or dissolution
This policy is not set in stone, and we can evolve it as we gain more experience working with LLMs.
