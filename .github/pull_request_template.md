Stack: main ← #N ← **this** ← #M. Every PR targets `main`; this PR's own changes are its top K commits.
<!-- Keep the Stack line only for a stacked PR. Fill in the numbers with `gh pr edit` once the PRs exist, and update it in every PR of the chain when the chain changes. -->

**What**
<!-- 1 to 3 bullets: what the change does, as the owner ("I", "we"). -->

**Why**
<!-- 1 to 2 lines: the problem, with a repro for a fix. Name each conformance id that now passes, with the spec section; name them, don't count them. -->

**How it works**
<!-- When the PR changes OCaml: short snippets copied from the diff (git show), each with a sentence on what it does. -->

**OCaml worth noticing**
<!-- When the PR changes OCaml: each language feature or Stdlib function a learner may not know, and why it is used here. -->

<!-- Add **Behavior change** when an input that worked now fails, a message changes or an API changes. Don't describe local test runs: CI shows them. -->
