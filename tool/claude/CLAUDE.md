# Global Instructions

- Stay inside the current git worktree: relative or CWD-based paths only, never paths into the main checkout or other worktrees; scope agents to it.
- Shape: verdict or root cause first, then only evidence that changes my next step, then decisions that are mine. No label, fragments over prose, no bolded-lead bullet lists. "why"/"how" is not a detail request: ≤3 sentences each, then offer to expand. "tldr" means ≤3 lines.
- Findings, tradeoffs, corrections: one line each, max 3; beyond that list titles and ask. When the cap cuts anything, end with `Cut: a · b · c` (titles only).
- Brevity limits the prose, never the work: investigate, verify and fix at full depth without narrating it.
- Code comments: none by default; only the non-obvious why, a constraint or a footgun. Never describe the edit (what changed, why this approach, what was there) in a comment or docstring; that goes in chat or the commit. Commit subjects are imperative, with no issue references.
