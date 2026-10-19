#!/usr/bin/env python3
"""Pre/PostToolUse/Bash: every unpushed commit a command creates must end with the
Co-Authored-By trailer, with no blank line after it (GitHub ignores it otherwise).
Checked after the command, since `git commit-tree`, `rebase` and `update-ref` build
messages that a PreToolUse check cannot see."""
import json
import os
import re
import subprocess
import sys
import tempfile

GIT_WRITE = re.compile(
    r"\bgit\b[^;&|\n]*\b(commit|commit-tree|rebase|update-ref|cherry-pick|merge|am|revert|reset|pull)\b"
)
TRAILER = re.compile(r"^Co-Authored-By: .+ <noreply@anthropic\.com>$")


def git(cwd, *args):
    out = subprocess.run(["git", "-C", cwd, *args], capture_output=True, text=True)
    return out.stdout if out.returncode == 0 else None


def unpushed(cwd):
    out = git(cwd, "rev-list", "HEAD", "--not", "--remotes")
    return set(out.split()) if out is not None else None


def state_path(data):
    key = data.get("tool_use_id") or data.get("session_id") or "default"
    return os.path.join(tempfile.gettempdir(), "claude-commit-trailer", re.sub(r"\W", "_", key))


def message(cwd, sha):
    raw = git(cwd, "cat-file", "commit", sha)
    return None if raw is None else raw.split("\n\n", 1)[-1]


def problem(msg):
    if msg.endswith("\n\n"):
        return "ends with a blank line after the trailer"
    last = msg.rstrip("\n").splitlines()[-1] if msg.strip() else ""
    if not TRAILER.match(last):
        return "does not end with the Co-Authored-By trailer"
    return None


def main():
    try:
        data = json.load(sys.stdin)
    except Exception:
        return
    if not GIT_WRITE.search((data.get("tool_input") or {}).get("command") or ""):
        return
    cwd = data.get("cwd") or os.getcwd()
    path = state_path(data)
    if data.get("hook_event_name") == "PreToolUse":
        before = unpushed(cwd)
        if before is not None:
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w") as f:
                f.write("\n".join(sorted(before)))
        return
    try:
        with open(path) as f:
            before = set(f.read().split())
        os.remove(path)
    except OSError:
        return
    after = unpushed(cwd)
    if after is None:
        return
    bad = []
    for sha in sorted(after - before):
        msg = message(cwd, sha)
        reason = msg is not None and problem(msg)
        if reason:
            bad.append(f"{sha[:10]} {msg.splitlines()[0]!r}: {reason}")
    if bad:
        print(
            "New commits must end with `Co-Authored-By: ... <noreply@anthropic.com>` and no blank "
            "line after it; pipe reused messages through `git stripspace`. Rewrite these:\n"
            + "\n".join(bad),
            file=sys.stderr,
        )
        sys.exit(2)


main()
