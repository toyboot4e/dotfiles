#!/usr/bin/env python3
"""PreToolUse hook: `git commit` messages must be a title plus Co-Authored-By trailers.

Denies a message with any other line. Asks the user when the message can't be read
from the command (shell variables, `-F -`, ...). Messages git writes itself (merge,
revert, `--no-edit`, `-C`) never pass through `-m`/`-F`, so they are left alone.
"""
import json
import os
import re
import shlex
import sys

GIT_COMMIT = re.compile(r"\bgit\b(?:\s+-[cC]\s+\S+|\s+--?[\w-]+(?:=\S+)?)*\s+commit(?![\w-])")
HEREDOC = re.compile(r"""^\$\(\s*cat\s+<<-?\s*['"]?(\w+)['"]?\n(.*?)\n\s*\1\s*\)$""", re.S)
TRAILER = re.compile(r"^co-authored-by:\s+\S", re.I)
SEPARATORS = {"&&", "||", ";", "|", "&"}


def decide(decision, reason):
    json.dump(
        {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": decision,
                "permissionDecisionReason": reason,
            }
        },
        sys.stdout,
    )
    sys.exit(0)


def ask(why):
    decide("ask", f"Can't check the commit message ({why}). It must be a title plus Co-Authored-By only.")


def messages(tokens, cwd):
    """Yield the message of every `-m`/`-F` after a `git commit` in the token list."""
    in_commit = False
    i = 0
    while i < len(tokens):
        tok = tokens[i]
        nxt = tokens[i + 1] if i + 1 < len(tokens) else None
        i += 1
        if tok in SEPARATORS or tok.endswith(";"):
            in_commit = False
        elif tok == "commit" and "git" in tokens[max(0, i - 8) : i]:
            in_commit = True
        elif not in_commit:
            continue
        elif tok in ("-m", "--message") or re.fullmatch(r"-[a-zA-Z]*m", tok):
            if nxt is None:
                ask("missing -m value")
            yield nxt
            i += 1
        elif tok.startswith("--message="):
            yield tok.split("=", 1)[1]
        elif tok in ("-F", "--file") or tok.startswith("--file="):
            path = tok.split("=", 1)[1] if "=" in tok else nxt
            if not path or path == "-":
                ask("message read from stdin")
            try:
                with open(os.path.join(cwd, os.path.expanduser(path))) as f:
                    yield f.read()
            except OSError:
                ask(f"can't read {path}")
            i += "=" not in tok


def main():
    data = json.load(sys.stdin)
    command = data.get("tool_input", {}).get("command", "")
    if not GIT_COMMIT.search(command):
        return
    try:
        tokens = shlex.split(command, posix=True)
    except ValueError:
        ask("unparseable command")

    paragraphs = []
    for msg in messages(tokens, data.get("cwd") or os.getcwd()):
        heredoc = HEREDOC.match(msg.strip())
        if heredoc:
            msg = heredoc.group(2)
        elif "$" in msg or "`" in msg:
            ask("message uses shell expansion")
        paragraphs.append(msg.strip())
    if not paragraphs:
        return  # editor, --no-edit, -C, merge: nothing written by hand

    lines = [l.strip() for l in "\n\n".join(paragraphs).splitlines() if l.strip()]
    extra = [l for l in lines[1:] if not TRAILER.match(l)]
    if extra:
        decide(
            "deny",
            "Commit messages are the title line plus Co-Authored-By only; remove the body: "
            + " / ".join(extra)[:300],
        )


main()
