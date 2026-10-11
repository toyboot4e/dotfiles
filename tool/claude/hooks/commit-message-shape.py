#!/usr/bin/env python3
"""PreToolUse/Bash: a commit message may hold only a subject paragraph, plus
comment/conflict blocks and trailers. Prose body paragraphs are refused."""
import json
import os
import re
import shlex
import sys

TRAILER_KEYS = ("co-authored-by", "signed-off-by", "reviewed-by", "acked-by",
                "reported-by", "tested-by", "suggested-by", "cc", "change-id")
TRAILER = re.compile(r"^(?:%s):\s*\S" % "|".join(TRAILER_KEYS), re.I)
CONFLICTS_HEADER = re.compile(r"^#?\s*Conflicts:\s*$", re.I)
SEPARATORS = {";", "&&", "||", "|", "&", "(", ")", "{", "}"}
HEREDOC = re.compile(r"<<-?\s*(['\"]?)(\w+)\1[^\n]*\r?\n(.*?)\r?\n\s*\2\b", re.S)
COMMIT_CMD = re.compile(r"(?:^|[\s/])git\s.*\bcommit(?![\w-])")


def split_words(text):
    try:
        return shlex.split(text, comments=False)
    except ValueError:
        return None


def commit_segments(cmd):
    """Token lists of `git ... commit ...` invocations, quote-aware."""
    toks = split_words(cmd)
    if toks is None:
        toks = re.findall(r"\S+", cmd)
    for i, t in enumerate(toks):
        if t != "git" and not t.endswith("/git"):
            continue
        seg = []
        for t2 in toks[i + 1:]:
            if t2 in SEPARATORS:
                break
            seg.append(t2)
        if "commit" in seg:
            yield seg


def read_file(path, cwd):
    try:
        with open(os.path.join(cwd, os.path.expanduser(path))) as f:
            return f.read()
    except OSError:
        return None


def option_messages(toks, cwd):
    """Messages given by -m/-F, including short-flag clusters like `-am msg`."""
    out = []
    i = 0
    while i < len(toks):
        t = toks[i]
        nxt = toks[i + 1] if i + 1 < len(toks) else None
        i += 1
        kind = value = None
        if t in ("--message", "--file"):
            kind, value = t[2], nxt
            i += 1
        elif t.startswith(("--message=", "--file=")):
            kind, value = t[2], t.split("=", 1)[1]
        elif not t.startswith("--"):
            hit = re.fullmatch(r"-[a-zA-Z]*?([mF])(.*)", t, re.S)
            if hit:
                kind, value = hit.group(1).lower(), hit.group(2)
                if not value:
                    value = nxt
                    i += 1
        # heredoc-fed values are read from the raw command by commit_heredocs
        if value is None or "<<" in value:
            continue
        if kind == "f":
            value = None if value == "-" else read_file(value, cwd)
        if value is not None:
            out.append(value)
    return out


def commit_heredocs(cmd):
    """Bodies of heredocs whose `<<` operator belongs to a `git commit` command."""
    for m in HEREDOC.finditer(cmd):
        line = cmd[cmd.rfind("\n", 0, m.start()) + 1:m.start()]
        last = re.split(r"&&|\|\||;|\|", line)[-1]
        if COMMIT_CMD.search(" " + last):
            yield m.group(3)


def paragraphs(msg):
    out, cur = [], []
    for line in msg.splitlines():
        if line.strip():
            cur.append(line)
        elif cur:
            out.append(cur)
            cur = []
    if cur:
        out.append(cur)
    return out


def allowed_tail(par):
    """Everything after the subject must be comments, a conflict list, or trailers."""
    if all(l.lstrip().startswith("#") for l in par):
        return True
    if CONFLICTS_HEADER.match(par[0]) and all(l.strip() for l in par[1:]):
        return True
    return all(TRAILER.match(l.strip()) for l in par)


def main():
    try:
        data = json.load(sys.stdin)
    except Exception:
        return
    cmd = (data.get("tool_input") or {}).get("command") or ""
    cwd = data.get("cwd") or os.getcwd()
    msgs = [m for toks in commit_segments(cmd) for m in option_messages(toks, cwd)]
    msgs += commit_heredocs(cmd)
    for msg in msgs:
        for par in paragraphs(msg)[1:]:
            if allowed_tail(par):
                continue
            print(json.dumps({
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": (
                        "Commit messages carry a subject paragraph only. No explanatory body "
                        "paragraphs; after the subject, only comment lines, a `Conflicts:` file "
                        "list, or attribution trailers such as Co-Authored-By are allowed; issue references (Refs/Fixes/Closes/See-also) are not. "
                        f"Offending paragraph starts: {par[0].strip()!r}"
                    ),
                }
            }))
            return


main()
