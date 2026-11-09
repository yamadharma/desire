#!/usr/bin/env python
"""Sync `;; Version:' headers in .el files from package.json.

Usage:
    python3 scripts/sync_version.py             # rewrite headers
    python3 scripts/sync_version.py --dry-run   # show changes only
    python3 scripts/sync_version.py --check     # exit 1 on mismatch
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

TARGETS = [
    "desire.el",
    "desire-straight.el",
    "desire-quelpa.el",
    "desirepath.el",
]
VERSION_RE = re.compile(r"^(;; Version:[ \t]*)(.+)$", re.MULTILINE)


def find_root() -> Path:
    here = Path(__file__).resolve().parent
    for d in (here, *here.parents):
        if (d / "package.json").is_file():
            return d
    sys.exit("sync_version: cannot locate package.json")


def read_version(root: Path) -> str:
    with (root / "package.json").open(encoding="utf-8") as fh:
        data = json.load(fh)
    if "version" not in data:
        sys.exit(f'{root / "package.json"}: missing "version" key')
    return data["version"]


def update_file(path: Path, version: str, mode: str) -> bool:
    """Return True on success (or on no-op), False on mismatch in --check."""
    text = path.read_text(encoding="utf-8")
    match = VERSION_RE.search(text)
    if not match:
        sys.exit(f"{path}: no `;; Version:' header")

    old = match.group(2).strip()
    if old == version:
        print(f"{path.name}: {version} (ok)")
        return True

    if mode == "check":
        print(f"{path.name}: expected {version}, found {old}", file=sys.stderr)
        return False

    if mode == "dry-run":
        print(f"{path.name}: {old} -> {version}")
        return True

    new_text = text[: match.start(2)] + version + text[match.end(2) :]
    path.write_text(new_text, encoding="utf-8")
    print(f"{path.name}: {old} -> {version}")
    return True


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--check", action="store_true")
    args = ap.parse_args()

    mode = "check" if args.check else "dry-run" if args.dry_run else "write"
    root = find_root()
    version = read_version(root)
    print(f"package.json: version = {version}")

    failures = sum(
        0 if update_file(root / name, version, mode) else 1 for name in TARGETS
    )
    if mode == "check" and failures:
        print(
            f"\n{failures} file(s) out of sync; run: make sync-version", file=sys.stderr
        )
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
