#!/usr/bin/env python3
"""Convert an EPE edge-data contour CSV (x,y lines) into a my-lisp data
literal: (define name (list (list x y) ...)).

Coordinates are rounded to 4 decimals — the exact-rational engine of
my-lisp treats these as exact decimals anyway; rounding just caps the
bignum size at the entrance.

Usage: python3 tools/csv_to_my.py <contour.csv> <out.my> <name>
Provenance: source file + sha256 are embedded in the output header.
"""

import hashlib
import sys
from pathlib import Path


def main() -> None:
    src, out, name = sys.argv[1], sys.argv[2], sys.argv[3]
    raw = Path(src).read_bytes()
    sha = hashlib.sha256(raw).hexdigest()

    pts = []
    for line in raw.decode().splitlines():
        line = line.strip()
        if not line:
            continue
        x, y = line.split(",")
        pts.append(f"({float(x):.4f} {float(y):.4f})")

    body = "\n  ".join(pts)
    Path(out).write_text(
        f";; converted from {Path(src).name}\n"
        f";; sha256 {sha}\n"
        f";; points: {len(pts)}\n"
        f"(def {name}\n"
        f"  (quote (\n  {body})))\n"
    )
    print(f"{out}: {len(pts)} points from {Path(src).name}")


if __name__ == "__main__":
    main()
