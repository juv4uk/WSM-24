#!/usr/bin/env python3
"""Convert ALL EPE edge contours into one my-lisp data file:
(def eggs (list (list "<id>" (quote ((x y) ...))) ...))

Each contour is strided to <=128 points (matching PLAN.md phase-0 M0
metric) and coordinates are rounded to 4 decimals to cap bignum size.
Raw base stays in resources/eggs/edge_data/ (gitignored); this file is
a derived artifact -- regenerate, don't hand-edit.
"""

import glob
import os
import sys

def main():
    src_dir = sys.argv[1] if len(sys.argv) > 1 else "resources/eggs/edge_data"
    out_path = sys.argv[2] if len(sys.argv) > 2 else "mylisp/eggs-data.my"
    files = sorted(glob.glob(os.path.join(src_dir, "*.bmp.csv")))
    with open(out_path, "w") as out:
        out.write(";; derived: do not edit. source: resources/eggs/edge_data/\n")
        out.write("(def eggs\n  (list\n")
        names = []
        for i, f in enumerate(files):
            pts = [l.split(",") for l in open(f).read().splitlines() if l.strip()]
            step = max(1, len(pts) // 128)
            sel = pts[::step][:128]
            name = os.path.basename(f).replace(".bmp.csv", "")
            names.append(name)
            body = " ".join(f"({float(x):.4f} {float(y):.4f})" for x, y in sel)
            out.write(f'   (list "{name}" (quote ({body})))\n')
        out.write("  ))\n")
        out.write("(def egg-names (quote (" + " ".join(names) + ")))\n")
    print(f"{out_path}: {len(files)} eggs")

if __name__ == "__main__":
    main()
