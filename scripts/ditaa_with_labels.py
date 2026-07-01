#!/usr/bin/env python3
"""Generate ditaa diagram, add labels, render to PNG at high DPI."""
import subprocess, sys, os, re, xml.etree.ElementTree as ET

DITAA_JAR = sys.argv[1]
DITAA_SRC = sys.argv[2]
OUT_PNG = sys.argv[3]
LABEL_TOP = sys.argv[4] if len(sys.argv) > 4 else ""
LABEL_BOTTOM = sys.argv[5] if len(sys.argv) > 5 else ""
SCALE = float(sys.argv[6]) if len(sys.argv) > 6 else 1.0

tmp_svg = OUT_PNG + ".svg"

# Step 1: generate SVG with ditaa
subprocess.run(
    ["java", "-Djava.awt.headless=true", "-jar", DITAA_JAR, DITAA_SRC, tmp_svg, "--svg", "-r", "-E", "-s", str(SCALE)],

    capture_output=True
)

# Step 2: parse and modify SVG
with open(tmp_svg) as f:
    svg = f.read()

# Fix namespace for proper parsing
svg = svg.replace(' xmlns="http://www.w3.org/2000/svg"', ' xmlns:x="http://www.w3.org/2000/svg" xmlns="http://www.w3.org/2000/svg"')

# Extract dimensions
w_match = re.search(r'width="(\d+)"', svg)
h_match = re.search(r'height="(\d+)"', svg)
w = int(w_match.group(1)) if w_match else 400
h = int(h_match.group(1)) if h_match else 300

# Calculate label area
label_h = 40  # height for labels
new_h = h + label_h * 2
new_w = w + 20  # slight horizontal padding

# Fix SVG header
svg = re.sub(r'(<svg[^>]*)>',
    lambda m: m.group(1) + f' viewBox="-10 -{label_h} {new_w} {new_h}" width="{new_w}" height="{new_h}">',
    svg, count=1)

# Build label SVG elements
labels_svg = ""
if LABEL_TOP:
    labels_svg += f'<text x="{w//2}" y="-12" font-family="sans-serif" font-size="14" fill="#000" text-anchor="middle" font-weight="bold">{LABEL_TOP}</text>\n'
if LABEL_BOTTOM:
    labels_svg += f'<text x="{w//2}" y="{h + label_h - 8}" font-family="sans-serif" font-size="14" fill="#666" text-anchor="middle">{LABEL_BOTTOM}</text>\n'

svg = svg.replace('<defs>', labels_svg + '\n<defs>', 1)

with open(tmp_svg, 'w') as f:
    f.write(svg)

# Step 3: convert SVG to PNG at 2x resolution
dpi_scale = 2.0
out_w = int(new_w * dpi_scale)
out_h = int(new_h * dpi_scale)
subprocess.run(
    ["rsvg-convert", tmp_svg, "-o", OUT_PNG, "-w", str(out_w), "-h", str(out_h)],
    check=True
)

print(f"Generated {OUT_PNG}: {out_w}x{out_h}")
os.remove(tmp_svg)
