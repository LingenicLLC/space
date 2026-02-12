#!/usr/bin/env python3
"""Extract Unicode Character Database to F* format."""

import sys

def parse_unicode_data(filename):
    """Parse UnicodeData.txt and extract relevant fields."""
    uppercase = []  # (cp, upper)
    lowercase = []  # (cp, lower)
    titlecase = []  # (cp, title)
    ccc = []        # (cp, class)
    decomp = []     # (cp, [cps], is_canonical)

    with open(filename) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            fields = line.split(';')
            if len(fields) < 15:
                continue

            cp = int(fields[0], 16)
            name = fields[1]
            combining_class = int(fields[3]) if fields[3] else 0
            decomp_field = fields[5]
            upper = fields[12].strip()
            lower = fields[13].strip()
            title = fields[14].strip()

            # Skip ranges (like CJK ideographs)
            if name.endswith(', First>') or name.endswith(', Last>'):
                continue

            # Case mappings
            if upper:
                uppercase.append((cp, int(upper, 16)))
            if lower:
                lowercase.append((cp, int(lower, 16)))
            if title and title != upper:  # Only if different from uppercase
                titlecase.append((cp, int(title, 16)))

            # Combining class
            if combining_class != 0:
                ccc.append((cp, combining_class))

            # Decomposition
            if decomp_field:
                is_canonical = not decomp_field.startswith('<')
                if decomp_field.startswith('<'):
                    # Remove tag like <compat>, <circle>, etc.
                    parts = decomp_field.split('> ')
                    if len(parts) > 1:
                        decomp_field = parts[1]
                    else:
                        continue

                cps = [int(x, 16) for x in decomp_field.split()]
                if cps:
                    decomp.append((cp, cps, is_canonical))

    return uppercase, lowercase, titlecase, ccc, decomp

def parse_special_casing(filename):
    """Parse SpecialCasing.txt for multi-char case mappings."""
    entries = []

    with open(filename) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            # Remove comments
            if '#' in line:
                line = line[:line.index('#')]
            line = line.strip()
            if not line:
                continue

            parts = [p.strip() for p in line.split(';')]
            if len(parts) < 4:
                continue

            cp = int(parts[0], 16)
            lower = [int(x, 16) for x in parts[1].split()] if parts[1].strip() else []
            title = [int(x, 16) for x in parts[2].split()] if parts[2].strip() else []
            upper = [int(x, 16) for x in parts[3].split()] if parts[3].strip() else []
            condition = parts[4].strip() if len(parts) > 4 else ''

            # Only include unconditional mappings that expand
            if not condition and (len(lower) > 1 or len(title) > 1 or len(upper) > 1):
                entries.append((cp, lower, title, upper))

    return entries

def parse_composition_exclusions(filename):
    """Parse CompositionExclusions.txt."""
    exclusions = []

    with open(filename) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('#'):
                continue

            # Remove comments
            if '#' in line:
                line = line[:line.index('#')]
            line = line.strip()
            if not line:
                continue

            # Handle ranges
            if '..' in line:
                start, end = line.split('..')
                for cp in range(int(start, 16), int(end, 16) + 1):
                    exclusions.append(cp)
            else:
                try:
                    exclusions.append(int(line, 16))
                except:
                    pass

    return exclusions

def format_hex(n):
    """Format number as F* hex literal."""
    return f"0x{n:04X}"

def generate_fstar_uppercase(mappings):
    """Generate F* code for uppercase mappings."""
    lines = ["(** Simple uppercase mappings from UnicodeData.txt *)"]
    lines.append("let uppercase_mappings : list mapping_entry = [")

    for cp, upper in sorted(mappings):
        lines.append(f"  {{ codepoint = {format_hex(cp)}; mapped = {format_hex(upper)} }};")

    lines.append("]")
    return "\n".join(lines)

def generate_fstar_lowercase(mappings):
    """Generate F* code for lowercase mappings."""
    lines = ["(** Simple lowercase mappings from UnicodeData.txt *)"]
    lines.append("let lowercase_mappings : list mapping_entry = [")

    for cp, lower in sorted(mappings):
        lines.append(f"  {{ codepoint = {format_hex(cp)}; mapped = {format_hex(lower)} }};")

    lines.append("]")
    return "\n".join(lines)

def generate_fstar_ccc(entries):
    """Generate F* code for combining classes."""
    lines = ["(** Canonical Combining Classes from UnicodeData.txt *)"]
    lines.append("let combining_class_table : list ccc_entry = [")

    for cp, ccc in sorted(entries):
        lines.append(f"  {{ codepoint = {format_hex(cp)}; ccc = {ccc} }};")

    lines.append("]")
    return "\n".join(lines)

def generate_fstar_decomp(entries, canonical_only=True):
    """Generate F* code for decompositions."""
    label = "canonical" if canonical_only else "all"
    lines = [f"(** {'Canonical' if canonical_only else 'All'} decompositions from UnicodeData.txt *)"]
    lines.append(f"let {label}_decomposition_table : list decomp_entry = [")

    for cp, cps, is_canonical in sorted(entries):
        if canonical_only and not is_canonical:
            continue
        cps_str = "; ".join(format_hex(c) for c in cps)
        lines.append(f"  {{ codepoint = {format_hex(cp)}; decomposition = [{cps_str}]; is_canonical = {'true' if is_canonical else 'false'} }};")

    lines.append("]")
    return "\n".join(lines)

def generate_fstar_special_casing(entries):
    """Generate F* code for special casing."""
    lines = ["(** Multi-character case mappings from SpecialCasing.txt *)"]
    lines.append("let special_casing_table : list special_case_entry = [")

    for cp, lower, title, upper in sorted(entries):
        lower_str = "; ".join(format_hex(c) for c in lower) if lower else ""
        title_str = "; ".join(format_hex(c) for c in title) if title else ""
        upper_str = "; ".join(format_hex(c) for c in upper) if upper else ""
        lines.append(f"  {{ codepoint = {format_hex(cp)}; lower = [{lower_str}]; title = [{title_str}]; upper = [{upper_str}] }};")

    lines.append("]")
    return "\n".join(lines)

def generate_fstar_exclusions(exclusions):
    """Generate F* code for composition exclusions."""
    lines = ["(** Composition exclusions from CompositionExclusions.txt *)"]
    lines.append("let composition_exclusion_table : list nat = [")

    for cp in sorted(exclusions):
        lines.append(f"  {format_hex(cp)};")

    lines.append("]")
    return "\n".join(lines)

def main():
    print("Parsing UnicodeData.txt...")
    uppercase, lowercase, titlecase, ccc, decomp = parse_unicode_data("UnicodeData.txt")
    print(f"  Uppercase mappings: {len(uppercase)}")
    print(f"  Lowercase mappings: {len(lowercase)}")
    print(f"  Titlecase mappings: {len(titlecase)}")
    print(f"  Non-zero CCC: {len(ccc)}")
    print(f"  Decompositions: {len(decomp)}")

    canonical_decomp = [(cp, cps, is_can) for cp, cps, is_can in decomp if is_can]
    print(f"  Canonical decompositions: {len(canonical_decomp)}")

    print("\nParsing SpecialCasing.txt...")
    special = parse_special_casing("SpecialCasing.txt")
    print(f"  Special casing entries: {len(special)}")

    print("\nParsing CompositionExclusions.txt...")
    exclusions = parse_composition_exclusions("CompositionExclusions.txt")
    print(f"  Composition exclusions: {len(exclusions)}")

    # Generate composition pairs from canonical decompositions
    composition_pairs = []
    for cp, cps, is_canonical in decomp:
        if is_canonical and len(cps) == 2:
            composition_pairs.append((cps[0], cps[1], cp))
    print(f"  Composition pairs: {len(composition_pairs)}")

    # Write outputs
    print("\nGenerating F* code...")

    with open("uppercase.fst.part", "w") as f:
        f.write(generate_fstar_uppercase(uppercase))
    print(f"  uppercase.fst.part ({len(uppercase)} entries)")

    with open("lowercase.fst.part", "w") as f:
        f.write(generate_fstar_lowercase(lowercase))
    print(f"  lowercase.fst.part ({len(lowercase)} entries)")

    with open("ccc.fst.part", "w") as f:
        f.write(generate_fstar_ccc(ccc))
    print(f"  ccc.fst.part ({len(ccc)} entries)")

    with open("decomp_canonical.fst.part", "w") as f:
        f.write(generate_fstar_decomp(decomp, canonical_only=True))
    print(f"  decomp_canonical.fst.part ({len(canonical_decomp)} entries)")

    with open("special_casing.fst.part", "w") as f:
        f.write(generate_fstar_special_casing(special))
    print(f"  special_casing.fst.part ({len(special)} entries)")

    with open("exclusions.fst.part", "w") as f:
        f.write(generate_fstar_exclusions(exclusions))
    print(f"  exclusions.fst.part ({len(exclusions)} entries)")

    # Generate composition pairs
    with open("composition.fst.part", "w") as f:
        f.write("(** Composition pairs (inverse of canonical 2-char decompositions) *)\n")
        f.write("let composition_pair_table : list comp_entry = [\n")
        for first, second, composed in sorted(composition_pairs):
            f.write(f"  {{ first = {format_hex(first)}; second = {format_hex(second)}; composed = {format_hex(composed)} }};\n")
        f.write("]\n")
    print(f"  composition.fst.part ({len(composition_pairs)} entries)")

    print("\nDone!")

if __name__ == "__main__":
    main()
