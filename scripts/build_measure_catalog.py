import pandas as pd
import json

# Load your CSV
df = pd.read_csv("SublexicalFrequencyAndConsistencyMeasures.csv")

# Keep only rows that look like actual measures (contain a ".")
rows = df.dropna(subset=["Measure"])
measures_clean = rows[rows["Measure"].str.contains("\\.")]

def infer_directionality(name: str, target: str | None) -> str | None:
    """Infer Reading vs Spelling directionality from measure name or target."""
    lower = name.lower()

    if (target and target.lower() in ["g_freq", "gp"]) or "_g_freq" in lower or "_gp" in lower:
        return "Reading"   # Grapheme → Phoneme
    if (target and target.lower() in ["p_freq", "pg"]) or "_p_freq" in lower or "_pg" in lower:
        return "Spelling"  # Phoneme → Grapheme
    if "consistency" in lower:
        return "Reading"   # default assignment
    return None            # Neutral (unit frequencies, etc.)

def parse_measure(name: str, desc: str):
    """Parse a measure string like 'OC_freq_noposition_PG.mean' into attributes."""
    if '.' not in name:
        return None
    base, stat = name.split('.')
    parts = base.split('_')

    # First token is always the unit type
    unitType = parts[0]

    weighting = None
    position = None
    measureType = "Frequency"   # Default assumption
    target = None

    # Second token often encodes weighting/position
    if parts[1] == 'default':
        weighting = 'Type'
    elif parts[1] == 'freq':
        weighting = 'Token'
    elif parts[1] == 'noposition':
        weighting = 'Type'
        position = 'noposition'
    else:
        weighting = parts[1]

    # Remaining tokens: position, target, etc.
    for p in parts[2:]:
        if p in ['PG','GP','P','G','PG_freq','P_freq','G_freq']:
            target = p
        elif p == 'noposition':
            position = 'noposition'
        elif p == 'freq':
            measureType = 'Frequency'
        else:
            pass

    directionality = infer_directionality(name, target)

    return {
        "id": name,                # The original measure name
        "unitType": unitType,
        "measureType": measureType,
        "directionality": directionality,
        "weighting": weighting,
        "position": position,
        "target": target,
        "stat": stat,
        "label": name.replace('_',' '),
        "description": desc if pd.notna(desc) else ""
    }

# Parse all valid measures with their description
parsed_all = []
for _, row in measures_clean.iterrows():
    m = parse_measure(row["Measure"], row.get("Description", ""))
    if m:
        parsed_all.append(m)

# Save as JSON
with open("measureCatalog.json", "w") as f:
    json.dump(parsed_all, f, indent=2)

print(f"Exported {len(parsed_all)} measures to measureCatalog.json")
