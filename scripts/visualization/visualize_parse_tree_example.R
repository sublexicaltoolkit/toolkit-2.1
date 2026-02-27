# Visualizing Parse Trees (New in v2.1)
#
# visualize_parse_tree() shows how a spelling can be parsed into graphemes,
# and optionally highlights the path that matches a given pronunciation.

# setwd("replace/with/path/to/visualization")
load("../../Toolkit_v2.1.RData")

# --- Basic Parse Tree ---
# Show all possible grapheme parsings for "sebe":
visualize_parse_tree("sebe")

# The following require the DiagrammeR library to be installed, and will open
# up graph visualizations in a new window.

# --- With Target Pronunciation ---
# Highlight the parse path matching a specific pronunciation:
visualize_parse_tree("sebe", "sib", show_plot = TRUE)

# --- Another Pronunciation ---
visualize_parse_tree("sebe", "s1bi", show_plot = TRUE)
