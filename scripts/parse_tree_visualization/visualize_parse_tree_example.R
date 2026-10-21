# Visualizing Parse Trees
# TODO add toolkit guide section
#
# visualize_parse_tree() shows how a spelling can be parsed into graphemes,
# and optionally highlights the path that matches a given pronunciation.

# setwd("<path to scripts/visualization>")
load("../../Toolkit_v2.1.RData")

# Show all possible grapheme parsings for "sebe" in the terminal:
visualize_parse_tree("sebe")

# The following require igraph or DiagrammeR installed:

# We can highlight the path for a given pronunciation in the visualization:
visualize_parse_tree("sebe", "sib", show_plot = TRUE)

# Another example:
visualize_parse_tree("sebe", "s1bi", show_plot = TRUE)
