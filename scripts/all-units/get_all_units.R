# TODO add toolkit guide section reference 
#
# Script to obtain the unique phonemes and graphemes across all mapping tables, 
# and the number of valid mappings, i.e. units, at each grain size.

# setwd("<path to scripts/all-units>")
load("../../Toolkit_v2.1.RData")

all_mappings <- rbind(syllable_initial_mappings, syllable_medial_mappings, syllable_final_mappings, word_initial_mappings, word_final_mappings)
print(paste0("Unique phonemes: ", length(unique(all_mappings$phoneme))))
print(paste0("Unique graphemes: ", length(unique(all_mappings$grapheme))))

for (table_name in c("all_tables_PG", "all_tables_OR", "all_tables_OC", "all_tables_ONC")){
    table <- get(table_name)
    level <- gsub("^all_tables_", "", table_name)
    print(paste0("Total ", level, " units: ", length(table[[1]]$phoneme)))
}
