library(dplyr)
library(tidyr)

# Count how many trials we have in each frequency condition
load(file.path("data", "updown.RData"))
data_updown <- data
counts_table <- data_updown %>%
    filter(pracval == 1) %>%
    # 1. Count rows for each unique combination
    count(subj, center_freq) %>%
    # 2. Spread the table into a "matrix" format
    pivot_wider(names_from = center_freq, values_from = n, values_fill = 0)
# Print the result
print(counts_table)

# Count how many trials we have for each pracval and subj
load(file.path("data", "updown.RData"))
data_updown <- data
counts_table <- data_updown %>%
    count(subj, pracval) %>%
    pivot_wider(names_from = pracval, values_from = n, values_fill = 0)
# Print the result
print(counts_table)

# Count how many trials we have for each deltaf and subj
load(file.path("data", "updown.RData"))
data_updown <- data
counts_table <- data_updown %>%
    filter(pracval == 4) %>%
    count(subj, freq_diff) %>%
    pivot_wider(names_from = freq_diff, values_from = n, values_fill = 0)
# Print the result
print(counts_table)
