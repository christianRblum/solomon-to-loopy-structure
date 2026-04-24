# Script ReadMe ####
# Title: Solomon Coder to Loopy Output Format
#
# Summary: This script takes a csv file containing Solomon Coder output and transforms it into a data structure similar to
# Loopy output.
# The Solomon Coder data export has to be done using the method "Analyze -> Select Output -> Coding Sheet", then
# "Analyze -> Export from files to Clipboard" and afterwards paste in Excel and save as csv file.
# The script should work with mismatched columns, in case behavioural categories are not identical across used .arch files.
# This should not occur, because Solomon Coder requires identical configurations for batch exports, but you never know.
# Maybe somebody got creative and combined different versions of the same ethogram into one CSV, stranger things have happened.
# This script should still work. It also has data validation included at the end.
# Still, as always, don't blindly trust and instead verify yourself, it is your data, you are responsible for its accuracy.
# 
# To make this script work with your data, you have to MANUALLY UPDATE REQUIRED INPUTS in those parts where it says
# "MANUAL INPUT FOR X REQUIRED". I turned those parts into chapters so you can find it more easily, but they
# concern the subjects in row 38ish and the instant event behaviours in row 481ish
# 
# Author: Christian Blum, member of Barbara Klump lab and Thomas Bugnyar lab
# Created on: 2025-12-05
# last updated on 2026-04-24

# set up workspace, load data ####

# clear workspace
#rm(list = ls()) # this deletes all objects from your current R session so you start clean

# load packages
library(tidyverse)
library(data.table)

# load data
# fread is faster than read_csv, usefuls for large datasets.
data = fread("solomon_focal_data.csv")

# Define subject order (get this info directly out of Solomon Coder)
# this corresponds to the position in the semicolon string, and the inside the squared brackets.
# you have to set this up manaully for your specific data
# MANUAL INPUT FOR SUBJECTS ####
subjects <- c(
  "Astrid","Joey","Lasse","Sven","Heidi","Anton","Jonas","Elen",
  "Mojo","Jakob","Willi","Sophie","Lena","Klara","Unknown"
)

# explore data ####
# let's have a look at the data structure and check current state and goals
glimpse(data)
head(data)
# View(data)

# we can see that currently the raw Solomon Coder output doesn't contain headers.
# it lists the filename in the first column, first row, followed by an empty row, followed by the data table for that scoring.
# then there is another empty row, then the title and data table for the next scoring etc.
# we need to get the title into a new column "scoring_title".
# then we need to get standardized column headers that match all scoring data tables, as their order and number 
# might be different. again, it shouldn't, due to Solomon Coder .arch configuration compatibility requirements.
# i have tested this with a few different Solomon Coder configurations, but of main importance is the focal data,
# so i have included an example dataset, extracted from 11 .arch focal video files. 
# i have modified one of those 11 output tables by changing up the column order and adding a new column manually, so i can
# properly test the dynamic capabilities of this script. it worked. that is no guarantee that it will work for your data.
# please verify yourself and contact me if my script messed something up.
#
# it also lists time in 0.2 second increments, meaning that every row represents a 0.2 second time window
# we need to get that into start and stop columns per bout to match the loopy file structure.
#
# it also contains scoring categories as columns. the contents are strings, where the position in a sequence of semicolons
# encodes the number of the subject in the subject list. this is the sender.
# the behaviour is written inside the string, in between semicolons.
# if the behaviour is directed, then the receiver is included in square brackets.
# we need to get that info into sender, behaviour and receiver columns. but we don't want those three columns for each of the
# categories, so we need to also long transform the data first, to create a "category" column.

# get scoring title into column ####

# Add scoring_title as first column
data <- data %>%
  mutate(scoring_title = NA_character_) %>%
  relocate(scoring_title, .before = 1)

# Move values containing ".arch" from the second column to scoring_title,
# and delete (set to NA) from the original column
data <- data %>%
  mutate(
    scoring_title = if_else(str_detect(.data[["V1"]], fixed(".arch")), .data[["V1"]], scoring_title),
    !!"V1" := if_else(str_detect(.data[["V1"]], fixed(".arch")), NA_character_, .data[["V1"]])
  )

# Treat blanks as NA and fill down scoring_title
# this way all data sub-tables will be assigned to the corresponding scoring_title
data <- data %>%
  mutate(scoring_title = na_if(scoring_title, "")) %>%
  tidyr::fill(scoring_title, .direction = "down")


# ensure column names and order for all scoring data tables is identical ####
# for the third and (i promise) last time: if you actually need this, you are not working with exported raw data from
# solomon coder, but with data that was modified afterwards. e.g. by combining multiple raw data outputs
# if you have a standardized column format, you can still run this code, it just won't change anything.

## get columns for each scoring_title ####

# Define "blank" in V1 as NA or only whitespace (including "")
is_blank <- function(x) is.na(x) | str_trim(x) == ""

data <- data %>%
  mutate(
    V1_chr = as.character(V1),
    blank_V1 = is_blank(V1_chr),
    # Select rows where current V1 is non-blank and the immediately previous V1 was blank
    first_after_blanks = !blank_V1 & dplyr::lag(blank_V1, default = FALSE)
  )

# Create the header_comparison table with the full rows that meet the condition
header_comparison <- data %>%
  filter(first_after_blanks) %>%
  select(-V1_chr, -blank_V1, -first_after_blanks)
header_comparison

# Check if all selected rows are identical across all columns
all_identical <- header_comparison %>%
  select(-any_of("scoring_title")) %>%
  distinct() 
all_identical # get all variations of column titles

# if not all identical, this says FALSE
all_identical%>%
  nrow() <= 1


## standardize columns for all scoring_titles ####

# Identify header rows per scoring_title (first non-blank in V1 after blanks)
data_marked <- data %>%
  mutate(
    V1_chr = as.character(V1),
    blank_V1 = is_blank(V1_chr),
    first_after_blanks = !blank_V1 & dplyr::lag(blank_V1, default = FALSE)
  )

header_rows <- data_marked %>%
  filter(first_after_blanks) %>%
  select(-V1_chr, -blank_V1, -first_after_blanks)

# Build header vectors (drop blanks), make them unique, per scoring_title
header_list <- header_rows %>%
  mutate(
    header_vec = pmap(
      # use all columns except scoring_title, preserve order
      select(., -scoring_title),
      ~ c(...)
    ) %>%
      map(~ {
        vals <- as.character(.x)
        vals <- vals[!is_blank(vals)]
        # Make duplicate names unique
        make.unique(vals)
      })
  ) %>%
  select(scoring_title, header_vec)

# Determine the "canonical" header order (the most frequent header sequence), 
# also collect all labels across headers to append extras later.
# use a signature to count identical sequences.
header_signatures <- header_list %>%
  mutate(sig = map_chr(header_vec, ~ paste(.x, collapse = "||")))

sig_counts <- header_signatures %>%
  count(sig, sort = TRUE)

canonical_sig <- sig_counts$sig[1]
canonical_header <- header_signatures %>%
  filter(sig == canonical_sig) %>%
  slice(1) %>%
  pull(header_vec) %>%
  .[[1]]

# All labels in order of first appearance across scoring_titles
all_labels_in_order <- header_list %>%
  pull(header_vec) %>%
  reduce(c) %>%
  { .[!duplicated(.)] }

# Extras are labels not in the canonical set, appended in first-appearance order
extras <- setdiff(all_labels_in_order, canonical_header)

final_header_order <- c(canonical_header, extras)
final_col_order <- c("scoring_title", final_header_order)

# Split data by scoring_title and align columns in each subset.
# For each scoring_title:
# - find its header row
# - rename columns using that header
# - drop header row from data
# - add missing columns (NA) and reorder to final_col_order
aligned_list <- data_marked %>%
  select(-V1_chr, -blank_V1, -first_after_blanks) %>%
  group_split(scoring_title, .keep = TRUE) %>%
  map(function(df) {
    st <- df$scoring_title[1]
    
    # Locate the header row within this group
    header_idx <- which(!is_blank(as.character(df$V1)) &
                          dplyr::lag(is_blank(as.character(df$V1)), default = TRUE))
    if (length(header_idx) == 0) {
      # No header row found; return df as-is (but ensure final columns exist)
      current <- df
      # Ensure we have at least scoring_title
      if (!"scoring_title" %in% names(current)) {
        current <- mutate(current, scoring_title = st)
      }
      # Add any missing columns and order
      missing_cols <- setdiff(final_col_order, names(current))
      if (length(missing_cols) > 0) {
        current <- bind_cols(current, as_tibble(setNames(rep(list(NA_character_), length(missing_cols)), missing_cols)))
      }
      return(select(current, any_of(final_col_order)))
    }
    
    header_row <- df[header_idx[1], , drop = FALSE]
    
    # Build header names from this row (excluding scoring_title), drop blanks, make unique
    header_vals <- header_row %>%
      select(-scoring_title) %>%
      as.list() %>%
      unlist(use.names = FALSE) %>%
      as.character()
    header_vals <- header_vals[!is_blank(header_vals)]
    header_vals <- make.unique(header_vals)
    
    # Rename columns for this group's data using the header row
    # Keep only as many data columns as header labels define (excluding scoring_title)
    # If there are more data columns than header labels, we keep them with their V* names.
    data_cols <- setdiff(names(df), "scoring_title")
    rename_map <- c("scoring_title", header_vals)
    # Limit rename_map to existing columns length
    n_map <- min(length(rename_map), ncol(df))
    new_names <- c(rename_map[1], header_vals)
    # Construct final names vector:
    # - first is "scoring_title"
    # - next length(header_vals) positions use header names
    # - remaining positions keep their original names (if any)
    final_names <- c(
      "scoring_title",
      header_vals,
      if (length(data_cols) > length(header_vals)) data_cols[(length(header_vals) + 1):length(data_cols)] else character(0)
    )
    # Apply names
    names(df) <- c("scoring_title", data_cols)  # ensure current baseline
    names(df)[seq_along(final_names)] <- final_names
    
    # Drop the header row itself
    df2 <- df[-header_idx[1], , drop = FALSE]
    
    # Add missing columns with NA and reorder to final_col_order
    missing_cols <- setdiff(final_col_order, names(df2))
    if (length(missing_cols) > 0) {
      df2 <- bind_cols(df2, as_tibble(setNames(rep(list(NA_character_), length(missing_cols)), missing_cols)))
    }
    
    select(df2, any_of(final_col_order))
  })

# recombine into one aligned data frame
aligned_data <- bind_rows(aligned_list)

# this was a lot. and it shouldn't even be necessary! because Solomon Coder actually requires standardised configuration
# in order to output.... nevermind. i promised i wouldn't bring it up again.

# at this point feel free to explore aligned_data and data to see if everything matches
# once you are satisfied, override data with aligned data, update column names, remove empty rows
data <- aligned_data %>%
  select(all_of(final_col_order))%>%
  filter(!is.na(Time) & str_trim(as.character(Time)) != "")


# long transform to create additional columns ####
## create "behavioural_category" column ####
# long transform
long_data <- data %>%
  pivot_longer(
    cols = -c(scoring_title, Time),
    names_to = "behavioural_category",
    values_to = "behaviour"
  )

# remove NA and blank behaviours
long_data_clean <- long_data %>%
  filter(!is.na(behaviour) & str_trim(as.character(behaviour)) != "")

head(long_data) 
head(long_data_clean)

# the next commands might run for a while, be patient

## create "sender" and "receiver" columns ####
data_sender <- long_data %>%
  mutate(behaviour = as.character(behaviour),
         behaviour = replace_na(behaviour, "")) %>%
  mutate(tokens = strsplit(behaviour, ";", fixed = TRUE)) %>%
  mutate(tokens = map(tokens, ~ {
    x <- .x
    length(x) <- length(subjects) # pad/truncate to number of subjects
    replace(x, is.na(x), "")
  })) %>%
  unnest_longer(tokens, values_to = "Behaviour", indices_to = "pos", indices_include = TRUE) %>%
  # Support multiple receivers like [2,3] by extracting all indices, mapping to names, and removing bracketed chunk
  mutate(
    receiver_chunk   = stringr::str_extract(Behaviour, "\\[[^\\]]+\\]"),
    receiver_indices = purrr::map(receiver_chunk, ~ {
      if (is.na(.x)) integer(0) else as.integer(stringr::str_extract_all(.x, "\\d+")[[1]])
    }),
    receivers = purrr::map(receiver_indices, ~ {
      idx <- .x
      # keep only valid, in-bounds indices
      idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(subjects)]
      subjects[idx]
    }),
    Behaviour = stringr::str_trim(stringr::str_replace(Behaviour, "\\s*\\[[^\\]]+\\]\\s*", ""))
  ) %>%
  # One row per receiver (keeps a row with NA receiver if none were specified)
  tidyr::unnest_longer(receivers, values_to = "receiver", keep_empty = TRUE) %>%
  # Drop blank behaviours
  filter(Behaviour != "") %>%
  # Map position to sender
  mutate(sender = subjects[pos]) %>%
  # Final columns
  select(scoring_title, Time, `behavioural_category`, sender, Behaviour, receiver)

## create "start" and "stop" columns ####
bouts <- data_sender %>%
  mutate(
    Time = as.numeric(Time),
    # bout key from all non-Time columns
    bout = pmap_chr(select(., -Time), ~ paste(..., sep = " | "))
  ) %>%
  arrange(bout, Time) %>%
  group_by(bout) %>%
  # Start a new run when the gap is not exactly 0.2
  mutate(
    bout_id = cumsum(coalesce(!dplyr::near(Time - lag(Time), 0.2), TRUE))
  ) %>%
  group_by(bout, bout_id) %>%
  summarise(
    start = min(Time, na.rm = TRUE),
    stop  = max(Time, na.rm = TRUE),
    scoring_title = first(scoring_title),
    behavioural_category = first(behavioural_category),
    sender = first(sender),
    Behaviour = first(Behaviour),
    receiver = first(receiver),
    .groups = "drop"
  ) %>%
  select(bout, bout_id, start, stop, scoring_title, behavioural_category, sender, Behaviour, receiver)

## add "total duration" behaviour to get video length ####
totals <- data_sender %>%
  mutate(Time = as.numeric(Time)) %>%
  group_by(scoring_title) %>%
  summarise(
    start = min(Time, na.rm = TRUE),
    stop  = max(Time, na.rm = TRUE),
    behavioural_category = "Total",
    sender = NA_character_,
    Behaviour = "total duration",
    receiver = NA_character_,
    .groups = "drop"
  ) %>%
  mutate(
    bout = paste(scoring_title, behavioural_category, Behaviour, sender, receiver, sep = " | ")
  ) %>%
  select(bout, start, stop, scoring_title, behavioural_category, sender, Behaviour, receiver)

# Combine
bouts_with_total <- dplyr::bind_rows(bouts, totals)

# Optional: add duration
# this is not included in the Loopy output, but everybody calculates it afterwards anyhow, so might as well do it now
bouts_with_total <- bouts_with_total %>%
  mutate(duration = stop - start)

# reorder columns ####
# this way we get a nice column order for our final dataset
names(bouts_with_total)

loopy_data <- bouts_with_total %>%
  rename(
    behaviour = Behaviour, # make lower case to standardize
    category = behavioural_category # "category" is a better fit, because e.g. trial or condition are not behaviours
  ) %>%
  select(scoring_title, start, stop, duration, category, sender, behaviour, receiver, bout_id, bout) %>%
  arrange(scoring_title, start, category)
  
# data validation ####
# to ensure our calculations (as we just did them) are correct, we also 
# calculate the sum durations of identical bouts via rownumber from the original data.
# then we compare.

## get sum of all bouts in duration, per title, sender, behaviour, receiver ####

# for the original data pre-transformation, based on rownumber
total_duration_original <- data_sender %>%
  mutate(Time = as.numeric(Time)) %>%
  arrange(scoring_title, sender, Behaviour, receiver, Time) %>%
  group_by(scoring_title, sender, Behaviour, receiver) %>%
  mutate(
    new_run = coalesce(!dplyr::near(Time - dplyr::lag(Time), 0.2), TRUE),
    run_id  = cumsum(new_run)
  ) %>%
  group_by(scoring_title, sender, Behaviour, receiver, run_id) %>%
  summarise(run_duration = 0.2 * (n() - 1), .groups = "drop") %>%
  group_by(scoring_title, sender, Behaviour, receiver) %>%
  summarise(row_based_duration = sum(run_duration), .groups = "drop")


# do the same for the transformed data
total_duration_transformed = loopy_data %>%
  group_by(scoring_title, sender, behaviour, receiver) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE), .groups = "drop")


# combine the two datasets
test_data <- total_duration_transformed %>%
  dplyr::left_join(total_duration_original, by = c(
    "scoring_title" = "scoring_title",
    "sender"        = "sender",
    "behaviour"     = "Behaviour",
    "receiver"      = "receiver"
  ))


# check if the two duration values are identical
unique(test_data$total_duration == test_data$row_based_duration)
# it includes FALSE??? Oh no, how could it be?? 
# no worries, it's all fine, this is expected and here's why:

# get extent of FALSENESS (difference in durations as numerical)
test_data$duration_difference = test_data$total_duration - test_data$row_based_duration
sort(unique(test_data$duration_difference)) # these values should be very small (close to 0 for our purposes)
# if that is the case, everything is fine.

# let's get maximum absolute difference without NAs
max_abs <- max(abs(test_data$duration_difference), na.rm = TRUE)
max_abs # if this is less than 0.0000000001 (or 1e-10), it is just a floating point noise issue that can be ignored
# if you want to know what a floating point noise issue is, go look it up.
# ok, fine, here the short version:
# basically, 0.2 cannot be EXACTLY represented in binary, and if you add a bunch of 0.2 values together, you add a bunch of 
# those "NOT EXACTLIES" together. for our purposes we can ignore those tiny differences, nobody cares if an animal eats for 
# 8.526513e-14 seconds longer or shorter....

# threshold for floating point issues (arbitrarily set to something very small)
threshold <- 1e-10  
# adjust if you want to be stricter/looser, anything below 0.2 should technically give the same result, provided you 
# keep the standard Solomon Coder time resolution (0.2) and don't change it. Either way, 1e-10 will work.

# here a quick summary of the check to print to console
if (is.infinite(max_abs)) {
  cat("No comparable rows (all differences are NA)\n")
} else if (max_abs <= threshold) {
  cat("You are fine: differences are within floating-point tolerance.\n")
} else {
  cat("You are NOT fine: differences exceed the tolerance (possible transformation issue).\n")
}

cat(sprintf("max_abs = %.16g, threshold = %.1e\n", max_abs, threshold))


# save the transformed dataset ####

# NOTE:
# instant events (single row behaviours without duration) that are directly following and identical will be 
# interpreted as one bout. 
# so if you have 3 instant events of identical scoring_title, sender, behaviour and receiver at e.g. 
# 0.2, 0.4 and 1.6 seconds, Solomon Coder will treat them as three instant events, 
# but my script will treat them as two bouts. you can get this information by looking at the duration.
# instant events should not have any bouts, so start should be the same as stop and duration should be 0s.
# Or (to stick with Loopy structure), they should have NAs in Stop and Duration.

# optional: for instant events; set Start and Duration to NA:
# detect instant event behaviours
  unique(loopy_data$behaviour) # get them from Solomon Coder configuration

# MANUAL INPUT FOR INSTANT EVENTS ####  
# define instant event behaviours (this might include subject names if they are coded as nearest neighbours)
instant_events = c("Astrid","Joey","Lasse","Sven","Heidi","Anton","Jonas","Elen",
"Mojo","Jakob","Willi","Sophie","Lena","Klara","Unknown", "Long Distance Call", "Headwish", "Food Call")

# set their stop and duration to NA
loopy_data <- loopy_data %>%
  mutate(
    stop = if_else(behaviour %in% instant_events, NA, stop),
    duration = if_else(behaviour %in% instant_events, NA, duration)
  )


# check if there are duration behaviours with 0 duration
duration_behaviours_zero <- loopy_data %>%
  filter(
    duration == 0,
    !behaviour %in% instant_events
  ) 

nrow(duration_behaviours_zero) # shows how many rows are affected, in my example data it is 31

View(duration_behaviours_zero) # shows affected rows

# shows what behaviours are affected and how often
loopy_data %>%
  filter(
    duration == 0,
    !is.na(behaviour),
    !behaviour %in% instant_events
  ) %>%
  count(behaviour, sort = TRUE)

# in my example data, this is:

# # A tibble: 7 × 2
# behaviour                         n
# <chr>                         <int>
# 1 Terminate Contact sit          13
# 2 Initiate Contact sit           12
# 3 CD-Without physical contact     2
# 4 Maunz                           1
# 5 Scratch                         1
# 6 Stretch                         1
# 7 Touch bill                      1

# in this case i believe all of them ar actually instant events that i neglected to include in instant_events above
# if your summary tibble is empty, great, your data is clean
# if not, maybe you also missed some. go back up and include them.
# if you spot actual duration behaviours, that aren't instant events, but don't have a duration. well, you have to figure out
# what you are gonna do with that now. probably go back to your solomon data and check those specific cases, how were they 
# coded? then you can decide if you want to keep it as is, or manually edit the data to give a minimum duration of say 0.2
# seconds. depends on how your ethogram is set up i guess.




# Final Note on data structure ####
# here the column names of our data structure
names(loopy_data)

# and here the column names of the actual loopy data:

# Scoring	Start Start_Frame Stop Stop_Frame Subject Behaviour Value Partner Initiation Termination Modifiers

# you can see they don't match exactly. and they never will, because they can't. sure, you can make some additional changes:

loopy_data_renamed <- loopy_data %>%
  rename(
    Scoring   = scoring_title,
    Start     = start,
    Stop      = stop,
    Behaviour = category,
    Value     = behaviour
  )

names(loopy_data_renamed)  

# but with the different ways that Solomon Coder and Loopy encode data, you will always have a mismatch between
# sender and receiver in Solomon Coder, and Subject, Partner, Initiation in Loopy.
# Termination, as far as i understand, isn't encoded by Solomon Coder at all.

# This script did a lot of heavy lifting, but how to make those final touches depends on each project. 
# Maybe you don't even need any more than what this script already did.


# export data ####

# export the transformed dataset, now in loopy structure
write_csv(loopy_data, # replace with loopy_data_renamed if you want Loopy-like naming
          "transformed_solomon_to_loopy_data.csv")

# export duration differences (the floating point same but not same issue with the binary storage thingy)
# in case you want to check that out in more deatail for some reason
# write_csv(test_data, "duration_differences.csv")



# sessionInfo dump ####
sessionInfo()



