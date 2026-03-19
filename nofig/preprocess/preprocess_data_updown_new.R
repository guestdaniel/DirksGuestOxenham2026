# preprocess_data_updown_new.R
#
# This script preprocesses raw .dat files from the experimental protocol and transforms them into
# a format suitable for public consumption. Crucially, this step includes de-identifying participant
# labels so that the public dataset is fully anonymous. This is done in the script in a way that the
# script itself contains no identifying information, so it can still be made publicly available
# even if the data it operates on cannot be.
# 
# This script is specific to the "updown" data from the up-down experiment described in the paper.
# A separate script preprocessed the same-different dataset.

# Load libraries
library(dplyr)
library(ggplot2)

# Define the directions where raw data will be found and where preprocessed data will be saved
raw_dir = 'data/PitchRoving'  # where the raw data lives (typical users would not have this on their path)
processed_dir = 'data'        # where the final data lives

# Make a vector of the names of folders within 
directories = c('Unpracticed',
                'Unpracticed/Thursday\ Participants',
		        'Practiced',
                'Practiced/Thursday\ Participants',
                'Practiced/Thursday\ Extra',
                'Practice2',
                'Practice3')

# Compile lists of files to load and folders to load from 
data_files = list()
for (ii in 1:length(directories)) {
    # Get a list of all the .dat files in this directory
	temp <- list.files(file.path(raw_dir, directories[[ii]]), pattern = '.dat')

    # Filter out controls, $s, Xs, and the loaddata function
	temp <- temp[!grepl("control", temp)]
	temp <- temp[!grepl("$", temp, fixed=TRUE)]
	temp <- temp[!grepl("X", temp, fixed=TRUE)]
	temp <- temp[!grepl("Loaddata_v1.m", temp)]

    # Store the path (relative to root dir) of each data file
	data_files[[ii]] = file.path(raw_dir, directories[[ii]], temp)
}

# Create a function to read in each data file
read_data <- function(filename, max_num_trials, pracval, flag){
  # Extract Listener and TargetTone
  Listener <- toupper(strsplit(filename, "_")[[1]][2])
  TargetTone <- strsplit(filename, "_")[[1]][3]
  TargetTone <- substr(TargetTone, 1, 2)  
 
  # Add column names 
  temp <- read.table(filename, sep = "", skip = 1, header = FALSE)
  colnames(temp) <- c("run","track","freq_diff","intensity","center_freq","num_trials","num_correct")
  names_ordered <- names(temp)
 
  # Add Listener, TargetTone, and pracval to data matrix 
  temp$Listener <- Listener
  temp$TargetTone <- TargetTone
  temp$pracval <- pracval
  temp$flag <- flag
 
  # Add empty columns to store tone_freq_int1 and tone_freq_int2
  temp$tone_freq_int1 = NA
  temp$tone_freq_int2 = NA

  # Cut out unwanted trials
  temp = temp[1:max_num_trials, ]

  # Loop through data points 
  for (i in 1:nrow(temp))  {
    
    # if the target is the HIGH tone
    if (temp$TargetTone[i] == "hi") {
      if (temp$center_freq[i] %% 2 == 0) {
        temp$tone_freq_int1[i] = (temp$center_freq[i]-2)*(1+temp$freq_diff[i]/200) # High tone comes first
        temp$tone_freq_int2[i] = (temp$center_freq[i]-2)*(1-temp$freq_diff[i]/200) # Low tone comes second
      }
      else {
        temp$tone_freq_int1[i] = (temp$center_freq[i]-1)*(1-temp$freq_diff[i]/200) # Low tone comes first
        temp$tone_freq_int2[i] = (temp$center_freq[i]-1)*(1+temp$freq_diff[i]/200) # High tone comes second
      }
      
      # if the target is the LOW tone      
    } else {
      if (temp$center_freq[i] %% 2 == 0) {
        temp$tone_freq_int1[i] = (temp$center_freq[i]-2)*(1-temp$freq_diff[i]/200) # Low tone comes first
        temp$tone_freq_int2[i] = (temp$center_freq[i]-2)*(1+temp$freq_diff[i]/200) # High tone comes first
      }
      else {
        temp$tone_freq_int1[i] = (temp$center_freq[i]-1)*(1+temp$freq_diff[i]/200) # High tone comes first
        temp$tone_freq_int2[i] = (temp$center_freq[i]-1)*(1-temp$freq_diff[i]/200) # Low tone comes first
      }  
    }
  }
  
  return(temp)
  }

# Create vectors needed to provide supplemental information for data import
df_2col = data.frame()
max_num_trials = c(60, 60, 300, 300, 96, 480, 480)
pracvals = c(1, 1, 2, 2, 2.5, 3, 4) #1 = unpracticed, 
                                  #2 = practiced, 
                                  #2.5 = practiced extras for Thursday subjects, 
                                  #3 = practice 2, 
                                  #4 = practice 3
flags = c(NA, 'Thursday', NA, 'Thursday', 'Thursday', NA, NA)

# Loop through data directories and use lapply to apply import function to each data file
for (ii in 1:length(directories)) {
	# Pull data
	df_2col = bind_rows(df_2col, lapply(data_files[[ii]], read_data, max_num_trials=max_num_trials[ii], pracval=pracvals[ii], flag=flags[ii]))
}

# Filter out bad conditions
df_2col = df_2col %>%
	filter(center_freq != 888)
df_2col = df_2col %>%
  filter(center_freq != 880)

# Patch issues in data
# Remove 889 and 890 from unpracticed for all subjects (pracval = 1)
df_2col = df_2col %>%
       filter(!(center_freq == 889 & pracval == 1)) 	
df_2col = df_2col %>%
       filter(!(center_freq == 890 & pracval == 1)) 	

# Create anonymous labels for subjects instead of true labels
df_2col$Listener = as.factor(df_2col$Listener)
levels(df_2col$Listener) = 1:length(levels(df_2col$Listener))

# Change 889 in Thursday Extras to pracval = 2 (these conditions I ran in)
df_2col[df_2col$Listener %in% c(2, 14, 15, 19) & 
	df_2col$center_freq == 889 &
	df_2col$pracval == 2.5, "pracval"] = 2

# remove pracval = 2.5 from data frame
df_2col = df_2col %>%
  filter(pracval != "2.5")

# Fix things to be more elegant
data = df_2col # rename to data
data$freq_diff = factor(data$freq_diff)
data$subj = factor(data$Listener)
data$target_tone = data$TargetTone
data$practice = data$pracval
data[data$center_freq %% 2 == 0 & data$target_tone == "hi", "sign"] = "High-Low"
data[data$center_freq %% 2 == 0 & data$target_tone == "lo", "sign"] = "Low-High"
data[data$center_freq %% 2 != 0 & data$target_tone == "hi", "sign"] = "Low-High"
data[data$center_freq %% 2 != 0 & data$target_tone == "lo", "sign"] = "High-Low"
data$sign = factor(data$sign)
data$target_tone = factor(data$target_tone, 
			  levels=levels(as.factor(data$target_tone)),
			  labels=c("High Tone Target", "Low Tone Target"))
data$practice = factor(data$practice,
		       labels=c("Unpracticed", "Practice Session 1", "Practice Session 2", "Practice Session 3"))


# Save preprocessed data to disk
save('data', file=file.path(processed_dir, 'updown.RData'))


