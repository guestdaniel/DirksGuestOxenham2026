#
# This script preprocesses raw .dat files from the experimental protocol and transforms them into
# a format suitable for public consumption. Crucially, this step includes de-identifying participant
# labels so that the public dataset is fully anonymous. This is done in the script in a way that the
# script itself contains no identifying information, so it can still be made publicly available
# even if the data it operates on cannot be.
# 
# This script is specific to the "samediff" data from the same-different experiment described in the
# paper. A separate script preprocessed the same-different dataset.

# Load libraries
library(dplyr)
library(ggplot2)

# Define the directions where raw data will be found and where preprocessed data will be saved
raw_dir = 'data/PitchRoving/Same\ or\ Different/Revised/Analysis'  # where the raw data lives (typical users would not have this on their path)
processed_dir = 'data'        # where the final data lives

# Create structures to store data files list
temp <- list.files(raw_dir, pattern = '.dat')
temp <- temp[!grepl("individual", temp)]
data_files = temp
data_files = file.path(raw_dir, data_files)

# function to read in a single sheet
read_data <- function(filename, max_num_trials){
  # Extract Listener and TargetTone
  Listener <- toupper(strsplit(filename, "_")[[1]][2])
  
  # Add column names 
  temp <- read.table(filename, sep = "", skip = 1, header = FALSE)
  colnames(temp) <- c("run","track","freq_diff","intensity","center_freq","num_trials","num_correct")
  names_ordered <- names(temp)
  
  # Add Listener to data matrix 
  temp$subj <- Listener
  
  # Add empty columns to store tone_freq_int1 and tone_freq_int2
  temp$tone_freq_int1 = NA
  temp$tone_freq_int2 = NA
  
  # Cut out unwanted trials
  temp = temp[1:max_num_trials, ]
  
  # Loop through data points 
  for (i in 1:nrow(temp))  {
    
    # low tone first
    if (temp$center_freq[i] %% 2 == 0) {
      temp$tone_freq_int1[i] = (temp$center_freq[i]-2) #high tone first
      temp$tone_freq_int2[i] = (temp$center_freq[i]-2) #low tone second
      temp$trial_type[i] = "same tones"
    }
    # high tone first
    else if (temp$center_freq[i] %% 2 == 0.5) {
      
        temp$tone_freq_int1[i] = (temp$center_freq[i]-0.5)*(1-temp$freq_diff[i]/200) #high tone first
        temp$tone_freq_int2[i] = (temp$center_freq[i]-0.5)*(1+temp$freq_diff[i]/200) #low tone second
        temp$trial_type[i] = "low-high"
    }
    # same tones
    else {
      temp$tone_freq_int1[i] = (temp$center_freq[i]-3)*(1+temp$freq_diff[i]/200) #high tone first
      temp$tone_freq_int2[i] = (temp$center_freq[i]-3)*(1-temp$freq_diff[i]/200) #low tone second
      temp$trial_type[i] = "high-low"
    }
  }
  
  return(temp)
}

# Create data frame to store everything
data = data.frame()
max_num_trials = 840

# Go through each .dat file and load with function above
data = bind_rows(data, lapply(data_files, read_data, max_num_trials=max_num_trials))

# Convert subj ID to anonymized version
data$subj = as.factor(data$subj)
levels(data$subj) = 1:length(levels(data$subj))

# Save preprocessed data to disk
save('data', file=file.path(processed_dir, 'samediff.RData'))
write.csv(data, file=file.path(processed_dir, 'samediff.csv'), row.names=FALSE)


