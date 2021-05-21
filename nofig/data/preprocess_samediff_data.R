# NOTE: This script is *not* expected to be run by outside users because it requires access to
# raw data on protected UMN servers... It is merely intended for inspection and end users should instead
# use preprocessed data files (see README)

library(dplyr)
library(ggplot2)
rm(list = ls())

# Configure directories
data_dir = 'data'
server_dir = '/mnt/m/Experiments/Coral/PitchRoving/Same\ or\ Different/Revised/Analysis'  # users outside APC Lab cannot access raw data here
setwd(server_dir)

### COMPILE DATA ###
data_files = list()
temp <- list.files(pattern = '.dat')
temp <- temp[!grepl("individual", temp)]
data_files = temp

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

data = data.frame()
max_num_trials = 840

# Pull data
data = bind_rows(data, lapply(data_files, read_data, max_num_trials=max_num_trials))

# Save preprocessed data to disk
setwd('/home/daniel/DirksGuestOxenham2021')
save('data', file=file.path(data_dir, 'samediff.RData'))

