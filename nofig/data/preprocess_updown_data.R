# Libraries
library(dplyr)
library(ggplot2)
library(lme4)

# Configure directories
if (Sys.info()['nodename'] == 'daniel-desktop'){
	data_dir = '/home/daniel/apc_store/pitchbias/data'
	git_dir = '/home/daniel/pitchbias/data'
	server_dir = '/mnt/m/Experiments/Coral/PitchRoving'
	setwd(server_dir)
	directories = c('Unpracticed', 
		'Unpracticed/Thursday\ Participants', 
		'Practiced', 
		'Practiced/Thursday\ Participants',
		'Practiced/Thursday\ Extra',
		'Practice2', 
		'Practice3')
} else {
	# Coral can put her directories here
}

# Compile lists of files to load and folders to load from 
data_files = list()
for (ii in 1:length(directories)) {
	temp <- list.files(directories[[ii]], pattern = '.dat')
	temp <- temp[!grepl("control", temp)]
	temp <- temp[!grepl("$", temp, fixed=TRUE)]
	temp <- temp[!grepl("X", temp, fixed=TRUE)]
	temp <- temp[!grepl("Loaddata_v1.m", temp)]
	data_files[[ii]] = temp
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

# Create vectors to control data import
df_2col = data.frame()
max_num_trials = c(60, 60, 300, 300, 96, 480, 480)
pracvals = c(1, 1, 2, 2, 2.5, 3, 4) #1 = unpracticed, 
                                  #2 = practiced, 
                                  #2.5 = practiced extras for Thursday subjects, 
                                  #3 = practice 2, 
                                  #4 = practice 3
flags = c(NA, 'Thursday', NA, 'Thursday', 'Thursday', NA, NA)
# Loop through data directories and import data
for (ii in 1:length(directories)) {
	# Set directory
	setwd(directories[ii])
	# Pull data
	df_2col = bind_rows(df_2col, lapply(data_files[[ii]], read_data, max_num_trials=max_num_trials[ii], pracval=pracvals[ii], flag=flags[ii]))
	# Return directory
	setwd(server_dir)
}

# Filter out bad conditions
df_2col = df_2col %>%
	filter(center_freq != 888)

# Patch issues in data
# Remove 889 and 890 from unpracticed for all subjects (pracval = 1)
df_2col = df_2col %>%
       filter(!(center_freq == 889 & pracval == 1)) 	
df_2col = df_2col %>%
       filter(!(center_freq == 890 & pracval == 1)) 	

# Change 889 in Thursday Extras to pracval = 2 (these conditions I ran in)
df_2col[grepl("AAM|KJR|LKJ|SAC", df_2col$Listener) & 
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
dir.create(file.path(data_dir, Sys.Date()), recursive=TRUE)
save('data', file=file.path(data_dir, Sys.Date(), 'clean_data.RData'))
dir.create(file.path(git_dir, Sys.Date()), recursive=TRUE)
save('data', file=file.path(git_dir, Sys.Date(), 'clean_data.RData'))

