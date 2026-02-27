# this fucntion takes as input a path and a time threshold "t" in hours, 
# and returns a list of .log files in the specified folder and its subfolders
# mofified less than "t" hours ago.

# It outputs a tibble with (date, time, file name, folder name, and full path) 
# of those files.

# library(tibble)
# library(dplyr)

findRecentLogs <- function(path, t) {
  # Convert hours to seconds
  time_threshold <- t * 3600
  
  # Get the current time
  current_time <- Sys.time()
  
  # Recursively list all .log files in the folder and its subfolders
  log_files <- list.files(path, pattern = "\\.log$", recursive = TRUE, full.names = TRUE)
  
  # Initialize a list to store results
  results <- list()
  
  # Process each file
  for (file in log_files) {
    # Get file information
    file_info <- file.info(file)
    mod_time <- file_info$mtime
    time_diff <- difftime(current_time, mod_time, units = "secs")
    
    # If the file was modified within the threshold, collect information
    if (time_diff < time_threshold) {
      file_name <- basename(file)
      folder_name <- basename(dirname(file))
      results <- append(results, list(
        list(
          date = as.Date(mod_time),
          time = format(mod_time, "%H:%M"),
          file_name = file_name,
          folder_name = folder_name,
          full_path = file
        )
      ))
    }
  }
  
  # If there are results, create a tibble; otherwise, return NULL
  if (length(results) > 0) {
    result_tibble <- bind_rows(lapply(results, as_tibble))
    print("Recently modified .log files:")
    cat("logs modified less than ", t, " hours ago\n")
    cat("within the folder: ", path, "\n")
    print(result_tibble[1:4])
    return(result_tibble)
  } else {
    cat("No .log files modified in the last", t, "hours.\n")
    return(NULL)
  }
}

# Example usage
# Replace "/path/to/your/folder" with the actual folder path
# result <- findRecentLogs("/path/to/your/folder", 2) 
# Check for .log files modified in the last 2 hours


################################################################################

# find_recent_logsOld <- function(path, t) {
#   # Convert hours to seconds
#   time_threshold <- t * 3600
#   
#   # Get the current time
#   current_time <- Sys.time()
#   
#   # Recursively list all .log files in the folder and its subfolders
#   log_files <- 
#     list.files(path, pattern = "\\.log$", recursive = TRUE, full.names = TRUE)
#   
#   # Check the modification times of the files
#   recent_logs <- sapply(log_files, function(file) {
#     mod_time <- file.info(file)$mtime
#     time_diff <- difftime(current_time, mod_time, units = "secs")
#     time_diff < time_threshold
#   })
#   
#   # Print the paths of recently modified .log files
#   if (any(recent_logs)) {
#     cat("Recently modified .log files:\n")
#     cat(log_files[recent_logs], sep = "\n")
#   } else {
#     cat("No .log files modified in the last", t, "hours.\n")
#   }
# }
# 
# # Example usage
# # Replace "/path/to/your/folder" with the actual folder path
# find_recent_logsOld("/path/to/your/folder", 2) 
# Check for .log files modified in the last 2 hours
