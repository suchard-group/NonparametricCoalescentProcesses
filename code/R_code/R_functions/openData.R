# opening the data 

#### this functions opens the log data ####
# with path_to_log
openLogFile <- function(logFile, cols=c()) {
  if (length(cols)==0) {
    dataLog <- fread(logFile, check.names = TRUE) |> as_tibble()
  } else {
    dataLog <- fread(logFile, select = cols, check.names = TRUE) |> as_tibble()
  }
  return(dataLog)
}  

# with path_to_xml (and job name/number))
openLogFileJob <- function(path=getwd(), job=1, cols=c()) {
  xmlName <- path |> basename()
  jobList  <- path |> list.dirs(recursive = FALSE) |> basename()
  logName <- str_c(xmlName,".log")
  traceName <- str_c("trace_", xmlName,".csv") # TODO traceName not used
  if (typeof(job) == "character") {
    if (job %in% jobList) job <- which(jobList, job)  else stop("This job does not exist")
  } else if ((typeof(job) %in% c("double", "integer")) && (job > length(jobList))) {
    stop("This job does not exist")
  }
  print(jobList[job])
  logFile <- file.path(path, jobList[job], logName)
  dataLog <- openLogFile(logFile, cols)

  list(data = dataLog, job = jobList[job])
}
#################################################

jobsListing <- function(comb_list) {
  merged_strings <- list()
  for (i in seq_along(comb_list)) {
    comb_list[[i]] <- comb_list[[i]] |> as.character()
    merged_strings <- c(merged_strings, list(paste(comb_list[[i]], collapse = "")))
  }
  collapsedValues <<- paste(unlist(merged_strings), collapse = "_")
  combinations <- do.call(expand.grid, comb_list)
  jobs <<- apply(combinations, 1, function(x) paste(x, collapse = "_")) |> 
    sort()
  print(collapsedValues)
  print(jobs)
}

keepFirstValuesInList <- function(list) {
    newList <-  lapply(list, 
             function(x) {
               if (is.vector(x) && length(x) > 1) {
                return(x[1])
              } else {
                return(x)
              }
            })
  return(newList)
}

extract_vector <- function(file_content, prefix) {
  # Find the line that contains the prefix
  line <- file_content[grepl(prefix, file_content)]
  
  # Remove the prefix and split the string by spaces
  values <- strsplit(trimws(sub(paste0(prefix,"="), "", line)), " ")[[1]]
  
  # Convert to numeric
  as.numeric(values)
}

# this function generates a numbered list of all the first level subfolders
listDirs <- function(path = getwd(), print = FALSE) { 
  indexed_directories <- matrix()
  directories <- path |> list.dirs(recursive = FALSE) |> basename()
  indexed_directories <- cbind(1:length(directories), directories)
  if ( print ) for( i in 1:length(directories) ) cat(i, directories[i], "\n")
  n_dirs <- length(directories)
  list(n = n_dirs, dirs = directories, matrix = indexed_directories)
}

# this function generates a numbered list of all the second level subfolders 
# of the indexed first level sufolder
listSubDirs <- function(path = getwd(), index = 1, print = FALSE) { 
  dirs <- path |> list.dirs(recursive = FALSE) |> basename()
  if ( typeof(index) == "character" ) {
    if (index %in% dirs) dirName <- index else stop("This directory does not exist")
  } else {
    dir_name <- dirs[index]
  }
  currentDir <- dirs[index]
  subDirs <- file.path(path, currentDir) |> 
    list.dirs(recursive = FALSE) |> basename()
  cat(currentDir, "\n")
  if ( print ) for (i in 1:length(subDirs)) cat(i, subDirs[i], "\n")
  n_subDirs <- length(subDirs)
  list(n = n_subDirs, dir = currentDir)
}

  