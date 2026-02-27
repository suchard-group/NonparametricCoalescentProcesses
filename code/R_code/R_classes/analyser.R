library(R6)

Analyser <- 
  R6Class("Analyser",
          public = list(
           project = NULL,
           path = NULL,

           attributes = list(),
           jobsAttributes = list(),
           additionalAttributes = list(),
           buildFromJobAttributes = FALSE,
           
           jobsList = NULL,
           
           burnIn = 0.1,
           thinning = 1,
           burnOut = 1,
           
           plottingFlags = list(),
           actions = list(),
           gridResults = NULL,

           fullList = list(),
           
           initialize = function(project = NULL, path = NULL, 
                                 jobs = NULL, 
                                 attributes = list(), jobsAttributes = list(), additionalAttributes = list(),
                                 burnIn = 0.1, thinning = 1, burnOut = 1,
                                 plottingFlags = list(), actions = list()) {
             self$project <- project
             self$path <- path
             
             self$jobsAttributes <- jobsAttributes
             self$additionalAttributes <- additionalAttributes
             if (length(attributes) == 0) {
               self$buildFromJobAttributes <- TRUE
               self$attributes <- append(self$jobsAttributes, self$additionalAttributes)
             } else {
               self$attributes <- attributes
             }
             
             # this is the dominant alternative to jobsAttributes and  additionalAttributes
             self$burnIn <- burnIn 
             self$thinning <- thinning
             self$burnOut <- burnOut
             
             self$plottingFlags <- plottingFlags
             self$actions <- actions
             
             self$gridResults <- getGridResultsStructure(self$project)
           },
           
           loadData = function() {
             stop("This method should be implemented in a subclass.")
           },
           
           getFullList = function() {
             return(self$fullList) # fix this
           },
           
           getJobsList = function() {
             return(self$jobsList)
           },
           
           getData = function(index = 1) {
             return(self$fullList[[index]]$data)
           },
           
           getInputs = function(index = 1) {
             stop("This method should be implemented in a subclass.")
           },
          
           printXml = function(xmlFilePath, start = 1, end = Inf, tail = NULL, text = NULL) {
             if (file.exists(xmlFilePath)) {
               
               cat("xml found")
               xmlFileRaw <- readLines(xmlFilePath)
               xmlFile <- gsub("\t", "    ", xmlFileRaw)
               total_lines <- length(xmlFile)
               cat(" with", total_lines, "lines \n")
               
               if (!is.null(text)) {
                 line_number_text <- which(grepl(text, xmlFile))[1]
                 if (is.na(line_number_text)) {
                   cat("Text not found \n")
                   line_number_text <- NULL
                 }
               } else {
                 cat("Text not specified \n")
                 line_number_text <- NULL
               }
               
               if (!is.null(line_number_text)) {
                 # print lines from "text" to the end
                 print(xmlFile[line_number_text:total_lines])
               } else {
                 if (!is.null(tail)) {
                   cat("Tail is null \n")
                   start = total_lines - tail + 1
                   end = total_lines
                 } 
                 if (is.infinite(start)) start <- 1
                 if (is.infinite(end) || end < start) end <- total_lines
                 
                 if (start > total_lines || end > total_lines) {
                   stop("Start or end are greater than total lines \n")
                 }
                 cat("Start and end are: ", start, end, "\n")
                 cat("-------------------------------------------------\n")
                 cat("-------------------------------------------------\n")
                 cat(xmlFile[start:end], sep = "\n")
                 # if (tail == Inf) tail <- total_lines - 1
                 # print(xmlFile[(total_lines - tail):total_lines])
               }
             } else {
               stop("xml not found \n")
             }
           },
           
           # thinAndBurn is called directly in initialization step!
           thinAndBurn = function(data) {
             if (self$burnIn < 0 ) stop("BurnIn must be greater than 0")
             if (self$burnOut < self$burnIn) stop("BurnOut must be greater than BurnIn")
             if (self$burnIn >= 1 && self$burnOut > 1) {
               burnInRow <- self$burnIn
               burnOutRow <- self$burnOut
             } else if (self$burnIn < 1 && self$burnOut <= 1) {
               burnInRow <- self$burnIn*nrow(data)
               burnOutRow <- self$burnOut*nrow(data)
             } else {
               stop("BurnIn and BurnOut must be in the same range")
             }
             # if ( self$burnIn < 1 ) burnInRow <- self$burnIn*nrow(data) else burnInRow <- 0
             # if ( self$burnOut < 1 ) burnOutRow <- self$burnOut*nrow(data) else burnOutRow <- nrow(data)
             data <- data %>% 
               filter(row_number() > burnInRow & row_number() < burnOutRow) %>% 
               filter(row_number() %% self$thinning == 0)
             return(data)
           },

           plotter = function() {
             className <- paste0("DataToPlot", self$project)
             if (!exists(className)) {stop(paste("DataToPlot class for project", self$project, "does not exist."))}
             for (i in seq_along(self$fullList)) {
               self$onePlot(i, className)
             }
             return(self$gridResults)
           },
           
           onePlot = function(index, className) {
             cat("Plotting job: ", self$fullList[[index]]$job, "\n")
             dataToPlot <- get(className)$new(self$fullList[[index]]$data, self$path, 
                                                self$fullList[[index]]$job, attributes = self$fullList[[index]]$attributes,
                                                self$plottingFlags, self$actions, self$gridResults)
             self$gridResults <- dataToPlot$plotCreator()
           },
           
           getDataToPlot = function(index = 1) {
             if (index > length(self$fullList)) {
               stop("Index is out of bounds")
             }
             dataToPlot <- get(paste0("DataToPlot", self$project))$new(self$fullList[[index]]$data, self$path, 
                                                                        self$fullList[[index]]$job, 
                                                                        attributes = self$fullList[[index]]$attributes,
                                                                        plottingFlags = self$plottingFlags, 
                                                                        actions = self$actions, 
                                                                        gridResults = self$gridResults)
             return(dataToPlot)
           },
           
           mcmc = function(index = 1) { # can also use jobName
             mcmcAnalyser <- McmcAnalyser$new(self$fullList[[index]]$data, self$path, self$fullList[[index]]$job, 
                                              self$fullList[[index]]$attributes, self$actions, burnIn = 0.0)
             return(mcmcAnalyser)
           }, 
           
           openLogFile = function(logFile, cols=c()) {
             if (length(cols)==0) {
               dataLog <- fread(logFile, check.names = TRUE) |> as_tibble()
             } else {
               dataLog <- fread(logFile, select = cols, check.names = TRUE) |> as_tibble()
             }
             return(dataLog)
           }
           # preProcessing = function(data, jobName) {
           #   data <- thinAndBurn(data)
           # }
           
         )
)

# ---- JOBLIST-specific subclass ----
AnalyserJobsList <- 
  R6Class("AnalyserJobsList",
         inherit = Analyser,
         public = list(
           availableJobs = NULL,
           attributesList = list(),

           initialize = function(project = NULL, path = NULL, jobs = NULL,  #path_to_xml
                                 attributes = list(), jobsAttributes = list(), additionalAttributes = list(),
                                 burnIn = 0.1, thinning = 1, burnOut = 1,
                                 plottingFlags = list(), actions = list()) {
             super$initialize(project = project, path = path, 
                              attributes = attributes, jobsAttributes = jobsAttributes, additionalAttributes = additionalAttributes,                                 
                              burnIn = burnIn, thinning = thinning, burnOut = burnOut,
                              plottingFlags = plottingFlags, actions = actions)
             if (length(attributes) == 0 & length(jobsAttributes) == 0 & is.null(jobs)) stop("Either Attributes or jobs Attributes or jobs must be specified")
             self$availableJobs <- list.dirs(self$path, recursive = FALSE) |> basename()
             cat("Path to jobs: ", self$path, "\n")
             cat("Available jobs: ", self$availableJobs, "\n")
             cat("\n")
             self$listsBuilder()
             self$validateJobsList()
             self$loadData()
           },

           loadData = function() { 
             for (i in 1:length(self$jobsList)) {
               loaderData <- self$openLogFileJob(job = self$jobsList[[i]])
               self$fullList[[length(self$fullList) + 1]] <- list(
                 data = self$thinAndBurn(loaderData$data),
                 job = loaderData$job, 
                 attributes = self$attributesList[[i]]
               )
             }
           },
           
           openLogFileJob = function(job=1, cols=c()) { # job can be a character or a number
             xmlName <- self$path |> basename()
             # self$availableJobs  <- self$path |> list.dirs(recursive = FALSE) |> basename()
             # traceName <- str_c("trace_", xmlName,".csv") # TODO traceName not used
             if (typeof(job) == "character") {
               if (job %in% self$availableJobs) jobIndex <- which(job == self$availableJobs)  else stop("This job does not exist")
             } else if (typeof(job) %in% c("double", "integer")) {
               if (job > length(self$availableJobs)) { stop("This job does not exist") }
                jobIndex <- job
             }
             cat("Loading job: ", self$availableJobs[jobIndex], "\n")
             logName <- str_c(xmlName,".log")
             logFile <- file.path(self$path, self$availableJobs[jobIndex], logName)
             dataLog <- self$openLogFile(logFile, cols)

             return(list(data = dataLog, job = self$availableJobs[jobIndex]))
           },
           
           listsBuilder = function() {
             param_grid <- expand.grid(self$attributes, stringsAsFactors = FALSE)
             self$attributesList <- apply(param_grid, 1, as.list)
             if (is.null(self$jobsList)) {
               self$buildingJobsList()
             }
           },
           
           buildingJobsList = function() {
             if (self$buildFromJobAttributes) {
               temp_list <- expand.grid(self$jobsAttributes, stringsAsFactors = FALSE) |> apply(1, as.list)
               self$jobsList <- sapply(temp_list, function(x) paste(unlist(x), collapse = "_"))
             } else {
               self$jobsList <- sapply(self$attributesList, function(x) paste(unlist(x), collapse = "_"))
             }
           },

           validateJobsList = function() {
             if (is.null(self$jobsList)) stop("At this point the job list must be specified")
             if (length(self$availableJobs) == 0) stop(paste("No jobs found at the specified path: ", self$path))
             
             if (is.character(self$jobsList[[1]])) {
               jobsIndices <- which(self$availableJobs %in% self$jobsList)
               if (!length(jobsIndices)) {
                 cat(" The path is: ", self$path, "\n")
                 cat(" The Job ID is: ", self$jobsList, "\n")
                 if (length(self$jobsList) == 1) {
                   cat(" The available jobs are: ")
                   cat(self$compare_jobs(self$jobsList, self$availableJobs))
                 } else {
                   cat(" The available jobs are: ", self$availableJobs, "\n")
                 }
                 stop("Job IDs (character) not compatible")
               }
             } else if (is.numeric(self$jobsList[[1]])) {
               if (!(max(self$jobsList) <= length(self$availableJobs) & min(self$jobsList) > 0)) {
                 stop("Jobs ids (numbers) not compatible")
               }
             } else {
               stop("List of folders to be analysed not of appropriate type")
             }
           }, 
           
           getInputs = function(index) { 
             inputFilePath <- file.path(self$path, self$jobsList[index], "inputs.txt")
             if (file.exists(inputFilePath)) { 
               cat("input found \n")
               inputFile <- readLines(inputFilePath)
               print(inputFile)
             } else {
               cat("input not found \n")
             }
           }, 
           
           getXml = function(index, start = 1, end = Inf, tail = NULL, text = NULL) {
             xmlName <- paste0(basename(self$path), ".xml")
             xmlFilePath <- file.path(self$path, self$jobsList[index], xmlName)
             self$printXml(xmlFilePath, start = start, end = end, tail = tail, text)
           }, 
           
           getPathToJob = function(index) {
             return(file.path(self$path, self$jobsList[[index]]))
           },
           
           printPathToJob = function(index) {
             cat("Path to job", self$jobsList[[index]], "is", "\n",
                 self$getPathToJob(index), "\n")
           },
           
           getFileInfo = function(index) {
             path_to_log <- file.path(self$path, self$jobsList[[index]], paste0(basename(self$path), ".log"))
             file_info <- file.info(path_to_log)
             print(path_to_log)
             print(file_info$mtime)
             #return(file_info)
           }, 
           
           compare_strings = function(str1, str2) {
             parts1 <- unlist(strsplit(str1, "_"))
             parts2 <- unlist(strsplit(str2, "_"))
             
             if (length(parts1) != length(parts2)) {
               return("WARNING: Available job String have different numbers of parts than the job ID")
             } else {
               differences <- mapply(function(x, y) if (x != y) paste0("(", y, ", ", x, ")") else NA, parts1, parts2)
               
               differences <- differences[!is.na(differences)]
               return(paste(differences, collapse = "; "))
             }

           }, 
           
           compare_jobs = function(job, availableJobs) {
             cat("There are available jobs: ", length(availableJobs), "\n")
             for (availableJob in availableJobs) {
               diff <- self$compare_strings(unlist(job), availableJob)
               return(paste0(availableJob, " NE: ", diff, "\n"))
             }
           }
        )
)

# ---- Log-specific subclass ----

AnalyserFromLog <- 
  R6Class("AnalyserFromLog",
          inherit = Analyser,
          public = list(
            cols = NULL,

            initialize = function(project = NULL, path = NULL, cols = c(), jobs = NULL,#path_to_log
                                  attributes = list(), jobsAttributes = list(), additionalAttributes = list(),
                                  burnIn = 0.1, thinning = 1, burnOut = 1,
                                  plottingFlags = list(), actions = list()) {
              super$initialize(project = project, path = path, jobs = jobs, 
                               attributes = attributes, jobsAttributes = jobsAttributes, additionalAttributes = additionalAttributes,
                               burnIn = burnIn, thinning = thinning, burnOut = burnOut,
                               plottingFlags = plottingFlags, actions = actions)
              self$cols <- cols
              self$jobsList <- paste(unlist(self$attributes), collapse = "_")
              self$loadData()
            },

            loadData = function() {
              data <- self$openLogFile(self$path, self$cols) |>
                self$thinAndBurn()
              self$fullList <- list(list(data = data, 
                                         job = self$jobsList,
                                         attributes = self$attributes)
                                    )
            }, 
            
            getFileInfo = function() {
              file_info <- file.info(self$path)
              cat("The path is: ", self$path)
              cat("The file infos are: ",file_info$mtime)
              #return(file_info)
            }, 
            
            getInputs = function() { 
              inputFilePath <- file.path(self$path, "inputs.txt")
              if (file.exists(inputFilePath)) { 
                cat("input found \n")
                inputFile <- readLines(inputFilePath)
                print(inputFile)
              } else {
                cat("input not found \n")
              }
            }, 
            
            getXml = function(tail = Inf, text = NULL) {
              xmlName <- paste0(basename(self$path), ".xml")
              xmlFilePath <- file.path(self$path, self$jobsList[1], xmlName)
              self$printXml(xmlFilePath, tail, text)
            }
          )
)


getGridResultsStructure <- function(project) {
  switch(project,
         "NPRates" = list(
           hyperparameters = list(),
           logRatesMeansSds = list(),
           logRatesQuantiles = list(),
           logRatesBoxPlots = list()
         ),
         "SkyGrid" = list(
           demographicModel = list(),
           other1 = list()
         ),
         "AnotherProject" = list(
           # Define the structure for another project here
         ),
         # Add more projects as needed
         list()  # Default empty list if project not recognized
  )
}

# Example of usage
# XML case
# plotterXML <- PlotLogRatesXML$new(path = "path/to/xml", jobs = NULL, attributes = list(), burnIn = 0.1)
# resultXML <- plotterXML$run()
# 
# # Log case
# plotterLog <- PlotLogRatesLog$new(path = "path/to/log", cols = c())
# resultLog <- plotterLog$run()
