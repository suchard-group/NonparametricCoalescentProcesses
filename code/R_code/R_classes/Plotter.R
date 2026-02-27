library(R6)

Plotter <- R6Class("Plotter",
                   public = list(
                     project = NULL,
                     path = NULL,
                     jobs = NULL,
                     attributes = list(),
                     burnIn = 0.1,
                     thinning = 1,
                     plottingFlags = list(),
                     actions = list(),
                     gridResults = NULL,
                     
                     initialize = function(project = NULL, path = path, jobs = NULL, attributes = list(), 
                                           burnIn = 0.1, thinning = 1,
                                           plottingFlags = list(), actions = list()) {
                       self$project <- project
                       self$path <- path
                       self$jobs <- jobs
                       self$attributes <- attributes
                       self$burnIn <- burnIn
                       self$thinning <- thinning
                       self$plottingFlags <- plottingFlags
                       self$actions <- actions
                       
                       self$gridResults <- getGridResultsStructure(self$project)
                     },
                     
                     loadData = function() {
                       stop("This method should be implemented in a subclass.")
                     },
                     
                     processJob = function(data, jobName) {

                       data <- thinAndBurn(data, self$burnIn, self$thinning)
                       className <- paste0("DataToPlot", self$project)
                       if (exists(className)) {
                         dataToPlot <- get(className)$new(data, self$path, jobName, self$attributes,
                                                      self$plottingFlags, self$actions, self$gridResults)

                         self$gridResults <- dataToPlot$plotCreator()
                       } else {
                         stop(paste("DataToPlot class for project", self$project, "does not exist."))
                       }
                     },
                     
                     run = function() {
                       dataJobs <- self$loadData()
                       for (i in seq_along(dataJobs)) {
                         cat("Processing job", dataJobs[[i]]$job, "\n")
                         self$processJob(dataJobs[[i]]$data, dataJobs[[i]]$job)
                       }
                       return(self$gridResults)
                     }
                   )
)

# JOBLIST-specific subclass
PlotterFromJobsList <- R6Class("PlotterFromJobsList",
                               inherit = Plotter,
                               public = list(
                                 path_to_xml = NULL,
                                 
                                 initialize = function(project = NULL, path_to_xml = NULL, jobs = NULL, attributes = list(), 
                                                       burnIn = 0.1, thinning = 1,
                                                       plottingFlags = list(), actions = list()) {
                                   super$initialize(project = project, path = path_to_xml, jobs = jobs, attributes = attributes, 
                                                    burnIn = burnIn, thinning = thinning, 
                                                    plottingFlags = plottingFlags, actions = actions)
                                   
                                   self$path_to_xml <- path_to_xml
                                 },
                                 
                                 loadData = function() {
                                   all_jobs <- list.dirs(self$path_to_xml, recursive = FALSE) |> basename()
                                   jobs_to_load <- self$jobsToLoad(self$jobs, all_jobs)
                                   dataJobs <- lapply(jobs_to_load, function(i) {
                                     loaderData <- openLogFileJob(self$path_to_xml, i)
                                     list(data = loaderData$data, job = loaderData$job)
                                   })
                                   return(dataJobs)
                                 },
                                 
                                 jobsToLoad = function(jobs, all_jobs) {
                                   if (is.null(jobs)) return(seq_along(all_jobs))
                                   
                                   if (is.character(jobs)) {
                                     jobs_to_load <- which(all_jobs %in% jobs)
                                     if (!length(jobs_to_load)) stop("Job IDs (character) not compatible")
                                     return(jobs_to_load)
                                   } else if (is.numeric(jobs)) {
                                     if (!(max(jobs) <= length(all_jobs) & min(jobs) > 0)) { 
                                       stop("Jobs ids (numbers) not compatible") 
                                     } else { 
                                       return(jobs) 
                                     }
                                   } else {
                                     stop("List of folders to be analysed not of appropriate type")
                                   }
                                 }
                               )
)



# Log-specific subclass
PlotterFromLog <- R6Class("PlotterFromLog",
                          inherit = Plotter,
                          public = list(
                            path_to_log = NULL, 
                            
                            cols = NULL,
                            
                            initialize = function(project = NULL, path_to_log = NULL, cols = c(), attributes = list(), 
                                                  burnIn = 0.1, thinning = 1,
                                                  plottingFlags = list(), actions = list()) {
                              super$initialize(project = project, path = path_to_log, jobs = NULL, attributes = attributes, 
                                               burnIn = burnIn, thinning = thinning, plottingFlags = plottingFlags, actions = actions)

                              self$cols <- cols
                            },
                            
                            loadData = function() {
                              data <- openLogFile(self$path, self$cols)
                              return(list(list(data = data, job = NULL)))
                            }
                          )
)


getGridResultsStructure <- function(project) {
  switch(project,
         "AGP" = list(
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
# plotterXML <- PlotLogRatesXML$new(path_to_xml = "path/to/xml", jobs = NULL, attributes = list(), burnIn = 0.1)
# resultXML <- plotterXML$run()
# 
# # Log case
# plotterLog <- PlotLogRatesLog$new(path_to_log = "path/to/log", cols = c())
# resultLog <- plotterLog$run()
