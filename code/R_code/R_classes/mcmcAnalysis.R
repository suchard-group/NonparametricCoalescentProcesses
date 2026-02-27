library(R6)
McmcAnalyser <-
  R6Class("McmcAnalyser",
          public = list(
            data = NULL,
            path = NULL,
            jobName = NULL,
            attributes = list(),
            # plottingFlags = list(),
            actions = list(),
            path_to_job = NULL,
            burnIn = 0.3,
            burnOut = 1,
            thinning = 1,

            initialize = function(data, path = NULL, jobName = NULL, 
                                  attributes = list(), actions = list(), 
                                  burnIn = 0.3, thinning = 1, burnOut = 1) {
              self$data <- data
              self$path <- path # todo this is not used
              self$jobName <- jobName # todo this is not used
              self$attributes <- attributes
              self$actions <- actions
              self$burnIn <- burnIn
              self$burnOut <- burnOut
              self$thinning <- thinning

              # if (!is.null(self$jobName)) {
              #   self$path_to_job <- file.path(self$path, self$jobName)
              # } else {
              #   self$jobName <- "Job1"  # Default job name
              #   self$path_to_job <- dirname(self$path)
              # }
            },
            
            getData = function() {
              return(self$data)
            },
            
            getColumns = function() {
              return(names(self$data))
            },

            thinAndBurn = function(data, burnIn, thinning) {
              if ( burnIn < 1 ) burnIn <- burnIn*nrow(data)
              data <- data %>%
                filter(row_number() > burnIn ) %>%
                filter(row_number() %% thinning == 0)
              return(data)
            },

            # ESS = function() { #, burnIn, thinning) {
            #   #data <- self$thinAndBurn(data, burnIn, thinning)
            #   ess <- coda::effectiveSize(self$data)
            #   return(ess)
            # },
            # 
            ESS = function() {
              ess <- numeric(ncol(self$data))
              names(ess) <- colnames(self$data)
              
              for (i in seq_along(ess)) {
                column_data <- self$data[, i]
                # Check for zero variance (ignoring NA)
                if (var(column_data, na.rm = TRUE) == 0 || all(is.na(column_data))) {
                  ess[i] <- 0
                } else {
                  ess[i] <- tryCatch(
                    coda::effectiveSize(column_data),
                    error = function(e) 0
                  )
                }
              }
              return(ess)
            },
            
            ESSSorted = function() {
              ess <- self$ESS()
              ess_df <- data.frame(Parameter = names(ess), ESS = as.numeric(ess))
              ess_df$ESS[ess_df$Parameter == "state"] <- - nrow(self$data)
              ess_sorted <- ess_df[order(ess_df$ESS, decreasing = FALSE), ]
              # ess_sorted <- ess_df[order(ess_df$ESS), ]
              # ess <- ess[order(ess, decreasing = TRUE)]
              return(ess_sorted)
            },

            summariesMcmc = function() {
              sapply(self$data, function(x) c(mean = mean(x), sd = sd(x), median = median(x))) %>%
                t() %>% round(4)
            },

            bigSummMcmc = function(ncols = 3, n_lags = 30) {
              mcmc_data <- coda::as.mcmc(self$data)
              summary <- summary(mcmc_data)
              #sapply(1:(maxCol-minCol), function(i) acf(data[,i]))
              (traces <- plottingTraces(ncols)) %>% print()
              (acfs <- plottingAcf(ncols, n_lags)) %>% print()
              return(list(summary=summary, traces=traces, acfs=acfs))
            },

            plottingAcf = function(n_cols=3, n_lags=30) {
              if (n_lags > 51) print("number of lags cannot be higher than 50") # due to function "acf"
              n_lags <- min(n_lags,nrow(self$data)-1)
              dataPlot <- tibble(lags = 0:(n_lags-1))
              for (i in 1:ncol(self$data)) {
                nameVar <- names(self$data)[i]
                valuesAcf <- acf(self$data[[i]], plot=FALSE)$acf[1:n_lags]
                dataPlot <- dataPlot %>% add_column(!!nameVar := valuesAcf)
              }
              dataLong <- dataPlot %>%
                pivot_longer(-lags, names_to = "variables", values_to = "acf")
              ggplot(dataLong, aes(x = lags, y = acf)) +
                geom_bar(stat = "identity", width = 0.05) + #fill = "skyblue"
                geom_hline(yintercept = 0, color = "black") +
                geom_hline(yintercept = 1.96/sqrt(n_lags), color = "blue", linetype = "dashed") +
                geom_hline(yintercept = -1.96/sqrt(n_lags), color = "blue", linetype = "dashed") +
                labs(title = "Autocorrelation Functions", x = "Lags", y = "Autocorrelation") +
                facet_wrap(~variables,  ncol=2) +
                theme_minimal() +
                theme(plot.title = element_text(face = "bold", size = 17, hjust = 0.45),
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.border = element_rect(color = "black", fill = NA, size = 1))
            },

            plottingOneTrace = function(var, nameVar) { # just for one
              dataPlot <- tibble(iterations=1:length(var), y = var)
              plot <- ggplot(dataPlot, aes(x = iterations, y = y)) +
                geom_line() + theme_minimal() +
                labs(title=paste0(nameVar), x = "iterations", y="") +
                theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
              return(plot)
            },

            plottingTraces = function(ncol = 3, cummean = FALSE, cols = NULL) {
              ncol <- min(ncol, ncol(self$data))
              if (is.null(cols)) {
                cols <- names(self$data)[1:ncol(self$data)]
              } else {
                cols <- names(self$data)[cols]
              }
              # print(cols)
              dataLong <- self$data %>% 
                dplyr::select(cols) %>%
                add_column(iterations = 1:nrow(self$data), .before = 1) %>%
                pivot_longer(-iterations, names_to = "variables", values_to = "value")
              if (cummean) {
                dataLong <- dataLong %>%
                  group_by(variables) %>%
                  mutate(reverse_cumulative_mean = cumsum(value) / seq_along(value))
              }
              plot <- ggplot(dataLong, aes(x=iterations, y=value))+
                geom_line() +
                facet_wrap(~variables,  ncol = ncol) +
                labs(title="Trace Plots") +
                theme(plot.title = element_text(face = "bold", size = 17, hjust = 0.5))

              if (cummean) {
                plot <- plot + geom_line(aes(y = reverse_cumulative_mean), color = "red", linetype = "dashed")
              }
              return(plot)
            }

            # inputsSetter = function() { # add more general details?
            #   stop("This method should be implemented in a subclass.")
            # },

          )
)