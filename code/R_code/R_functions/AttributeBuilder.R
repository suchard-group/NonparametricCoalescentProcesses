# attributes Builders

# attributesNPRates <- function(x) {
#   attr(x, "class") <- c("NPRates", class(x))
#   x
# }
  
#building attributes list from underscore-separated list
create_attributesSkygrid <- function(input_string, simulation = "off") {
  values <- strsplit(input_string, "_")[[1]]
  
  # Define the allowed values for each key
  jobs_allowed_values <- list(
      "covariate" = c("Temperature"), 
      "scale_shape" = c("NUMBER"),
      "scale_scale" = c("NUMBER"),
      "length_shape" = c("NUMBER"),
      "length_scale" = c("NUMBER"),
  )
  additional_allowed_values <- list(
    "tree" = c("YF")
  )
  
  # Define the keys in order and their positions in the string
    jobsKeys <- names(jobs_allowed_values)
    #additionalKeys <- names(additional_allowed_values)
    njk <- length(jobsKeys)
    #aak <- length(additionalKeys)
    jobsAttributes <- checkingAndBuildingAttributes(values[1:njk], jobsKeys, jobs_allowed_values)
    # additionalAttributes <- 
    #   checkingAndBuildingAttributes(values[(njk + 1):(njk + aak)], additionalKeys, additional_allowed_values)
    additionalAttributes <- list("tree" = "rabies",
                                 "dataType" = "RealData", 
                                 "msim" = "lrm", 
                                 "predictorFun" = "Equal", 
                                 "align_length" = 0, 
                                 "nstates" = as.vector(17))
    return(list(jobsAttributes, additionalAttributes))
}


#building attributes list from underscore-separated list
create_attributesNPRates <- function(input_string, simulation = "off") {
  values <- strsplit(input_string, "_")[[1]]
  
  # Define the allowed values for each key
  jobs_allowed_values <- list(
    "mana" = c("Glm", "Gp"),
    "kernel" = c("D", "E", "L", "LE"),
    "hyper" = c("hyperOn", "hyperOff"),
    "predictorName" = c("RangeOverlap", "HostDistance", "HostDistance2",
                        "RoostOverlap", "BodySize", "WingAspectRatio", 
                        "WingLoading", "L1Circle", "L2Circle"),
    "expected_changes" = c("NUMBERRANDOM"),
    "operator_logRates" = c("Adapt", "HMC"),
    "norm" = c("normOff", "normOn"),
    "SRF" = c("SRFFalse", "SRFTrue"),
    "FM" = c("Uniform", "Empirical")
  )
  additional_allowed_values <- list(
    "tree" = c("rabies", "primates"),
    "dataType" = c("Alph", "RealData"), # can be defaulted
    "msim" = c("lrm", "Glm"), # not relevant for sim off
    "predictorFun" = c("Equal", "Exp"), # not relevant for sim off
    "align_length" = c("NUMBER"), # not relevant for sim off
    "nstates" = c("NUMBER")
  )
  
  # Define the keys in order and their positions in the string
  if (simulation == "on"){ 
    allowed_values <- append(jobs_allowed_values, additional_allowed_values)
    keys <- names(allowed_values)
    attributes <- checkingAndBuildingAttributes(values, keys, allowed_values)
    return(attributes)
  } else if (simulation == "off") {
    # TODO THIS CASE MUST BE CORRECTED
    jobsKeys <- names(jobs_allowed_values)
    #additionalKeys <- names(additional_allowed_values)
    njk <- length(jobsKeys)
    #aak <- length(additionalKeys)
    jobsAttributes <- checkingAndBuildingAttributes(values[1:njk], jobsKeys, jobs_allowed_values)
    # additionalAttributes <- 
    #   checkingAndBuildingAttributes(values[(njk + 1):(njk + aak)], additionalKeys, additional_allowed_values)
    additionalAttributes <- list("tree" = "rabies",
                                 "dataType" = "RealData", 
                                 "msim" = "lrm", 
                                 "predictorFun" = "Equal", 
                                 "align_length" = 0, 
                                 "nstates" = as.vector(17))
    return(list(jobsAttributes, additionalAttributes))
  } else {
    stop("Invalid value for simulation")
    }
}


checkingAndBuildingAttributes <- function(values, keys, allowed_values) {
  attributes <- list()
  for (i in seq_along(keys)) {
    key <- keys[i]
    value <- values[i]
    
    if  ("NUMBER" %in% allowed_values[[key]]) {
      numeric_value <- as.numeric(value)
      if (is.na(numeric_value)) {
        stop(paste("Invalid numeric value for", key, ":", value))
      }
      attributes[[key]] <- numeric_value
    } else if ("NUMBERRANDOM" %in% allowed_values[[key]]) {
      cat(key, " is set to be random: ", value, "\n")
      attributes[[key]] <- value
    } else {
      if (!value %in% allowed_values[[key]]) {
        stop(paste("Invalid string value for", key, ":", value))
      }
      attributes[[key]] <- value
    }
  }
  return(attributes)
}
# 
# # Example usage
# input_string <- "Glm_D_hyperOn_RangeOverlap_0.001_Adapt_normOff_SRFFalse_Uniform_rabies_Alph_lrm_Equal_3_17"
# attributes <- create_attributesNPRates(input_string, "off")
# 
# # Print the result
# print(attributes)

