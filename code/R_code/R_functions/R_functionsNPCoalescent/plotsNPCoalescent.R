findInversionPoints <- function(median, xAxisVariable) {
  # median <- PJL$getData(1)[grepl("gmrf.mean", colnames(PJL$getData(1)))]  |> apply(2, median) 
  # xAxisVariable <- PJL$getDataToPlot(1)$loadCovariate()
  findInversionPointsData <- tibble(median = median, xAxisVariable = xAxisVariable) |> 
    arrange(xAxisVariable)
  findInversionPointsData <- findInversionPointsData %>%
    mutate(
      median_smooth = predict(mgcv::gam(median ~ s(xAxisVariable, bs = "cs")), newdata = findInversionPointsData),
    )
  findInversionPointsData$diffs <- c(0, diff(findInversionPointsData$median_smooth))
  findInversionPointsData$signs <- sign(findInversionPointsData$diffs)
  findInversionPointsData$signChange <- c(0, diff(findInversionPointsData$signs))
  inversionPoints <- filter(findInversionPointsData, abs(signChange) == 2)[["xAxisVariable"]]
  return(unname(inversionPoints))
}


findInflectionPointsTRUE <- function(median, xAxisVariable) {
  findInversionPointsData <- tibble(median = median, xAxisVariable = xAxisVariable) |> 
    arrange(xAxisVariable)
  findInversionPointsData <- findInversionPointsData %>%
    mutate(
      median_smooth = predict(mgcv::gam(median ~ s(xAxisVariable, bs = "cs")),
                              newdata = findInversionPointsData)
    )
  
  ## --- REPLACE the second-difference block with this (handles non-uniform x) ---
  x <- findInversionPointsData$xAxisVariable
  y <- findInversionPointsData$median_smooth
  h  <- diff(x)                              # uneven spacings
  d1 <- diff(y) / h                          # first derivative on segments
  f2_inner <- 2 * diff(d1) / (h[-1] + h[-length(h)])  # non-uniform 2nd deriv.
  findInversionPointsData$diffs <- c(0, f2_inner, 0)  # pad ends
  
  # robust small-curvature suppression (scale by MAD, not by range)
  # eps <- max(1e-2 * diff(range(y)), 3 * stats::mad(findInversionPointsData$diffs, constant = 1))
  # findInversionPointsData$diffs[abs(findInversionPointsData$diffs) < eps] <- 0
  ## ---------------------------------------------------------------------------
  
  findInversionPointsData$signs <- sign(findInversionPointsData$diffs)
  findInversionPointsData$signChange <- c(0, diff(findInversionPointsData$signs))
  inversionPoints <- dplyr::filter(findInversionPointsData, abs(signChange) == 2)[["xAxisVariable"]]
  
  ## Optional: thin detections if they’re clustered in dense x regions
  if (length(inversionPoints) > 1L) {
    min_sep <- 0.05 * diff(range(x))        # e.g., 5% of x-range
    inversionPoints <- inversionPoints[c(TRUE, diff(inversionPoints) > min_sep)]
  }
  
  return(unname(inversionPoints))
}

findInflectionPoints <- function(median, xAxisVariable, eps_factor = 0.05, min_run = 4) {
    library(dplyr)
    library(mgcv)
    
    # Prepare and smooth
    dat <- tibble(median = median, x = xAxisVariable) |> arrange(x)
    dat <- dat |> mutate(
      median_smooth = predict(mgcv::gam(median ~ s(x, bs = "cs")), newdata = dat)
    )
    
    # Compute first derivative accounting for uneven x spacing
    dx <- diff(dat$x)
    dy <- diff(dat$median_smooth)
    slope <- dy / dx
    slope <- c(slope[1], slope)  # pad to match length
    
    dat$slope <- slope
    
    # ---- FIXED: compute eps after smoothing, within same environment ----
    eps <- eps_factor * diff(range(dat$median_smooth))
    eps <- max(eps, 3 * stats::mad(dat$slope, constant = 1))
    # ---------------------------------------------------------------------
    
    # Detect consecutive near-zero slopes
    flat <- abs(dat$slope) < eps
    r <- rle(flat)
    
    # find indices of runs of near-flat slopes
    run_ends <- cumsum(r$lengths)
    long_runs <- which(r$values & r$lengths >= min_run)
    flatten_x <- numeric(0)
    if (length(long_runs) > 0) {
      for (i in long_runs) {
        start <- if (i == 1) 1 else run_ends[i - 1] + 1
        end <- run_ends[i]
        # take midpoint of the flat segment as the "flattening point"
        flatten_x <- c(flatten_x, mean(dat$x[c(start, end)]))
      }
    }
    
    return(unname(flatten_x))
  }
  




# findInflectionPoints <- function(median, xAxisVariable) {
#   # median <- PJL$getData(1)[grepl("gmrf.mean", colnames(PJL$getData(1)))]  |> apply(2, median) 
#   # xAxisVariable <- PJL$getDataToPlot(1)$loadCovariate()
#   
#   # Create ordered tibble
#   findInflectionPointsData <- tibble(median = median, xAxisVariable = xAxisVariable) |> 
#     arrange(xAxisVariable)
#   
#   # Fit a smooth GAM
#   gam_model <- mgcv::gam(median ~ s(xAxisVariable, bs = "cs"))
#   
#   # Predict first and second derivatives
#   # The 'pderiv' argument is used in mgcv::predict.gam to get derivatives (requires mgcv >= 1.9)
#   # Alternatively, use finite differences for the second derivative:
#   findInflectionPointsData <- findInflectionPointsData %>%
#     mutate(
#       median_smooth = predict(gam_model, newdata = findInflectionPointsData)
#     )
#   
#   # Second derivative via discrete approximation
#   diffs1 <- diff(findInflectionPointsData$median_smooth)
#   diffs2 <- diff(diffs1)  # second derivative (approx)
#   
#   # Center the second derivative vector
#   second_derivative <- c(NA, diffs2, NA)
#   
#   findInflectionPointsData$second_derivative <- second_derivative
#   
#   # Detect zero crossings in the second derivative
#   signs <- sign(findInflectionPointsData$second_derivative)
#   signChange <- c(0, diff(signs))
#   
#   # Inflection points occur where sign of curvature changes
#   inflectionPoints <- findInflectionPointsData %>%
#     filter(abs(signChange) == 2) %>%
#     pull(xAxisVariable)
#   
#   return(unname(inflectionPoints))
# }



plotDemographicQuantiles <- function(summStat, xAxisVariable, xAxisVariableName, jobName, attributes, actions) {
  
  summaryParameter <- summStat |> row.names() |> 
    sapply(function(x) grepl("PopSize", x)) %>% summStat[.,] |> as_tibble()
  mean <- mean(c(summaryParameter$median, summaryParameter$q05, summaryParameter$q95))
  sd <- sd(c(summaryParameter$median, summaryParameter$q05, summaryParameter$q95))

  summaryParameter$median <- (summaryParameter$median - mean) / sd
  summaryParameter$q05 <- (summaryParameter$q05 - mean) / sd
  summaryParameter$q95 <- (summaryParameter$q95 - mean) / sd

  # lmod <- lm(median ~ predictor, summaryLogRates)
  summaryFull <- cbind(xAxisVariable, summaryParameter) |> as_tibble()
  if(attributes$tree == "Hiv") {
    
    summaryFull <- summaryFull |>  slice_head(n = 15)
   
    slopes <-  c(2.087457,  8.810495, 14.634277)  ## TODO this was done manually
    middleNumber <- 6
    # intercepts <- summaryFull$median[6] - slopes * summaryFull$xAxisVariable[6]
    
    bbb <- lm(median ~ xAxisVariable, data = summaryFull)$fitted.values
    bbb <- unname(bbb)
    # print(lm(median ~ xAxisVariable, data = summaryFull))
      
  } else if (attributes$tree == "Mox") {
    slopes <-  c(0,  8.810495, 0)  ## TODO this was done manually
    slopes <- c(-0.5181557, -0.1145099, 0.3778857)
    middleNumber <- nrow(summaryFull) %/% 2 + 1

  } else if (attributes$tree == "YF") {
    slopes <-  c(-1.5218159, 0.2369071,  2.1587352)  ## TODO this was done manually WITH FULL DATA! DO AGAIN
    middleNumber <- nrow(summaryFull) %/% 2 + 1 - 6
    # inversionPoints <- findInversionPoints(summaryFull$median, summaryFull$xAxisVariable)
    inversionPoints <- findInflectionPoints(summaryFull$median, summaryFull$xAxisVariable)
    cat("The inversion points are:")
    cat(inversionPoints)
  }
  
  intercepts <- summaryFull$median[middleNumber] - slopes * summaryFull$xAxisVariable[middleNumber]
  summaryFull <- summaryFull |>
    mutate(linearModelMedian = slopes[2]*xAxisVariable + intercepts[2])   |> 
    mutate(linearModelLow = slopes[1]*xAxisVariable + intercepts[1])   |>
    mutate(linearModelHigh = slopes[3]*xAxisVariable + intercepts[3])
  if (!("smoothing" %in% names(attributes)) || attributes$smoothing == "true") {
    # Default (or explicitly true): apply smoothing
    summaryFull <- summaryFull %>%
      mutate(
        q05_smooth = predict(mgcv::gam(q05 ~ s(xAxisVariable, bs = "cs")), newdata = summaryFull),
        q95_smooth = predict(mgcv::gam(q95 ~ s(xAxisVariable, bs = "cs")), newdata = summaryFull)
      )
  } else {
    # Smoothing explicitly set to something other than "true"
    summaryFull <- summaryFull %>%
      mutate(
        q05_smooth = q05,
        q95_smooth = q95
      )
  }
  
  plotParameter <- ggplot(summaryFull, aes(x = xAxisVariable)) + 
    # geom_errorbar(aes(ymin = q25, ymax = q75, color = "darkgreen"), width = 0.05) +
    # geom_errorbar(aes(ymin = q05, ymax = q95, color = "green"), width = 0.1) +
    
    # geom_point(aes(y = median, color = "PopSize"), size = 0.7) + 
    # geom_smooth(aes(y = median, color = "PopSize"), se = FALSE, linewidth = 1.5) + # method = "loess", 
    
    geom_line(aes(y = median, color = "PopSize", linetype = "PopSize"), linewidth = 0.5)  +
    geom_ribbon(aes(ymin = q05_smooth, ymax = q95_smooth), fill = "darkgoldenrod1", alpha = 0.5) +
    # geom_line(aes(y = linearModelHigh)) +
    # geom_line(aes(y = linearModelLow)) +
    # geom_ribbon(aes(ymin = min(linearModelLow, linearModelHigh), ymax = max(linearModelLow, linearModelHigh)), fill = "blue", alpha = 0.5) +
    # geom_line(aes(y = linearModel2)) +
    # geom_ribbon(aes(ymin = q05, ymax = q95, fill="errorInterval"), alpha = 0.5) +
    theme_minimal()
  
  if (attributes$tree == "YF" & attributes$simulation != "on") {
    x_range <- range(summaryFull$xAxisVariable, na.rm = TRUE)
    pretty_breaks <- scales::breaks_pretty(n = 5)(x_range)
    all_breaks <- sort(unique(c(pretty_breaks, inversionPoints)))
  
    plotParameter <- plotParameter + 
      geom_vline(xintercept = inversionPoints, linetype = "solid", color = "black", linewidth = 0.3) 
    #   geom_rect(data = tibble(xmin = -Inf, xmax = inversionPoints[1], ymin = -Inf, ymax = Inf),
    #             aes(xmin = -Inf, xmax = inversionPoints[1], ymin = -Inf, ymax = Inf),
    #             fill = "#2171B5", alpha = 0.2, inherit.aes = FALSE) + 
    #   geom_rect(data = tibble(xmin = inversionPoints[2], xmax = Inf, ymin = -Inf, ymax = Inf),
    #             aes(xmin = inversionPoints[2], xmax = Inf, ymin = -Inf, ymax = Inf),
    #             fill = "#2171B5", alpha = 0.2, inherit.aes = FALSE) + 
    #   # geom_ribbon(aes(ymin = q05_smooth, ymax = q95_smooth), fill = "white", alpha = 1)  +
    # geom_line(aes(y = median, color = "PopSize", linetype = "PopSize"), linewidth = 0.5)  +
    #   geom_ribbon(aes(ymin = q05_smooth, ymax = q95_smooth), fill = "darkgoldenrod1", alpha = 0.5)
      # scale_x_continuous(
      #   breaks = all_breaks,
      #   labels = function(x) round(x, 2)
      # ) 
  } 
  
  plotParameter <- plotParameter + 
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 5),
      labels = function(x) x
    ) 
  
  if (attributes$simulation == "on") {
    if (attributes$curve == "Linear") {
      trueline <- xAxisVariable
    } else if (attributes$curve == "Concave") {
      trueline <- -8/7 * xAxisVariable^2 + 1/7 * xAxisVariable + 30/7
      trueline <- (trueline - mean(trueline)) / sd(trueline)
    } else {
      stop("True line not implemented")
    }
    
    plotParameter <- plotParameter +
      geom_line(aes(y = trueline, color = "ATrueValues", linetype = "ATrueValues"), linewidth = 0.5) +
      labs(x = paste0(xAxisVariableName), y = expression(logN[e](t))) + 
      scale_color_manual(
        name = "",
        values = c("ATrueValues"="#2171B5", "PopSize"="#F8766D"), # remember to write them in alphabetical order
        labels = c("True values", "GP") # and match the labels appropriately
    ) + 
    scale_linetype_manual(
      name = "",
      values = c("ATrueValues"="12", "PopSize"="solid"), # remember to write them in alphabetical order
      labels = c("True values", "GP") # and match the labels appropriately
    )  
    # theme(legend.margin = margin(-30, 6, 0, 6))
  } else {
    plotParameter <- plotParameter + 
      # geom_ribbon(aes(ymin = linearModelLow, ymax = linearModelHigh), fill = "darkgreen", alpha = 0.2) +
      geom_line(aes(y = linearModelMedian, color= "Linear", , linetype = "Linear")) +
      labs(x = paste0(xAxisVariableName), y = expression(logN[e](t))) + 
      scale_color_manual(
        name = "",
        values = c("Linear" = "#2171B5", "PopSize" = "#F8766D"), # Assign same labels
        labels = c("LL", "GP")
      ) +
      scale_linetype_manual(
        name = "",
        values = c("Linear" = "dashed", "PopSize" = "solid"), # Assign same labels
        labels = c("LL", "GP")
      ) 
      
      # scale_fill_manual(
      #   name = "",
      #   values = c("errorInterval" = "darkgoldenrod1"), # Define fill aesthetics separately
      #   labels = c("95% HDI")
      # )  + 
      # guides(
      #   linetype = guide_legend(order = 2),   # "95% HDI" will appear second
      #   color = guide_legend(order = 1)   # "Median" will appear first
      # )
      # theme(
      #   legend.spacing.x = unit(0.5, "cm")  # Adjust vertical spacing
      # )
      # scale_fill_manual(
      #   name = "Legend",  # Same legend name to merge both
      #   values = c("errorInterval" = "darkgoldenrod1", "PopSize" = "#F8766D"),
      #   labels = c("95% HDI", "Median")
      # )
      # scale_color_manual(
      #   name = "Legend",
      #   # values = c("OtherVar" = "#00BFC4", "PopSize"="#F8766D", "PopSizeQ"="red"), # remember to write them in alphabetical order
      #   values = c("PopSize"="#F8766D"), # remember to write them in alphabetical order
      #   labels = c("Median")
      #   ) + # and match the labels appropriately
      #  scale_fill_manual(
      #     name = "Legend",
      #     values = c("errorInterval" = "darkgoldenrod1"), # Define fill aesthetics separately
      #     labels = c("95% HDI")
      #   )
  }
  
  if (actions$publicationQuality) {
    fontSize <- 12
    fontSizeSmall <- 10
    fontType <- "serif"
    
    plotParameter <- plotParameter + theme_classic() +
      theme(legend.position = "none",
            legend.background = element_rect(fill = NA, color = NA),
            legend.key = element_rect(fill = NA, color = NA),
            legend.box.background = element_rect(fill = NA, linewidth = 0.1, linetype = "solid", color = NA),
            legend.margin = margin(-30, 2, 0, 6),
            # legend.box.margin = margin(-50, 6, 0, 6),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # frame around plot)
            panel.grid.major.y = element_line(color = "grey90", linetype = "33", size = 0.3),
            panel.grid.minor.y = element_blank(),  # optional: remove minor grid lines
            panel.grid.major.x = element_line(color = "grey90", linetype = "33", size = 0.3),
            # panel.grid.major.x = element_blank(),  # optional: remove vertical lines if you want
            axis.ticks.x = element_line(size = 0.3),
            axis.ticks.y = element_line(size = 0.3),
            axis.line = element_blank()
      ) +
        # panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        
        # legend.box.just = "right",
        # legend.key.width = unit(0.6, "lines"),
        # legend.spacing.y = unit(0, "cm"),          # reduce space between legend items
        # legend.key.height = unit(0.3, "cm"),        # reduce height of each legend key (line)
        # # legend.margin = margin(0, 0, 0, 0),
        # # legend.margin = margin(6, 6, 6, 6),
        # legend.text = element_text(size = fontSizeSmall, family = fontType, color = "black"),
        # # legend.title = element_text(size = fontSizeSmall, family = fontType, color = "black", face = "bold"),
        # legend.title=element_blank(),
        # legend.background = element_rect(
        #   fill = alpha("white", 0))  # Semi-transparent white background
        # color = "black",             # Border color
        # size = 0.5)                   # Border thickness
      theme(text = element_text(size = fontSize, family = fontType, color = "black"),  # Set base text size
            axis.text = element_text(size = fontSizeSmall, family = fontType, color = "black"),  # Axis tick labels
            axis.title = element_text(size = fontSize, family = fontType, color = "black")
      )
    

    # plotParameter <- plotParameter +
    #                 theme(legend.position = "bottom", 
    #                       legend.direction = "horizontal",
    #                       legend.text = element_text(size = fontSize, family = fontType, color = "black"),
    #                       legend.title = element_text(size = fontSize, family = fontType, color = "black"),
    #                       legend.key.size = unit(2, 'cm'),
    #                       plot.margin = margin(1, 1, 2, 1, "cm"),  # Increase bottom margin for legend
    #                       legend.box.spacing = unit(1, "cm")
    # ) +
    #   theme(text = element_text(size = fontSize, family = fontType, color = "black"),  # Set base text size
    #         axis.text = element_text(size = fontSize, family = fontType, color = "black"),  # Axis tick labels
    #         axis.title = element_text(size = fontSize, family = fontType, color = "black")
    #   )
    #   
  }
  
  if (!actions$publicationQuality) {
    plotParameter <- plotParameter + labs(title = paste0(jobName)) + 
      theme(legend.position = "none")
      # theme(legend.text = element_text(size = 20), 
      #       legend.title = element_text(size = 20), 
      #       legend.key.size = unit(2, 'cm')) 
  }
  
  return(plotParameter)
}




# ------ TIME + POP + COVARIATE ------
plotDemographicGridCovariateQuantiles <- function(summStat, xAxisVariable, xAxisVariableName,
                                                  otherYAxisVariable, otherYAxisVariableName, jobName,
                                                  attributes, actions) {
  if(actions$publicationQuality) {
    fontSize <- 30
    fontType <- "serif"
  } else {
    fontSize <- 15
    fontType <- "serif"
  }
  
  
  summaryParameter <- summStat |> row.names() |>
    sapply(function(x) grepl("PopSize", x)) %>% summStat[.,] |> as_tibble()
  # summaryParameter$median <- (summaryParameter$median - mean(summaryParameter$median)) / sd(summaryParameter$median)
  
  
  if(attributes$tree == "Hiv") {
    summaryParameter <- summaryParameter |> slice_head(n = 15)
    xAxisVariable <- xAxisVariable[1:15]
    otherYAxisVariable <- otherYAxisVariable[1:15]
  } 
  
  
  mean <- mean(c(summaryParameter$median, summaryParameter$q05, summaryParameter$q95))
  sd <- sd(c(summaryParameter$median, summaryParameter$q05, summaryParameter$q95))
  summaryParameter$median <- (summaryParameter$median - mean) / sd
  summaryParameter$q05 <- (summaryParameter$q05 - mean) / sd
  summaryParameter$q95 <- (summaryParameter$q95 - mean) / sd
  otherYAxisVariable <- otherYAxisVariable
  summaryFull <- cbind(xAxisVariable, summaryParameter, otherYAxisVariable) |> as_tibble()
  scale_factor <- max(summaryFull$median) / max(summaryFull$otherYAxisVariable)
  # summaryFull$label <- otherYAxisVariableName
  
  library(scales)
  summaryFull <- summaryFull |> arrange(xAxisVariable) 
  summaryFull$outOfRange <- FALSE
  summaryFull$outOfRange <- factor(summaryFull$outOfRange, levels = c(0, 1))
  if (!is.null(attributes$time_range_min) & !is.null(attributes$time_range_max)) {
    temp0 <- summaryFull |> filter(xAxisVariable == min(xAxisVariable)) |> 
      mutate(xAxisVariable = as.numeric(attributes$time_range_min))
     temp0$outOfRange <- TRUE
    tempMax <- summaryFull |> filter(xAxisVariable == max(xAxisVariable)) |> 
      mutate(xAxisVariable = as.numeric(attributes$time_range_max))
    tempMax$outOfRange <- TRUE
    summaryFull <- rbind(temp0, summaryFull) 
    summaryFull <- rbind(summaryFull, tempMax)
    xMAX <- tempMax$xAxisVariable[1]
    xMIN <- temp0$xAxisVariable[1]
  }
  # summaryFull <- summaryFull %>%
  #   mutate(time_start = xAxisVariable,
  #          time_end = lead(xAxisVariable)) %>%
  #   filter(!is.na(time_end))  # drop last row to avoid NA
  blue_white_red <- colorspace::divergingx_hcl(11, palette = "Temps")
  # ------ HIV -----
  if(attributes$tree == "Hiv") {
    
    summaryFull <- summaryFull |> slice_head(n = 15)
    deStandardize <- function(vector) {
      return(vector * sd + mean)
    }
    r2 <- max(summaryFull$otherYAxisVariable, na.rm = TRUE) 
    r1 <- min(summaryFull$otherYAxisVariable, na.rm = TRUE)
    
    l2 <- max( deStandardize(((otherYAxisVariable - mean(otherYAxisVariable))/sd(otherYAxisVariable))), na.rm = TRUE)
    l1 <- min( deStandardize(((otherYAxisVariable - mean(otherYAxisVariable))/sd(otherYAxisVariable))), na.rm = TRUE)
    
    cat("r1:", r1, "r2:", r2, "\n")
    cat("l1:", l1, "l2:", l2, "\n")
    a <- (r2 - r1) / (l2 - l1)
    b <- r1 - a * l1
    inv_a = 1 / a      # = -0.83333...
    inv_b = -b / a 
    
    
    
    
    plotParameter <- ggplot(summaryFull, aes(x = xAxisVariable)) +
      # geom_point(aes(y = deStandardize(median), color = "PopSize"), size = 3) +
      geom_line(aes(y =  deStandardize(median), color = "PopSize"), size = 0.5) +
      geom_ribbon(aes(ymin = deStandardize(q05), ymax = deStandardize(q95)), fill = "darkgoldenrod1", alpha = 0.5) +
      geom_line(aes(y = deStandardize(((otherYAxisVariable - mean(otherYAxisVariable))/sd(otherYAxisVariable))), color = "OtherVar"), 
                    size = 0.5) +
      scale_y_continuous(
        name = expression(N[e](t)), 
        breaks = scales::breaks_pretty(n = 3),
        labels = function(x) parse(text = paste0("10^", x)),
        sec.axis = sec_axis(~ a * . + b, name = otherYAxisVariableName)
      ) +
      scale_x_reverse(breaks = scales::breaks_pretty(n = 6),
        labels = function(x) 2005 - x
      ) +
      labs(x = paste0(xAxisVariableName)) +
      # labs(title = paste0(jobName), x = paste0(xAxisVariableName), y = "Pop. Sizes (quantiles)") +
      scale_color_manual(
        name = "Legend",
        # values = c("OtherVar" = "#00BFC4", "PopSize"="#F8766D", "PopSizeQ"="red"), # remember to write them in alphabetical order
        values = c("OtherVar" = "#2171B5", "PopSize"="#F8766D", "PopSizeQ"="red"), # remember to write them in alphabetical order
        labels = c(paste0(otherYAxisVariableName), expression(N[e](t)), "PopSizeQ") # and match the labels appropriately
      ) +
      # theme_minimal() + 
      # theme(legend.position = "bottom") +
      # labs(x = "Time", y = expression(N[e](t))) +
      theme_classic() +
      theme(panel.grid.major.x = element_line(color = "grey90", linetype = "dashed", size=0.3), 
            panel.grid.major.y = element_line(color = "grey90", linetype = "dashed", size=0.3), 
            axis.line.y =  element_blank(), 
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
            legend.position = c(0.5, 0.165), 
            legend.title = element_blank(),
            panel.background = element_rect(fill = NA, colour = NA),
            legend.background =  element_rect(fill = NA, colour = NA)) 
  }
  
  # ------ MOX -----
  if (attributes$tree == "Mox") {
    deStandardize <- function(vector) {
      return(vector * sd + mean)
    }
    summaryFull <- summaryFull |> mutate(symmetricOtherVar = - (otherYAxisVariable - mean(otherYAxisVariable)) / sd(otherYAxisVariable))
    summaryFull <- summaryFull |> mutate(symmetricOtherVar = deStandardize(symmetricOtherVar))
    summaryFull$xAxisVariable <- -summaryFull$xAxisVariable
    r2 <- -min(summaryFull$otherYAxisVariable, na.rm = TRUE)
    r1 <- -max(summaryFull$otherYAxisVariable, na.rm = TRUE)

    l2 <- max(summaryFull$symmetricOtherVar, na.rm = TRUE)
    l1 <- min(summaryFull$symmetricOtherVar, na.rm = TRUE)
    a <- (r2 - r1) / (l2 - l1)
    b <- r1 - a * l1
    inv_a = 1 / a      # = -0.83333...
    inv_b = -b / a 

    # temp0 <-   summaryFull |> filter(xAxisVariable == min(xAxisVariable)) |> mutate(xAxisVariable = 0)
    # print(summaryFull$xAxisVariable)
    # tempMax <- summaryFull |> filter(xAxisVariable == max(xAxisVariable)) |> mutate(xAxisVariable = 150000)
    # print(temp0)
    # print(tempMax)
    # summaryFull <- rbind(temp0, summaryFull) 
    # summaryFull <- rbind(tempMax, summaryFull)
    nROWS <- nrow(summaryFull)
    t_min_inputted <- summaryFull[["xAxisVariable"]][nROWS] 
    t_max_inputted <- summaryFull[["xAxisVariable"]][nROWS-1] 
    inputted_value <- deStandardize(summaryFull[["median"]])[nROWS]
    inputted_value_otherVar <- summaryFull[["symmetricOtherVar"]][nROWS]
    lineSize <- 0.7
    plotParameter <- ggplot(summaryFull, aes(x = xAxisVariable, y = deStandardize(median)) ) +
      
      geom_stepribbon(aes(ymin = deStandardize(q05), ymax = deStandardize(q95)), fill = "darkgoldenrod1", alpha = 0.5, direction = "hv") +
      geom_step(aes(color= "APopSize"), size = lineSize, direction = "hv") +
      geom_step(aes(color= "OtherVar", y = symmetricOtherVar), size = lineSize, direction = "hv") +
      annotate("line", x = c(t_min_inputted, t_max_inputted), 
               y = inputted_value, color = "white", size = lineSize, linetype = "dotted") +
      annotate("line", x = c(t_min_inputted, t_max_inputted), 
               y = inputted_value, color = "darkgoldenrod1", size = lineSize, alpha = 0.5, linetype = "dotted") +
      annotate("line", x = c(t_min_inputted, t_max_inputted), 
               y = inputted_value_otherVar, color = "white", size = lineSize, linetype = "dotted") +
      annotate("line", x = c(t_min_inputted, t_max_inputted), 
               y = inputted_value_otherVar, color = "darkgoldenrod1", size = lineSize, alpha = 0.5, linetype = "dotted") +
      
      # geom_ribbon(aes(ymin = deStandardize(q05), ymax = deStandardize(q95)), fill = "darkgoldenrod1", alpha = 0.5) +
      labs(x = "Time", y = expression(N[e](t))) +
      theme_classic() +
      theme(panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"), 
            axis.line.y =  element_blank(), 
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
            legend.position = c(0.74, 0.3), 
            panel.background = element_rect(fill = NA, colour = NA),
            legend.background =  element_rect(fill = NA, colour = NA)) +
      scale_y_continuous(
        name = expression(N[e](t)),
        breaks = scales::breaks_pretty(n = 4),
        labels = function(x) parse(text = paste0("10^", x)), 
        sec.axis = sec_axis(~ -a * . - b, name = expression(delta^18 * O))
      ) +
      scale_color_manual(
        name = "",
        values = c("APopSize"="#F8766D", "OtherVar" = "#2171B5"), 
        labels = c(expression(N[e](t)), expression(delta^18 * O))
      ) +
      # scale_x_reverse(
      scale_x_continuous(
        breaks = scales::breaks_pretty(n = 5),
        labels = function(x) label_comma()(-x)
      ) + 
      xlab("Radiocarbon years (Before Present)") 
    
    if(FALSE) {
      summaryFull <- summaryFull |> mutate(q05Transformed = a * q05 + b)
      summaryFull <- summaryFull |> mutate(q95Transformed = a * q95 + b)
      summaryFull <- summaryFull |> mutate(medianTransformed = a * median + b)
      summaryFull <- summaryFull |> mutate(symmetricOtherVarTransformed = a * symmetricOtherVar + b)
      
      plot_a <-  ggplot(summaryFull, aes(x = xAxisVariable, y = -medianTransformed)) +
        geom_line(color= "#2171B5", aes(y = -symmetricOtherVarTransformed), size=1, alpha=0.5) +
        geom_ribbon(aes(ymin = -q05Transformed, ymax = -q95Transformed), fill = "darkgoldenrod1", alpha = 0.5) +
        geom_line(color= "#F8766D", size=1) +
        
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              panel.grid.major.x = element_line(color = "grey80", linetype = "dashed")) +
        scale_y_reverse(
          name = "D18",
          sec.axis = sec_axis(~ -inv_a * . + inv_b, name = expression(N[e](t)))) +
        theme_classic() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              panel.grid.major.x = element_line(color = "grey80", linetype = "dashed")) 
      
      y_range <- plot_a$layout$panel_params[[1]]$y.range
      y_range <- c(-max(y_range), -min(y_range))  # Reverse the y-axis range
      
      # Plot B
      plot_b <- ggplot(summaryFull, aes(x = xAxisVariable, y = otherYAxisVariable)) +
        # geom_rect(data = shade_df, inherit.aes = FALSE,
        #           aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = ymax),
        #           fill = "#2171B5", alpha = 0.2) +
        geom_line(aes(y = otherYAxisVariable), color = "#2171B5", size=1) +
        labs(y = "D18", x = "Time") +
        theme_classic() + 
        theme(panel.grid.major.x = element_line(color = "grey80", linetype = "dashed")) +
        ylim(c(-45, -32.5))
      
      plotParameter <- plot_a / plot_b + plot_layout(ncol = 1, heights = c(1, 1))
    }
    
  } 
  # ------ YF ----
  if (attributes$tree == "YF") {
  summaryFull <- summaryFull |> mutate(xAxisVariable = ifelse(xAxisVariable == max(xAxisVariable),  2019.2, xAxisVariable))
  
  time <- summaryFull$xAxisVariable
  b <- summaryFull$otherYAxisVariable
  
  inversionPoints <- c(-1.633633, 0.6075834) # TODO
  
  # 2. Interpolate crossing times using approx
  # get_crossing <- function(i1, i2, invPoint) {
  #   if (any(is.na(c(b[i1], b[i2], time[i1], time[i2])))) return(NA)
  #   approx(x = b[c(i1, i2)], y = time[c(i1, i2)], xout = invPoint)$y
  # }

  # # 3. Find regions where b > inversion points
  # above <- b > inversionPoints[2]
  # regions <- list()
  # i <- 1
  # while (i <= length(above)) {
  #   if (above[i]) {
  #     start_idx <- i
  #     while (i < length(above) && above[i + 1]) {
  #       i <- i + 1
  #     }
  #     end_idx <- i
  #
  #     # Left boundary
  #     if (start_idx == 1 || !above[start_idx - 1]) {
  #       left_crossing <- if (start_idx > 1) get_crossing(start_idx - 1, start_idx, inversionPoints[2]) else time[start_idx]
  #     } else {
  #       left_crossing <- time[start_idx]
  #     }
  #
  #     # Right boundary
  #     if (end_idx == length(b) || !above[end_idx + 1]) {
  #       right_crossing <- if (end_idx < length(b)) get_crossing(end_idx, end_idx + 1, inversionPoints[2]) else time[end_idx]
  #     } else {
  #       right_crossing <- time[end_idx]
  #     }
  #
  #     # Only add if both limits are valid (non-NA)
  #     if (!is.na(left_crossing) && !is.na(right_crossing)) {
  #       regions[[length(regions) + 1]] <- data.frame(
  #         xmin = left_crossing,
  #         xmax = right_crossing,
  #         ymin = -Inf,
  #         ymax = Inf
  #       )
  #     }
  #   }
  #   i <- i + 1
  # }
  #
  # shade_df <- do.call(rbind, regions)
  # cat("Shade df:\n")
  # print(shade_df)
  
  
  find_regions <- function(data, threshold_column, threshold_value, is_above = TRUE) {
    time <- data$xAxisVariable
    b <- data[[threshold_column]]
    
    # Determine the logical condition based on 'is_above'
    if (is_above) {
      condition <- b > threshold_value
    } else {
      condition <- b < threshold_value
    }
    
    get_crossing <- function(i1, i2, threshold) {
      if (any(is.na(c(b[i1], b[i2], time[i1], time[i2])))) return(NA)
      approx(x = b[c(i1, i2)], y = time[c(i1, i2)], xout = threshold)$y
    }
    
    regions <- list()
    i <- 1
    while (i <= length(condition)) {
      if (condition[i]) {
        start_idx <- i
        while (i < length(condition) && condition[i + 1]) {
          i <- i + 1
        }
        end_idx <- i
        
        # Left boundary
        left_crossing <- if (start_idx == 1 || !condition[start_idx - 1]) {
          if (start_idx > 1) get_crossing(start_idx - 1, start_idx, threshold_value) else time[start_idx]
        } else {
          time[start_idx]
        }
        
        # Right boundary
        right_crossing <- if (end_idx == length(b) || !condition[end_idx + 1]) {
          if (end_idx < length(b)) get_crossing(end_idx, end_idx + 1, threshold_value) else time[end_idx]
        } else {
          time[end_idx]
        }
        
        # Only add if both limits are valid (non-NA)
        if (!is.na(left_crossing) && !is.na(right_crossing)) {
          regions[[length(regions) + 1]] <- data.frame(
            xmin = left_crossing,
            xmax = right_crossing,
            ymin = -Inf,
            ymax = Inf
          )
        }
      }
      i <- i + 1
    }
    
    final_regions <- do.call(rbind, regions)
    return(filter(final_regions, abs(xmin-xmax)>0.01))
    
  }
  # Computes a pointwise second derivative on possibly irregular grids
  .compute_second_derivative <- function(x, y) {
    n <- length(x)
    s2 <- rep(NA_real_, n)
    if (n < 3) return(s2)
    
    # Three-point, nonuniform formula for interior points
    for (i in 2:(n-1)) {
      x0 <- x[i-1]; x1 <- x[i]; x2 <- x[i+1]
      y0 <- y[i-1]; y1 <- y[i]; y2 <- y[i+1]
      h0 <- x1 - x0
      h1 <- x2 - x1
      if (is.na(x0) || is.na(x1) || is.na(x2) ||
          is.na(y0) || is.na(y1) || is.na(y2) ||
          h0 <= 0 || h1 <= 0) {
        s2[i] <- NA_real_
      } else {
        s2[i] <- 2 * ( (y0 / (h0*(h0+h1))) -
                         (y1 / (h0*h1)) +
                         (y2 / (h1*(h0+h1))) )
      }
    }
    s2
  }
  
  # Example usage for your two cases
  # Regions above inversionPoints[2]
  above_shade_df <- find_regions(
    data = summaryFull, 
    threshold_column = "otherYAxisVariable",
    threshold_value = inversionPoints[2],
    is_above = TRUE
  )
  
  # Regions below inversionPoints[1]
  below_shade_df <- find_regions(
    data = summaryFull,
    threshold_column = "otherYAxisVariable",
    threshold_value = inversionPoints[1],
    is_above = FALSE
  )
  
  # cat("Above shade df:\n")
  # print(above_shade_df)
  # cat("Below shade df:\n")
  # print(below_shade_df)
  
  shade_df <- above_shade_df
  
  
  
  firstRound <- TRUE
  for (i in c(2015, 2016, 2017)) {
    min_val <- summaryFull |> filter(xAxisVariable >= i-1 & xAxisVariable < i) |> 
      filter(otherYAxisVariable == min(otherYAxisVariable)) 
    if (firstRound) {
      min_x_coord <-  min_val$xAxisVariable[1]
      min_y_coord <- min_val$otherYAxisVariable[1] + 1
      firstRound <- FALSE
    } else {
      min_x_coord <- c(min_x_coord, min_val$xAxisVariable[1]) 
      min_y_coord <- c(min_y_coord, min_val$otherYAxisVariable[1] + 1)
    }
  }
  #max
  firstRound <- TRUE
  for (i in c(2015, 2016, 2017, 2018, 2019)) {
    max_val <- summaryFull |> filter(xAxisVariable >= i-1 & xAxisVariable < i) |> 
      filter(otherYAxisVariable == max(otherYAxisVariable)) 
    if (firstRound) {
      max_x_coord <-  max_val$xAxisVariable[1]
      max_y_coord <- max_val$otherYAxisVariable[1] + 1
      firstRound <- FALSE
    } else {
      max_x_coord <- c(max_x_coord, max_val$xAxisVariable[1]) 
      max_y_coord <- c(max_y_coord, max_val$otherYAxisVariable[1] + 1)
    }
  }
  t_min_inputted <- summaryFull[["xAxisVariable"]][1] 
  t_max_inputted <- summaryFull[["xAxisVariable"]][2] 
  inputted_value <- summaryFull[["median"]][1]
  # # Plot A
  plot_a <- ggplot(summaryFull, aes(x = xAxisVariable, y = median)) +
    geom_rect(data = shade_df, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "#2171B5", alpha = 0.2) +
    geom_rect(data = below_shade_df, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "#2171B5", alpha = 0.2) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 5),
      labels = function(x) x,
      expand = expansion(mult = 0, add = 0)
    ) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "darkgoldenrod1", alpha = 0.5) +
    geom_line(aes(color = outOfRange), color= "#F8766D", size=1) +
    annotate("line", x = c(t_min_inputted, t_max_inputted), 
             y = inputted_value, color = "white", size = 1, linetype = "dotted") +
    annotate("line", x = c(t_min_inputted, t_max_inputted), 
             y = inputted_value, color = "darkgoldenrod1", size = 1, alpha = 0.5, linetype = "dotted") +
    # scale_linetype_manual(values = c(0 = "dotted", 1 = "solid")) +
    labs(y = expression(logN[e](t))) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.line.x = element_blank(),
          panel.grid.major.x = element_line(color = "grey80", linetype = "dashed")) +
    coord_cartesian(xlim = c(t_min_inputted, xMAX))

  inputted_value <- summaryFull[["otherYAxisVariable"]][1]
  # Plot B
  plot_b <- ggplot(summaryFull, aes(x = xAxisVariable, y = otherYAxisVariable)) +
    geom_rect(data = shade_df, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = inversionPoints[2], ymax = ymax),
              fill = "#2171B5", alpha = 0.2) +
    geom_rect(data = below_shade_df, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = inversionPoints[2], ymax = ymax),
              fill = "#2171B5", alpha = 0.2) +
    geom_rect(data = below_shade_df, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = inversionPoints[1]),
              fill = "#2171B5", alpha = 0.2) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 5),
      labels = function(x) x,
      expand = expansion(mult = 0, add = 0)
    ) +
    geom_line(aes(y = inversionPoints[2]), size = 0.5, color = "black") +
    geom_line(aes(y = inversionPoints[1]), size = 0.5, color = "black") +
    geom_line(aes(y = otherYAxisVariable), color = "#2171B5", size=1) +
    annotate("line", x = c(t_min_inputted, t_max_inputted), 
             y = inputted_value, color = "white", size = 1, linetype = "dotted") +
    # annotate("line", x = c(t_min_inputted, t_max_inputted), 
    #          y = inputted_value, color = "#2171B5", size = 1, alpha = 0.5, linetype = "dashed") +
    labs(y = "Temperature", x = "Calendar Time") +
    theme_classic() + 
    theme(panel.grid.major.x = element_line(color = "grey80", linetype = "dashed")) +
    coord_cartesian(xlim = c(t_min_inputted, xMAX))
  
  cat("invpoints:", inversionPoints[1], " ", inversionPoints[2], "\n")
  
  
  addDownArrow <- function(plot, x, y, color = "blue", 
                           arrow_start_offset = 0.7, 
                           xoffset = 0, yoffset = 0) {
    plot +     
      annotate(
        "segment",
        x = x + xoffset,
        y = y + arrow_start_offset + yoffset,
        xend = x + xoffset,
        yend = y + yoffset,
        arrow = arrow(length = unit(0.3, "cm")),
        color = color,
        size = 1
      )
  }
  addUpArrow <- function(plot, x, y, color = "red", 
                           arrow_start_offset = 0.7, 
                           xoffset = 0, yoffset = -2.5) {
    plot +     
      annotate(
        "segment",
        x = x + xoffset,
        y = y  + yoffset,
        xend = x + xoffset,
        yend = y + yoffset + arrow_start_offset,
        arrow = arrow(length = unit(0.3, "cm")),
        color = color,
        size = 1
      )
  }
  # plot_b <- plot_b |> 
  #   # addDownArrow(min_x_coordOld, min_y_coordOld) |> 
  #   # addDownArrow(min_x_coord[1], min_y_coord[1], xoffset = 0.05) |> 
  #   addDownArrow(min_x_coord[2], min_y_coord[2]) |>
  #   addDownArrow(min_x_coord[3], min_y_coord[3], xoffset = -0.05, yoffset= -0.6)
  # 
  # plot_a <- plot_a |> 
  #   # addDownArrow(min_x_coordOld, min_y_coordOld) |> 
  #   # addDownArrow(min_x_coord[1], min_y_coord[1], xoffset = -0.08, yoffset = 1.5) |> 
  #   addDownArrow(min_x_coord[2], min_y_coord[2], yoffset = 2.5) |>
  #   addDownArrow(min_x_coord[3], min_y_coord[3], xoffset = -0.05, yoffset= -0.1)
  #   
  # plot_b <- plot_b |> 
  #   # addUpArrow(max_x_coordOld, max_y_coordOld) |> 
  #   addUpArrow(max_x_coord[1], max_y_coord[1], xoffset = 0, yoffset = -2.4) |> 
  #   # addUpArrow(max_x_coord[2], max_y_coord[2]) |>
  #   addUpArrow(max_x_coord[3], max_y_coord[3], xoffset = -0.0, yoffset = -1.85, 
  #              arrow_start_offset = 0.6) |> 
  # addUpArrow(max_x_coord[4], max_y_coord[4], xoffset = 0, yoffset = -2) |> 
  # addUpArrow(max_x_coord[5], max_y_coord[5], xoffset = -0.01, yoffset = -2.3)
  # 
  # plot_a <- plot_a |> 
  #   # addDownArrow(max_x_coordOld, max_y_coordOld) |> 
  #   addDownArrow(max_x_coord[1], max_y_coord[1], xoffset = 0, yoffset = -1.7, "red") |> 
  #   # addDownArrow(max_x_coord[2], max_y_coord[2]) |>
  #   addDownArrow(max_x_coord[3], max_y_coord[3], xoffset = -0.0, yoffset = -1, 
  #                arrow_start_offset = 0.6, "red") |> 
  #   addDownArrow(max_x_coord[4], max_y_coord[4], xoffset = 0, yoffset = -2.5, "red") |> 
  #   addDownArrow(max_x_coord[5], max_y_coord[5], xoffset = -0.01, yoffset = -2.3, "red")

    plotParameter <- list(plot_a, plot_b)
      # plot_a / plot_b + plot_layout(ncol = 1, heights = c(1, 1))
      # END YF
    
  } 
  # ------ OTHER -----
  if (attributes$tree == "Dengue") {
    plotParameter <- ggplot(summaryFull, aes(x = xAxisVariable)) +
      # geom_point(aes(y = median, color = "PopSize"), size = 3) +
      geom_line(aes(y = median, color = "PopSize"), size = 1.5) +
      geom_line(aes(y = 0), color = "darkgreen") +
      # geom_line(aes(y = q05, color = "PopSizeQ")) +
      # geom_line(aes(y = q95, color = "PopSizeQ")) +
      geom_ribbon(aes(ymin = q05, ymax = q95), fill = "darkgoldenrod1", alpha = 0.5) +
      # geom_smooth(aes(y = median, color = "PopSize"), method = "loess", se = FALSE, linewidth = 0.4) +
      # geom_point(aes(y = otherYAxisVariable * scale_factor, color = "OtherVar"), size = 3) +
      geom_line(aes(y = otherYAxisVariable * scale_factor, color = "OtherVar"), size = 1.5) +
      # geom_smooth(aes(y = otherYAxisVariable * scale_factor, color = paste0(otherYAxisVariableName)),
      # method = "loess", se = FALSE, linewidth = 0.4) +
      scale_y_continuous(
        name = expression(N[e](t)),
        sec.axis = sec_axis(~ . / scale_factor, name = paste0(otherYAxisVariableName))
      ) +
      labs(x = paste0(xAxisVariableName)) +
      # labs(title = paste0(jobName), x = paste0(xAxisVariableName), y = "Pop. Sizes (quantiles)") +
      scale_color_manual(
        name = "Legend",
        # values = c("OtherVar" = "#00BFC4", "PopSize"="#F8766D", "PopSizeQ"="red"), # remember to write them in alphabetical order
        values = c("OtherVar" = "#2171B5", "PopSize"="#F8766D", "PopSizeQ"="red"), # remember to write them in alphabetical order
        labels = c(paste0(otherYAxisVariableName), expression(N[e](t)), "PopSizeQ") # and match the labels appropriately
      ) +
      theme_minimal()
  }
  
  # if (actions$publicationQuality) {
  #   plotParameter <- plotParameter + theme(
  #     legend.position = "bottom",
  #     # legend.position = "bottom",  # Moves legend to upper right inside the plot  c(0.85, 0.85)
  #     legend.direction = "horizontal",
  #     # legend.justification = "center",
  #     legend.text = element_text(size = fontSize, family = fontType, color = "black"),
  #     legend.title = element_text(size = fontSize, family = fontType, color = "black"),
  #     legend.key.size = unit(2, 'cm'),
  #     plot.margin = margin(1, 1, 2, 1, "cm"),  # Increase bottom margin for legend
  #     legend.box.spacing = unit(1, "cm")
  #   ) +
  #   theme(text = element_text(size = fontSize, family = fontType, color = "black"),  # Set base text size
  #        axis.text = element_text(size = fontSize, family = fontType, color = "black"),  # Axis tick labels
  #        axis.title = element_text(size = fontSize, family = fontType, color = "black")
  #   )
  # }
  # 
  # if (!actions$publicationQuality) {
  #   plotParameter <- plotParameter + labs(title = paste0("Covariate: ", otherYAxisVariableName))
  # }
  return(plotParameter)
}

# plotDemographicGridCovariateQuantiles <- function(summStat, xAxisVariable, xAxisVariableName, 
#                                                   otherYAxisVariable, otherYAxisVariableName, jobName, 
#                                                   actions) {
#   summaryParameter <- summStat |> row.names() |>
#     sapply(function(x) grepl("PopSize", x)) %>% summStat[.,] |> as_tibble()
#   #summaryParameter$median <- (summaryParameter$median - mean(summaryParameter$median)) / sd(summaryParameter$median)
#   mean <- mean(c(summaryParameter$median, summaryParameter$q05, summaryParameter$q95))
#   sd <- sd(c(summaryParameter$median, summaryParameter$q05, summaryParameter$q95))
#   summaryParameter$median <- (summaryParameter$median - mean) / sd
#   summaryParameter$q05 <- (summaryParameter$q05 - mean) / sd
#   summaryParameter$q95 <- (summaryParameter$q95 - mean) / sd
#   otherYAxisVariable <- otherYAxisVariable 
#   summaryFull <- cbind(xAxisVariable, summaryParameter, otherYAxisVariable) |> as_tibble()
#   scale_factor <- max(summaryFull$median) / max(summaryFull$otherYAxisVariable)
#   # summaryFull$label <- otherYAxisVariableName
#   
#   # plotParameter <- ggplot(summaryFull, aes(x = xAxisVariable)) +
#   #   geom_point(aes(y = median, color="PopSizes"), size = 1.5) +
#   #   # geom_line(aes(y = median, color = "PopSize")) +
#   #   # geom_line(aes(y = 0), color = "darkgreen") +
#   #   # geom_ribbon(aes(ymin = q05, ymax = q95), fill = "darkgoldenrod1", alpha = 0.5) +
#   #   # geom_point(aes(y = otherYAxisVariable * scale_factor), color = "green", size = 1.0) +
#   #   # geom_line(aes(y = otherYAxisVariable * scale_factor, color = "OtherVar")) + 
#   #   theme(legend.position = "bottom") +
#   #   scale_color_manual(name = "Legend",  # Adding manual color scale with legend
#   #                     values = c("PopSizes" = "red"),
#   #                     labels = c("LogPopSizes"))
# 
#   plotParameter <- ggplot(summaryFull, aes(x = xAxisVariable)) +
#     geom_point(aes(y = median, color = "PopSizes"), size = 1.5) +  # Use color as a variable
#     theme(legend.position = "bottom") +  # Set legend to the bottom
#     scale_color_manual(name = "Legend",  # Custom color scale for the legend
#                        values = c("PopSizes" = "red"),
#                        labels = c("LogPopSizes"))
#   plotParameter <- plotParameter + theme_update(legend.position = "bottom")
#   
#   if (!actions$publicationQuality) {
#     plotParameter <- plotParameter + labs(title = paste0("Covariate: ", otherYAxisVariableName))
#   }
#   
#   set.seed(123)  # For reproducibility
#   data <- data.frame(
#     x = rnorm(100),
#     y = rnorm(100),
#     category = sample(c("A", "B", "C"), 100, replace = TRUE)
#   )
#   
#   # Create a ggplot with color and legend at the bottom
#   ppp <- ggplot(data, aes(x = x, y = y, color = category)) +
#     geom_point() + 
#     theme_minimal() +
#     labs(title = "Scatter Plot with Legend Below",
#          x = "X-axis", y = "Y-axis") +
#     theme(legend.position = "bottom")
#   
#   
#   return(ppp)
# }


