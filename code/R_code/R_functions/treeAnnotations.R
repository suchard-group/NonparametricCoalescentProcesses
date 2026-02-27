extract_coordinate <- function(tree, node_number) {
  tree_data <- tree$data
  # Extract the coordinates for a specific node number
  coords <- tree_data[tree_data$node == node_number, c("x", "y")] |> as.list() |> unlist()
  return(coords)
}
# extract_coordinate_rescaled <- function(tree, node_number, mrsd) {
#   coords <- extract_coordinate(tree, node_number)
#   coords[1] <- mrsd - coords[1]  # Invert the x-coordinate based on mrsd
#   return(coords)
# }
getHPD <- function(tree, node_number) {
  tree_data <- tree$data
  hpd <- tree_data[tree_data$node == node_number, "height_0.95_HPD"][[1]][[1]]
  return(hpd)
}

getHPD_around_Zero <- function(tree, node_number) {
  hpd <- getHPD(tree, node_number)
  height <- tree$data[tree$data$node == node_number, "height"][[1]] 
  if (!is.numeric(height)) height <- as.numeric(height)
  hpd_around_zero <- hpd - height
  return(hpd_around_zero)
}

corrected_hpd_by_mrsd <- function(tree, node_number, mrsd, reversedAxis = FALSE) {
  coords <- extract_coordinate(tree, node_number)
  hpd <- getHPD_around_Zero(tree, node_number) + coords[1]
  
  if(!reversedAxis){
    return(data.frame(x =  hpd[1], xend = hpd[2], y = coords[2] ))
  } else { 
    return(data.frame(x = coords[2],y = hpd[1], yend = hpd[2] ))
  }
  
}

addHPD_rectangles_background <- function(plot, tree, nodes, 
                                         color = "#FFDB58", alpha = 0.6, 
                                         width = 4, reversedAxis = FALSE) {
  plot_background <- tree |> addHPD_rectangles(tree, nodes, color, alpha, width, reversedAxis = reversedAxis)
  # plot_foreground <- tree + theme(panel.background = element_rect(fill = NA, colour = NA),
  #                                 plot.background = element_rect(fill = NA, colour = NA)) 
  # plot_foreground <- plot_foreground |> addHPD_rectangles(tree, nodes, color, alpha, width=0, reversedAxis = reversedAxis)
  return(plot_background + geom_tree())
}

addHPD_rectangles <- function(plot, tree, nodes,
                              color = "#FFDB58", alpha = 0.6, 
                              width = 4, reversedAxis = FALSE) {
  newplot <- tree
  for (i in nodes) {
    hpd_values <- corrected_hpd_by_mrsd(tree, i, 0, reversedAxis = reversedAxis) |> unlist()
    if (!reversedAxis) {
      newplot <- newplot + 
        annotate("rect",
                 xmin = hpd_values[1], xmax = hpd_values[2],
                 ymin = hpd_values[3]-width/2, ymax = hpd_values[3]+width/2,
                 fill = color, alpha = alpha)
    } else {
      newplot <- newplot + annotate("rect",
                                    xmin = hpd_values[2], xmax = hpd_values[3],
                                    ymin = hpd_values[1]- width/2, ymax = hpd_values[1] + width/2,
                                    fill = color, alpha = alpha)
    }
    
  }
  
  return(newplot)
}


add_HPD_Whisher <- function(tree, node_number, mrsd, cap_length = 7, color_segments = "grey", linewidth = 0.5) {
  tree +  
    geom_segment(
      data = corrected_hpd_by_mrsd(tree, node_number, mrsd),
      aes(x = x, xend=xend, y = y),
      color = color_segments,
      linewidth =linewidth,
      inherit.aes = FALSE
    ) +
    # Cap at the start of the HPD segment
    geom_segment(
      data = corrected_hpd_by_mrsd(tree, node_number, mrsd),
      aes(
        y = y - cap_length / 2,
        yend = y + cap_length / 2,
        x = x,
        xend = x
      ),
      color = color_segments,
      linewidth = linewidth,
      inherit.aes = FALSE
    ) + 
    # Cap at the end of the HPD segment
    geom_segment(
      data = corrected_hpd_by_mrsd(tree, node_number, mrsd),
      aes(
        y = y - cap_length / 2,
        yend = y + cap_length / 2,
        x = xend,
        xend = xend
      ),
      color = color_segments,
      linewidth = linewidth,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = data.frame(
        plot_x = extract_coordinate(tree, node_number)[1],
        plot_y = extract_coordinate(tree, node_number)[2]
      ),
      aes(x = plot_x, y = plot_y),
      color = color_segments,
      size = linewidth + 0.5,
      shape = 16,
      inherit.aes = FALSE
    )
}
add_HPD_reverse_tree_Whisher <- function(tree, node_number, mrsd, cap_length = 7, color_segments = "grey", linewidth = 0.5) {
  tree +  
    geom_segment(
      data = corrected_hpd_by_mrsd(tree, node_number, mrsd, reversedAxis = TRUE),
      aes(y = x, x = y, xend = yend),
      color = color_segments,
      linewidth = linewidth,
      inherit.aes = FALSE
    ) + 
    # Cap at the start of the HPD segment
    geom_segment(
      data = corrected_hpd_by_mrsd(tree, node_number, mrsd, reversedAxis = TRUE),
      aes(
        y = x - cap_length / 2,
        yend = x + cap_length / 2,
        x = y,
        xend = y
      ),
      color = color_segments,
      linewidth = linewidth,
      inherit.aes = FALSE
    ) + 
    # Cap at the end of the HPD segment
    geom_segment(
      data = corrected_hpd_by_mrsd(tree, node_number, mrsd, reversedAxis = TRUE),
      aes(
        y = x - cap_length / 2,
        yend = x + cap_length / 2,
        x = yend,
        xend = yend
      ),
      color = color_segments,
      linewidth = linewidth,
      inherit.aes = FALSE
    ) + 
    geom_point(
      data = data.frame(
        plot_x = extract_coordinate(tree, node_number)[2],
        plot_y = extract_coordinate(tree, node_number)[1]
      ),
      aes(x = plot_y, y = plot_x),
      color = color_segments,
      size = linewidth + 0.5,
      shape = 16,
      inherit.aes = FALSE
    )
}

date_to_years_decimals <- function(date_obj) {
  year <- as.numeric(format(date_obj, "%Y"))
  
  # Calculate the day of the year (1-366)
  day_of_year <- as.numeric(format(date_obj, "%j"))
  
  # Determine if it's a leap year
  is_leap_year <- function(year) {
    (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  }
  
  # Get the total number of days in the year
  days_in_year <- ifelse(is_leap_year(year), 366, 365)
  
  # Calculate the decimal part
  decimal_part <- (day_of_year - 1) / days_in_year # Subtract 1 because Jan 1st is day 1, but represents 0% of the year passed.
  year_decimals <- year + decimal_part
  # Combine to get the years.decimals format
  return(year_decimals)
}