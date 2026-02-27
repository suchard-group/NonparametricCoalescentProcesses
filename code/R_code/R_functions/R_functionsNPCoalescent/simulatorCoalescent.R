# functions to simulate the coalescent and build the newick tree
simulate_coalescent_timeline <- function(N, gridPoints, pop_sizes) {
  if (length(gridPoints) + 1 != length(pop_sizes)) {
    stop("Number of population sizes must be one more than the number of gridPoints")
  }
  
  x <- numeric(N - 1)  # Store coalescent times
  current_time <- 0  # Start from time 0
  k <- N  # Start with N lineages
  
  # Track intervals
  interval_idx <- 1  
  next_breakpoint <- ifelse(length(gridPoints) > 0, gridPoints[interval_idx], Inf)
  
  while (k > 1) {
    # Current population size
    N_current <- pop_sizes[interval_idx]
    
    # Coalescence rate in this interval
    rate <- k * (k - 1) / ( 2 * N_current)
    
    # Generate waiting time
    wait_time <- rexp(1, rate)
    
    if (current_time + wait_time < next_breakpoint) {
      # Coalescent event happens within the current interval
      current_time <- current_time + wait_time
      x[N - k + 1] <- current_time
      k <- k - 1
    } else {
      # Move to next population size region
      wait_time <- next_breakpoint - current_time
      current_time <- next_breakpoint
      
      interval_idx <- interval_idx + 1
      next_breakpoint <- ifelse(interval_idx <= length(gridPoints), 
                                gridPoints[interval_idx], Inf)
    }
  }
  
  return(x)
}

build_newick <- function(N, x) {
  if (length(x) != N - 1) {
    stop("Length of x must be N - 1")
  }
  
  # Initialize taxa as leaf nodes
  taxa <- as.character(1:N)
  
  # Maintain a list of active nodes
  active_nodes <- taxa
  
  # Track the age of each node
  node_ages <- setNames(rep(0, N), taxa)
  
  for (i in seq_along(x)) {
    # Coalesce the first two active nodes in order
    new_node <- sprintf("(%s:%f,%s:%f)", 
                        active_nodes[1], x[i] - node_ages[active_nodes[1]], 
                        active_nodes[2], x[i] - node_ages[active_nodes[2]])
    
    # Update node ages
    node_ages[new_node] <- x[i]
    
    # Remove coalesced nodes and add the new one
    active_nodes <- c(active_nodes[-(1:2)], new_node)
  }
  
  # Final tree with a terminating semicolon
  return(paste0(active_nodes, ";"))
}


generate_taxa_xml <- function(N) {
  taxa_lines <- c('<taxa id="taxa">')
  
  for (i in 1:N) {
    taxa_lines <- c(taxa_lines, sprintf('\t<taxon id="%d"/>', i))
  }
  
  taxa_lines <- c(taxa_lines, "</taxa>")
  
  return(paste(taxa_lines, collapse = "\n"))
}