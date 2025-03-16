# Law of Large Numbers - Coin Flip Simulation in R

coin_flip_simulation <- function(num_flips) {
  # Simulate coin flips (0 for tails, 1 for heads)
  flips <- sample(0:1, num_flips, replace = TRUE)
  
  # Calculate the cumulative average after each flip
  cumulative_sum <- cumsum(flips)
  flips_array <- 1:num_flips
  cumulative_averages <- cumulative_sum / flips_array
  
  # Count total heads and tails
  total_heads <- sum(flips)
  total_tails <- num_flips - total_heads
  
  return(list(
    cumulative_averages = cumulative_averages, 
    flips_array = flips_array,
    total_heads = total_heads,
    total_tails = total_tails,
    final_proportion = cumulative_averages[length(cumulative_averages)],
    num_flips = num_flips
  ))
}

plot_results <- function(simulation_results) {
  # Extract results
  cumulative_averages <- simulation_results$cumulative_averages
  flips_array <- simulation_results$flips_array
  
  # Create plot with fixed y-axis limits from 0 to 1
  plot(flips_array, cumulative_averages, type = "l", 
       log = "x", # Using log scale for x-axis to better visualize early behavior
       xlab = "Number of Flips", 
       ylab = "Proportion of Heads",
       main = "Law of Large Numbers - Coin Flip Simulation",
       ylim = c(0, 1)) # Set y-axis limits from 0 to 1
  
  # Add expected value line
  abline(h = 0.5, col = "red", lty = 2)
  
  # Add legend
  legend("topright", 
         legend = c("Running Average", "Expected Value (0.5)"),
         col = c("black", "red"), 
         lty = c(1, 2))
  
  # Add final value annotation
  final_value <- cumulative_averages[length(cumulative_averages)]
  text_y <- ifelse(final_value > 0.5, 0.45, 0.55)
  arrows(flips_array[length(flips_array)] * 0.3, text_y,
         flips_array[length(flips_array)], final_value,
         length = 0.1)
  text(flips_array[length(flips_array)] * 0.2, text_y,
       paste0("After ", flips_array[length(flips_array)], 
              " flips: ", round(final_value, 4)))
  
  # Add grid
  grid(lty = 3, col = "gray")
}

print_results <- function(simulation_results) {
  # Extract results
  cumulative_averages <- simulation_results$cumulative_averages
  flips_array <- simulation_results$flips_array
  
  # Print summary statistics
  cat("\n===== SIMULATION RESULTS =====\n")
  cat("Number of flips:", flips_array[length(flips_array)], "\n")
  cat("Final proportion of heads:", round(cumulative_averages[length(cumulative_averages)], 6), "\n")
  cat("Expected proportion:", 0.5, "\n")
  cat("Difference from expected:", round(abs(cumulative_averages[length(cumulative_averages)] - 0.5), 6), "\n")
  
  # Print some intermediate values
  if (length(flips_array) > 10) {
    cat("\nProportion of heads after:\n")
    checkpoints <- c(10, 100, 1000, 10000, 100000, 1000000)
    checkpoints <- checkpoints[checkpoints <= length(flips_array)]
    
    for (cp in checkpoints) {
      cat(sprintf("%7d flips: %f\n", cp, cumulative_averages[cp]))
    }
  }
  cat("==============================\n\n")
}

print_all_results_table <- function(simulations) {
  if (length(simulations) == 0) {
    cat("No simulations were run.\n")
    return()
  }
  
  cat("\n\n=========== ALL SIMULATION RESULTS ===========\n\n")
  
  # Create a data frame for the results table
  results_df <- data.frame(
    Simulation = 1:length(simulations),
    Flips = sapply(simulations, function(x) x$num_flips),
    Heads = sapply(simulations, function(x) x$total_heads),
    Tails = sapply(simulations, function(x) x$total_tails),
    Proportion = sapply(simulations, function(x) round(x$final_proportion, 6)),
    Deviation = sapply(simulations, function(x) round(abs(x$final_proportion - 0.5), 6))
  )
  
  # Print the data frame as a nicely formatted table
  print(results_df, row.names = FALSE)
  
  # Print summary statistics
  cat("\nSummary Statistics:\n")
  cat("Total number of simulations:", length(simulations), "\n")
  cat("Total flips across all simulations:", sum(results_df$Flips), "\n")
  cat("Average proportion of heads:", round(mean(results_df$Proportion), 6), "\n")
  cat("Average deviation from expected value:", round(mean(results_df$Deviation), 6), "\n")
  
  # Calculate standard deviation of proportions
  if(length(simulations) > 1) {
    cat("Standard deviation of proportions:", round(sd(results_df$Proportion), 6), "\n")
  }
  
  cat("\n==============================================\n")
  
  # Return the data frame so it can be used for further analysis if needed
  invisible(results_df)
}

# Modified function to ensure table is always displayed at the end
get_yes_no_input <- function(prompt, all_simulations) {
  while(TRUE) {
    cat(prompt)
    answer <- tolower(readline())
    
    if (answer == "y" || answer == "yes") {
      return(TRUE)
    } else if (answer == "n" || answer == "no") {
      # When the user selects "n", display the results table
      print_all_results_table(all_simulations)
      return(FALSE)
    } else {
      cat("Invalid input. Please enter 'y' or 'n'.\n")
    }
  }
}

main <- function() {
  # Initialize the list inside main to avoid global variable issues
  all_simulations <- list()
  simulation_count <- 0
  
  while(TRUE) {
    # Get user input for number of flips
    cat("Enter the number of coin flips to simulate (e.g., 10000): ")
    num_flips_input <- readline()
    
    # Check if input is valid
    num_flips <- suppressWarnings(as.numeric(num_flips_input))
    
    if(is.na(num_flips) || num_flips <= 0) {
      cat("Please enter a positive number.\n")
      next
    }
    
    # Run the simulation
    simulation_count <- simulation_count + 1
    simulation_results <- coin_flip_simulation(num_flips)
    
    # Store the simulation results
    all_simulations[[simulation_count]] <- simulation_results
    
    # Print the results
    print_results(simulation_results)
    
    # Plot the results
    plot_results(simulation_results)
    
    # Ask if user wants to run another simulation with improved validation
    # Pass all_simulations to get_yes_no_input so it can display the table when needed
    if (!get_yes_no_input("Do you want to run another simulation? (y/n): ", all_simulations)) {
      # No need to call print_all_results_table here since it's now called within get_yes_no_input
      break
    }
  }
}

# Run the main function to start the simulation
main()
