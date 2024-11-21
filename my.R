# Load necessary library
library(ggplot2)
library(dplyr)
library(tidyr)

# Parameters
# Set the number of candidates
num_candidates <- 500
# Set the selection top percentage
top_percentage = 0.01
# Number of trials
N_trials <- 1000
# Different exploration proportions
explore_fractions <- seq(0.1, 0.8, by = 0.1)  


################################################################################
# Function for create candidate full information data frame
create_candidate_df <- function(num_candidates){
  # Create a data frame with candidate names
  data <- data.frame(
    # Names candidate1, candidate2, etc.
    candidate = paste0(1:num_candidates),
    # Scores from uniform distribution
    score = round(runif(num_candidates, min = 0, max = 100))
  )
  return(data)
}
################################################################################


# Function for get top candidates min max scores
get_top_min_max_scores <- function(data, top_percentage){
  # Sort the interviewed candidates data frame
  sorted_revealed_candidates_df <- data[order(-data$score), ]
  
  # Extract the top percentage rows form interviewed candidates
  num_rows <- nrow(sorted_revealed_candidates_df)
  top_1_count <- max(1, ceiling(top_percentage * num_rows))
  top_1_percent <- sorted_revealed_candidates_df[1:top_1_count, ]
  
  min_score <- min(top_1_percent$score)  # Get the minimum score
  max_score <- max(top_1_percent$score)  # Get the maximum score
  
  # If min and max are equal, adjust the min value
  if (min_score == max_score) {
    min_score <- max_score - 1
  }
  return(list(top_1_percent, min_score, max_score))
}
################################################################################


# Function to simulate the Basic Secretary Problem
simulate_secretary <- function(num_candidates, revealed_df, unrevealed_df, revealed_count) {
  # Search time
  search_time <- NA
  # Explore max score
  max_score <- max(revealed_df$score)

  # Exploitation phase
  for (i in 1: nrow(unrevealed_df)){
    # Get the candidate name
    candidate_name <- unrevealed_df$candidate[i]
    # Get the candidate score
    candidate_score <- unrevealed_df$score[i]
    # Check whether candidate score is greater than max score
    if (candidate_score >= max_score){
      search_time <- i + revealed_count
      # Get the selected candidate
      selected_candidate <- candidate_name
    }
  }
  # If no candidate meets criteria, select the last candidate
  if (is.na(search_time)) {
    search_time <- num_candidates
    # Get the selected candidate
    selected_candidate <- candidate_name
  }
  return(list(search_time, selected_candidate))
}
################################################################################


# Function for check success of the simulations
check_selection_success <- function(full_info_data, selected_candidate, top_percentage){
  # Number of top scores
  top_scores <- ceiling(nrow(full_info_data) * top_percentage)
  # Get the top 1% unique scores
  top_unique_scores <- unique(full_info_data$score)[1:top_scores]
  # Include all candidates with scores equal to or greater than highest score
  top_candidates <- full_info_data[full_info_data$score >= top_unique_scores, ]
  # Check if the given candidate is in the top percentage
  is_in_top_1_percent <- selected_candidate %in% top_candidates$candidate
  return(is_in_top_1_percent)
}
################################################################################


# Function for simulate strategy
simulate_strategy <- function(num_candidates, fraction, top_percentage, N_trials){
  # Store results
  results <- data.frame(
    SearchTimeStrategy = integer(),
    SearchTimeSecretary = integer(),
    SelectionSuccessStrategy = integer(),
    SelectionSuccessSecretary = integer()
  )
  
  for (trial in 1:N_trials) {
    # Candidate full information data frame
    full_information_data <- create_candidate_df(num_candidates)
    
    # Sorted candidate full information data
    sorted_full_information_data <- full_information_data[order(-full_information_data$score), ]
    
    # Calculate number of interviewed candidates 
    revealed_candidates <- round(num_candidates * fraction)
    
    # Calculate number of not interviewed candidates 
    unrevealed_candidates <- (num_candidates - revealed_candidates)
    
    # Interviewed candidates data frame
    revealed_candidates_df <- full_information_data[1:revealed_candidates, ]
    
    # Not interviewed candidates data frame
    unrevealed_candidates_df <- full_information_data[(revealed_candidates + 1) : nrow(full_information_data), ]
    
    for (i in 1: nrow(unrevealed_candidates_df)){
      # Get the candidate score
      candidate_score <- unrevealed_candidates_df$score[i]
      # Call the min max scores function to get min max score values
      # And get top candidates data frame
      min_max_score <- get_top_min_max_scores(revealed_candidates_df, top_percentage)
      
      top_1_percent <- min_max_score[[1]]
      min_score <- min_max_score[[2]]
      max_score <- min_max_score[[3]]
      
      #print(paste("Min Score:", min_score, "Max Score:", max_score))
      # Check whether candidate score within min max range
      if (candidate_score >= min_score && candidate_score <= max_score){
        # If the candidate score is in range, add them to the top_1_percent data frame
        top_1_percent <- rbind(top_1_percent, unrevealed_candidates_df[i, ])
        # Search time, number of candidates
        search_time <- i + revealed_candidates
        # Selected candidate
        selected_candidate <- unrevealed_candidates_df$candidate[i]
        # Success rate calculation
        selection_success_strategy <- if (check_selection_success(sorted_full_information_data, selected_candidate, top_percentage)) 1 else 0
        break;
      }
      else{
        revealed_candidates_df <- rbind(revealed_candidates_df, unrevealed_candidates_df[i, ])
      }
      
    }
    
    # Simulate secretary
    secretary_problem <- simulate_secretary(num_candidates, revealed_candidates_df, unrevealed_candidates_df, revealed_candidates)
    # Secretary problem search time
    search_time_secretary <- secretary_problem[[1]]
    # Secretary problem success rate
    success_rate_secretary <- if (check_selection_success(sorted_full_information_data, secretary_problem[[2]], top_percentage)) 1 else 0
    # Record results
    results <- rbind(results, data.frame(
      Trial = trial,
      SearchTimeStrategy = search_time,
      SearchTimeSecretary = search_time_secretary,
      SelectionSuccessStrategy = selection_success_strategy,
      SelectionSuccessSecretary = success_rate_secretary
    ))
  }
  return(results)
}
################################################################################


# Set seed
set.seed(123)
# Run simulations for different exploration fractions
all_results <- lapply(explore_fractions, function(fraction) {
  simulate_strategy(num_candidates, fraction, top_percentage, N_trials) %>%
    mutate(ExploreFraction = fraction)
}) %>%
  bind_rows()
################################################################################


# Summary results average search time
summary_results_time <- all_results %>%
  group_by(ExploreFraction) %>%
  summarise(
    AvgSearchTimeStrategy = mean(SearchTimeStrategy),
    AvgSearchTimeSecretary = mean(SearchTimeSecretary)
  )

# Summary results average search success rate
summary_results_rate <- all_results %>%
  group_by(ExploreFraction) %>%
  summarise(
    AvgSuccessRateStrategy = mean(SelectionSuccessStrategy),
    AvgSuccessRateSecretary = mean(SelectionSuccessSecretary)
  )
################################################################################


# Analyze and visualize results
# plot  Average search time
# Transform data to long format for plotting
long_results_time <- summary_results_time %>%
  pivot_longer(
    cols = c(AvgSearchTimeStrategy, AvgSearchTimeSecretary),
    names_to = "Metric",
    values_to = "AVGSearchTime"
  )

# Plot the results
ggplot(long_results_time, aes(x = ExploreFraction, y = AVGSearchTime, color = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.37, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Average Search Time vs Explore Fraction",
    x = "Explore Fraction",
    y = "Average Search Time (Number of Candidates)",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 10)
  )
################################################################################


# plot  Average success rate
# Transform data to long format for plotting
long_results_rate <- summary_results_rate %>%
  pivot_longer(
    cols = c(AvgSuccessRateStrategy, AvgSuccessRateSecretary),
    names_to = "Metric",
    values_to = "AVGSuccessRate"
  )

# Plot the results
ggplot(long_results_rate, aes(x = ExploreFraction, y = AVGSuccessRate, color = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.37, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Average Success Rate vs Explore Fraction",
    x = "Explore Fraction",
    y = "Average Success Rate",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 10)
  )
################################################################################



