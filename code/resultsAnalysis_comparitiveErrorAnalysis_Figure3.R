

# R script associated with: "Automated HEART Score Determination via ChatGPT: Honing a Framework for Iterative Prompt Development"

# File name: "resultsAnalysis_comparitiveErrorAnalysis_Figure3.R" 
# Please see README file on GitHub repository for description: 
# https://github.com/conradsafranek/LLM_IterativeClinicalPromptDesign


# Quantitative Analysis - Comparison of error rates relative to physician assessment for LLM responses from two different prompt versions
# Code to Generate Figure 3


################################################################################
################################################################################

# 0) Install and load necessary packages

# Uncomment and run this code line the first time to install the packages in "tidyverse"
# install.packages("tidyverse") 

# Load installed library:
library(tidyverse) 


################################################################################
################################################################################


# 1) Load results from the two trials to compare (in this case, initial vs. final prompts for Figure3)

# Edit the file paths in "" to match your CSV file results 
# (**Adjust this file path based on the location of your file download** for example, see instructions here: https://support.apple.com/guide/mac-help/get-file-folder-and-disk-information-on-mac-mchlp1774/mac#:~:text=On%20your%20Mac%2C%20click%20the,bottom%20of%20the%20Finder%20window.)
results_V1_gpt3.5 <- read_csv("/Users/conradsafranek/Desktop/gptResponses_promptV1_gpt3.5.csv")
results_V1_gpt4   <- read_csv("/Users/conradsafranek/Desktop/gptResponses_promptV1_gpt4.csv")

results_V4_gpt3.5 <- read_csv("/Users/conradsafranek/Desktop/gptResponses_promptV4_gpt3.5.csv")
results_V4_gpt4   <- read_csv("/Users/conradsafranek/Desktop/gptResponses_promptV4_gpt4.csv")


all_results <- list(results_V1_gpt3.5, results_V4_gpt3.5, 
                    results_V1_gpt4, results_V4_gpt4) 

all_results <- lapply(all_results, function(df) {
  select(df, PAT_ENC_ID, PAT_ID, trialNum, histScore, ekgScore, risksScore, heartSCORE)
})

################################################################################
################################################################################

# 2) Defining true subscores

# Create a list of correct subscores based on physician gold-standard assessments 

true_scores <- list(
  pt1 = data.frame(histScore=2, ekgScore=1, risksScore=2),
  pt2 = data.frame(histScore=0, ekgScore=0, risksScore=0),
  pt3 = data.frame(histScore=0, ekgScore=1, risksScore=2),
  pt4 = data.frame(histScore=2, ekgScore=2, risksScore=1)
)


################################################################################
################################################################################


# 3) Clean up error types (create dataframe with clean error types)

# Label errors with "Incorrect Response Format"
clean_scores <- function(df) {
  columns_to_clean <- c("histScore", "ekgScore", "risksScore")
  
  for(col in columns_to_clean) {
    # Keep "0", "1", "2" values as is
    valid_responses <- c("0", "1", "2", "ERROR - Not enough info")
    df[[col]][!(df[[col]] %in% valid_responses) | is.na(df[[col]])] <- "ERROR - Incorrect Response Format"
  }
  return(df)
}


# Applying the function to each dataframe in all_results
all_results <- lapply(all_results, clean_scores)

# Preview of progress:
# View(all_results[[1]])


################################################################################
################################################################################

# 4) Define weighted error cost matrix for incorrect numerical LLM responses

# The matrix defines penalties (weights) for disagreements between the subscore ratings of a model and the true subscore.
# Diagonal elements (from top left to bottom right) represent perfect agreement and thus have a weight of 0.
# Off-diagonal elements provide penalties for specific disagreements.

# Matrix Structure:
# Rows correspond to the ratings given by the model, and columns correspond to the correct answers.
# Both rows and columns represent the possible values: 0, 1, 2, "ERROR - Not enough info", and "ERROR - Incorrect Response Format".

# Weights Logic:
# 1. A 1-point difference (e.g., model chooses 0 vs. correct answer is 1) is given a penalty of 1.
# 2. A 2-point difference (e.g., model chooses 0 vs. correct answer is 2) is given a penalty of 2.
# NA values generated for non-numerical answers

# NOTE: Could potentially introduce a higher error for underestimations relative to overestimations if we value sensitivity over specificity

# 4) Define weighted Kappa Matrix for wrong-answers:

# Expand the matrix to 5x5 for the new categories
weights <- matrix(c(0, 1, 2, NA, NA,    # Penalties when the model chooses 0,  and the correct answer is respectively either 0, 1, 2, or "ERROR - Not enough info"
                    1, 0, 1, NA, NA,    # Penalties when the model chooses 1,  and the correct answer is respectively either 0, 1, 2, or "ERROR - Not enough info"
                    2, 1, 0, NA, NA,    # Penalties when the model chooses 2,  and the correct answer is respectively either 0, 1, 2, or "ERROR - Not enough info"
                    NA,NA,NA,NA, NA,    # Penalties when the model chooses "ERROR - Not enough info",  and the correct answer is respectively either 0, 1, 2, or "ERROR - Not enough info"
                    NA,NA,NA,NA, NA),   # Penalties when the model response is "ERROR - Incorrect Response Format",  and the correct answer is respectively either 0, 1, 2, or "ERROR - Not enough info"
                  nrow=5)

# Transpose for alignment (worth double checking that it's working correctly for your own data)
weights <- t(weights)

# Modify the weight matrix to have row and column names
rownames(weights) <- c("0", "1", "2", "ERROR - Not enough info", "ERROR - Incorrect Response Format")
colnames(weights) <- c("0", "1", "2", "ERROR - Not enough info", "ERROR - Incorrect Response Format")


################################################################################
################################################################################

# 5. Calculate weighted kappa errors

# This function calculates the weighted error for each data row based on the defined weights matrix.
# It calculates the error for each subscore (histScore, ekgScore, risksScore) by referencing the matrix.
# The total weighted error for a row is then determined by summing up the weighted errors of the three subscores.
calculate_weighted_error <- function(df, true_scores, weights) {
  df <- df %>%
    rowwise() %>%
    mutate(
      weightErr_hist = weights[which(rownames(weights) == as.character(histScore)), which(colnames(weights) == as.character(true_scores[[PAT_ID]]$histScore))],
      weightErr_ekg = weights[which(rownames(weights) == as.character(ekgScore)), which(colnames(weights) == as.character(true_scores[[PAT_ID]]$ekgScore))],
      weightErr_risks = weights[which(rownames(weights) == as.character(risksScore)), which(colnames(weights) == as.character(true_scores[[PAT_ID]]$risksScore))],
      weightErr_total = weightErr_hist + weightErr_ekg + weightErr_risks
    )
  
  return(df)
}

# Applying the function to the datasets
all_results <- lapply(all_results, function(df) {
  calculate_weighted_error(df, true_scores, weights)
})



# Calculate mean errors and confidence intervals (see Results section)

# Calculate mean and 95% confidence intervals for the errors across all columns for each dataframe
confidence_intervals_avg_error <- lapply(all_results, function(df) {
  
  # Combine errors from all three columns into one vector
  combined_errors <- c(df$weightErr_hist[!is.na(df$weightErr_hist)], 
                       df$weightErr_ekg[!is.na(df$weightErr_ekg)], 
                       df$weightErr_risks[!is.na(df$weightErr_risks)])
  
  # Calculate standard deviation
  SD <- sd(combined_errors)
  
  # Calculate standard error
  SE <- sd(combined_errors) / sqrt(length(combined_errors))
  
  # Calculate the mean of combined errors
  mean_error <- mean(combined_errors)
  
  # Compute the 95% confidence intervals
  lower_bound <- mean_error - (1.96 * SE)
  upper_bound <- mean_error + (1.96 * SE)
  
  return(c(mean = mean_error, lower = lower_bound, upper = upper_bound, SD = SD))
})


################################################################################
################################################################################

# 6. Calculate unweighted errors

calculate_errors <- function(df, true_scores) {
  # Initializing the error columns
  df$histError <- 0; df$ekgError <- 0; df$risksError <- 0
  df$totalErrors <- 0;
  
  for(i in 1:nrow(df)) {
    patient <- df$PAT_ID[i]
    
    # Checking and updating errors for histScore, ekgScore, and risksScore
    columns <- c("histScore", "ekgScore", "risksScore")
    error_columns <- c("histError", "ekgError", "risksError")
    
    for(j in 1:3) {
      if(df[[columns[j]]][i] == "ERROR - Incorrect Response Format") {
        df[[error_columns[j]]][i] <- "ERROR - Incorrect Response Format"
      } else if(df[[columns[j]]][i] == "ERROR - Not enough info") {
        df[[error_columns[j]]][i] <- "ERROR - Not enough info"
      } else if(df[[columns[j]]][i] != as.character(true_scores[[patient]][[columns[j]]])) {
        df[[error_columns[j]]][i] <- "Numerical Error"
      }
    }
    
    # Calculating total errors and weighted error sum for each patient
    df$totalErrors[i] <- sum(df[i, error_columns] == "Numerical Error")
  }
  return(df)
}

# Applying the function to all results dfs
all_results <- lapply(all_results, function(df) calculate_errors(df, true_scores))




################################################################################
################################################################################

# 7. Create comprehensive error_summary dataframe

errors_summary <- sapply(all_results, function(df) {

  # Non-Numerical Errors (and unweighted yes/no classification of numerical errors) 
  
  hist_incorrect_format <- sum(df$histScore == "ERROR - Incorrect Response Format", na.rm = TRUE)
  ekg_incorrect_format <- sum(df$ekgScore == "ERROR - Incorrect Response Format", na.rm = TRUE)
  risks_incorrect_format <- sum(df$risksScore == "ERROR - Incorrect Response Format", na.rm = TRUE)
  total_incorrect_format <- hist_incorrect_format + ekg_incorrect_format + risks_incorrect_format
  
  hist_not_enough_info <- sum(df$histScore == "ERROR - Not enough info", na.rm = TRUE)
  ekg_not_enough_info <- sum(df$ekgScore == "ERROR - Not enough info", na.rm = TRUE)
  risks_not_enough_info <- sum(df$risksScore == "ERROR - Not enough info", na.rm = TRUE)
  total_not_enough_info <- hist_not_enough_info + ekg_not_enough_info + risks_not_enough_info
  
  hist_numerical_error <- sum(df$histError == "Numerical Error", na.rm = TRUE)
  ekg_numerical_error <- sum(df$ekgError == "Numerical Error", na.rm = TRUE)
  risks_numerical_error <- sum(df$risksError == "Numerical Error", na.rm = TRUE)
  total_numerical_error <- hist_numerical_error + ekg_numerical_error + risks_numerical_error
  
  total_nonNumerical = total_not_enough_info + total_incorrect_format
  total_numerical = total_numerical_error
  total_allErrors = total_not_enough_info + total_incorrect_format + total_numerical_error
  
  # Non-numerical error rates (for Figure 3A)
  numSubscorePrompts = 3
  totalResponses = (numSubscorePrompts * nrow(df))
  total_incorrectFormat_rate <- total_incorrect_format / totalResponses
  total_notEnoughInfo_error_rate <- total_not_enough_info / totalResponses
  
  # WEIGHTED Numerical Error Rates for Figure 3B (rates, normalized for number of numerical answers)
  
  hist_numerical_error_weighted_proportional  <- sum(df$weightErr_hist[df$histScore != "ERROR - Incorrect Response Format" & df$histScore != "ERROR - Not enough info"], na.rm = TRUE)      / (nrow(df) - hist_incorrect_format - hist_not_enough_info)
  ekg_numerical_error_weighted_proportional   <- sum(df$weightErr_ekg[df$ekgScore != "ERROR - Incorrect Response Format" & df$ekgScore != "ERROR - Not enough info"], na.rm = TRUE)         / (nrow(df) - ekg_incorrect_format - ekg_not_enough_info)
  risks_numerical_error_weighted_proportional <- sum(df$weightErr_risks[df$risksScore != "ERROR - Incorrect Response Format" & df$risksScore != "ERROR - Not enough info"], na.rm = TRUE)   / (nrow(df) - risks_incorrect_format - risks_not_enough_info)
  
  return(c(
    
    # Non-Numerical error counts:
    
    Hist_IncorrectFormat=hist_incorrect_format,
    EKG_IncorrectFormat=ekg_incorrect_format,
    Risks_IncorrectFormat=risks_incorrect_format,
    Total_IncorrectFormat=total_incorrect_format, 
    
    Hist_NotEnoughInfo=hist_not_enough_info,
    EKG_NotEnoughInfo=ekg_not_enough_info,
    Risks_NotEnoughInfo=risks_not_enough_info,
    Total_NotEnoughInfo=total_not_enough_info, 
    
    Hist_NumericalError=hist_numerical_error,
    EKG_NumericalError=ekg_numerical_error,
    Risks_NumericalError=risks_numerical_error,
    Total_NumericalError=total_numerical_error,
    
    Total_NonNumerical=total_nonNumerical,
    Total_Numerical=total_numerical,
    Total_AllErrors=total_allErrors,
    
    # Non-numerical error rates (for Figure 3A)
    Total_IncorrectFormat_rate=total_incorrectFormat_rate,
    Total_NotEnoughInfo_rate=total_notEnoughInfo_error_rate,
    
    # Weighted Numerical Error Rates (for Figure 3B)
    hist_numerical_error_weighted_proportional=hist_numerical_error_weighted_proportional,
    ekg_numerical_error_weighted_proportional=ekg_numerical_error_weighted_proportional,
    risks_numerical_error_weighted_proportional=risks_numerical_error_weighted_proportional))
})


################################################################################
################################################################################

# 8. Visualizing Results (to generate Figure 3)

all_errors <-  as.data.frame(errors_summary) %>% select(V1, V2, V3, V4)

# Rename the columns in each new dataframe to be consistent (note these correspond to Starting and Final prompt versions for GPT-3.5 and GPT-4 respectively)
colnames(all_errors) <- c("Starting", "Final", "Starting ", "Final ")


######################## For non-numerical responses: ######################## 


# Reshaping and filtering
long_data_nonNumerical <- all_errors %>%
  rownames_to_column("ErrorType") %>%
  filter(ErrorType %in% c("Total_IncorrectFormat_rate", "Total_NotEnoughInfo_rate")) %>% 
  pivot_longer(cols = starts_with("Starting") | starts_with("Final"),
               names_to = "Version",
               values_to = "Rate")

# Ensure the order of the ErrorType factor
long_data_nonNumerical$ErrorType <- factor(long_data_nonNumerical$ErrorType, levels = c("Total_IncorrectFormat_rate", "Total_NotEnoughInfo_rate")) 

# Set the factor levels for the Version column
long_data_nonNumerical$Version <- factor(long_data_nonNumerical$Version, levels = c("Starting", "Final", "Starting ", "Final "))

# Figure 3, Panel A: 
ggplot(long_data_nonNumerical, aes(x = Version, y = as.numeric(gsub("%", "", Rate)), fill = ErrorType)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#e993c8", "#A8D5BA"), #, "#79a0ba", "#4d80a3", "#21618C"
                    labels = c("Incorrect response format", "Insufficient information\nto determine score")) + 
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 0.225)) +
  labs(title = "Non-Numerical Response Rate",
       x = "Version of Prompt",
       y = "Percent of Responses",
       fill = "Error Type") +
  theme_minimal() + 
  theme(legend.position = "right", #"bottom" also an option
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey90"),
        panel.border = element_rect(colour = "grey40", fill=NA, linewidth=1))



########################    For numerical errors    ######################## 


# Graph of numerical wegithed error rates (Figure 3); weighted (e.g., model choice of 2 when correct error was 0 = 2 point error); proportional (i.e., numerical error rates are calculated out of numerical LLM responses)

# Reshaping and filtering data
long_data_weighted_proportional <- all_errors %>%
  rownames_to_column("ErrorType") %>%
  filter(ErrorType %in% c("hist_numerical_error_weighted_proportional", "ekg_numerical_error_weighted_proportional", "risks_numerical_error_weighted_proportional")) %>%  # "Total_IncorrectFormat_Weighted", "Total_NotEnoughInfo_Weighted", 
  pivot_longer(cols = starts_with("Starting") | starts_with("Final"),
               names_to = "Version",
               values_to = "WeightedError")

# Ensure the order of the ErrorType factor
long_data_weighted_proportional$ErrorType <- factor(long_data_weighted_proportional$ErrorType, levels = c("hist_numerical_error_weighted_proportional", "ekg_numerical_error_weighted_proportional", "risks_numerical_error_weighted_proportional")) 

# Set the factor levels for the Version column
long_data_weighted_proportional$Version <- factor(long_data_weighted_proportional$Version, levels = c("Starting", "Final", "Starting ", "Final "))

# Extract mean values from the confidence_intervals_avg_error list
mean_values <- sapply(confidence_intervals_avg_error, function(x) x["mean"])

# Convert the extracted mean values into a dataframe
mean_df <- data.frame(Version = names(mean_values), average_error = as.numeric(mean_values))

# Set the factor levels for the Version column in mean_df to match long_data_weighted_proportional
mean_df$Version <- factor(mean_df$Version, levels = levels(long_data_weighted_proportional$Version))

mean_df$Version <- c("Starting", "Final", "Starting ", "Final ")

# Figure 3, Panel B: 
ggplot(long_data_weighted_proportional, aes(x = Version, y = WeightedError)) +
  geom_bar(aes(fill = ErrorType), stat = "identity", position = "dodge", width = 0.5) +
  geom_crossbar(data = mean_df, 
                aes(y = average_error, ymin = average_error, ymax = average_error), 
                width = 0.52, 
                size = 0.2, 
                color = "grey50") +
  scale_fill_manual(values = c("#ffeda0", "#feb24c", "#f03b20"),
                    labels = c("History", "ECG", "Risk Factors")) +
  labs(title = "Average Error for Numerical Subscore Responses",
       x = "Version of Prompt",
       y = "Average Error",
       fill = "Error Type") +
  scale_y_continuous(limits = c(0, 0.7)) +
  theme_minimal() + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "grey90"),
        panel.border = element_rect(colour = "grey40", fill=NA, linewidth=1))




