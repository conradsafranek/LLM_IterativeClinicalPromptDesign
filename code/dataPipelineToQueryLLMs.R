
# R script associated with: "Automated HEART Score Determination via ChatGPT: Honing a Framework for Iterative Prompt Development"

# File name, main script: "dataPipelineToQueryLLMs.R" 
# Please see README file on GitHub repository for description: 
# https://github.com/conradsafranek/LLM_IterativeClinicalPromptDesign



# OVERVIEW of STEPS:

### 0) Install and load necessary packages

### 1) Gather relevant notes for each subscore prompts 
#      (History, Risk Factors and EKG for the following 3 steps, respectively)
# 1a. Current_Notes: Gather time stamp, note type, and provider type, for all 
#         notes related to the current encounter (not including EKG/troponin notes)
# 1b. All_Notes: Gather all patient current & past notes (not including EKG/troponin notes)
# 1c. EKG_Interpretation: Gather same data for EKG read associated with the current visit
# 1d. Duplicate rows for N simulated repeat trials for each patient

### 2) Send prompt queries and receive GPT responses for the respective prompts and with each set of compiled notes from step 1
# 2a. Prepare LLM settings
# 2b. History
# 2c. EKG
# 2d. Risks

### 3) Calculate heart score
# 3a. Extract bracketed answers from GPT responses
# 3b. Calculate Trop and Age subscores from parsing structured data in the synthetic EHR data extract
# 3c. Calculate HEART scores

### 4) Calculate meta statistics (price and run-time)

### 5) Examine results (Count of NA, Variability of responses):


# NOTE:
# Key adjustable parameters [and where to edit them]:
#   - Number of repeat trials per note, N, found in step [1d]
#   - LLM choice, either GPT-3.5-turbo-16k or GPT-4, found in step [2a]
#   - LLM "temperature", i.e. response stochasticity, found in step [2a]
#   - Insertion of personal OpenAI API key, necessary to run the API calls [see helperFunction]

################################################################################
################################################################################

# (0) Install and load necessary packages


# For each of the subsequent libraries, for the first time running this script, uncomment and run the following lines ...
# install.packages("tidyverse"); install.packages("future"); # .... 

# Load necessary libraries
library(tidyverse) # Includes dplyr, ggplot2, purrr, stringr, among others
library(future) # For parallelization of GPT-3.5 queries
library(furrr)  # For parallelization of GPT-3.5 queries
library(glue) # For "gluing" together strings 
library(httr) #For GPT API queries
library(jsonlite) # For GPT API queries
library(reticulate) # For interacting with Python script for tokenizer function to calculate price


################################################################################
################################################################################
################################################################################

### (1) Gather relevant notes 


# Prep work for all steps:

# Read the csv spreadsheet into a dataframe called "allNotes" 
# (Note: **Adjust this file path based on the location of your file download** for example, see instructions here: https://support.apple.com/guide/mac-help/get-file-folder-and-disk-information-on-mac-mchlp1774/mac#:~:text=On%20your%20Mac%2C%20click%20the,bottom%20of%20the%20Finder%20window.)
allNotes <- read.csv('/Users/conradsafranek/Documents/DECILE/Automated Heart Score via LLM/JAMA Submission/Supplemental/data/simulatedPatientNotes_ehrDataExtractSample.csv',
                     header = TRUE)

# Convert 'LastEditedInstant' column to POSIXct (recognizable/readable date format)
allNotes$LastEditedInstant <- as.POSIXct(allNotes$LastEditedInstant, format="%Y-%m-%d %H:%M:%S")

# Order "allNotes" by PAT_ENC_ID and then by LastEditedInstant in descending order (most recent at top)
# Note: In future research it may be worth testing the effects of ordering these in ascending order instead (chronological)
allNotes <- arrange(allNotes, PAT_ID, desc(LastEditedInstant))

# Add a new column "compiledText" to "allNotes" which is the note with the relevant labels for that note, constructed as follows:
# - Start with either "((CURRENT Visit Encounter Note))" or "((PAST Visit Encounter Note))" based on the presence of "visCurrent" in the "PAT_ENC_ID" column.
# - Append "Last Edited: " followed by the date from "LastEditedInstant".
# - Append "Type: " followed by the value from "Type". (e.g., ED Visit or PCP Visit)
# - Append "Provider: " followed by the value from "Provider". (e.g. Nurse or Physician)
# - Finally, append the actual note text from the "TEXT" column.
allNotes <- mutate(allNotes,
                   compiledText = paste0(
                     if_else(grepl("visCurrent", PAT_ENC_ID), 
                             "((CURRENT Visit Encounter Note))", 
                             "((PAST Visit Encounter Note))"),
                     "\n\nLast Edited: ", LastEditedInstant, 
                     "\nType: ", Type, 
                     "\nProvider: ", Provider, 
                     "\n\n\n", TEXT))

# Preview result (uncomment and run this line to preview result of previous step):
# cat(allNotes$compiledText[1])


################################################################################
################################################################################

# (1a) Current_Notes: Gather all notes related to the current encounter (excluding EKG/troponin notes).


# Create "currentVisitNotes" dataframe by selecting only the "PAT_ENC_ID" and "PAT_ID" columns from "allNotes".
currentVisitNotes <- select(allNotes, PAT_ENC_ID, PAT_ID)

# Retain only those rows where "PAT_ENC_ID" contains the string "visCurrent".
currentVisitNotes <- filter(currentVisitNotes, grepl("visCurrent", PAT_ENC_ID))

# Remove duplicate rows to ensure uniqueness in the "currentVisitNotes" dataframe.
currentVisitNotes <- distinct(currentVisitNotes)

# Initialize an empty column "compiledAllText_current" in "currentVisitNotes" for storing aggregated texts.
currentVisitNotes$compiledAllText_current = ""
counter = 1 # Initialize a counter to keep track of the number of notes appended during the loop.

# Loop through each unique "PAT_ENC_ID" in "currentVisitNotes".
for (n in 1:length(currentVisitNotes$PAT_ENC_ID)){
  # For each unique ID, loop through all rows in "allNotes" to find matching entries.
  for (i in 1:length(allNotes$PAT_ENC_ID)){
    # Check if the current "PAT_ENC_ID" matches and the type of note is "ED Visit".
    if (currentVisitNotes$PAT_ENC_ID[n] == allNotes$PAT_ENC_ID[i] &
        allNotes$Type[i] == "ED Visit"){
      # Capture the current aggregated text (from "prep work" step above) for the specific "PAT_ENC_ID".
      currentText = currentVisitNotes$compiledAllText_current[n]
      # Append the new note to the aggregated text. Include logical delimiters between notes.
      currentVisitNotes$compiledAllText_current[n] = paste0(currentText,
                                                            "#####################################",
                                                            "\n\n NOTE [#", counter, "] IN CURRENT ENCOUNTER",
                                                            "\n", allNotes$compiledText[i], "\n\n\n")
      # Increment the counter.
      counter = counter + 1
    }
  }
  # Reset the counter for the next unique "PAT_ENC_ID".
  counter = 1
}

# Preview result:
# cat(currentVisitNotes$compiledAllText_current[1])


################################################################################
################################################################################

# (1b) Current and Past Notes: Accumulate all pertinent patient notes from the patient's history leading up to, and including, the current encounter.

# Create "allPtNotes" dataframe from "allNotes" by selecting the "PAT_ENC_ID", "PAT_ID", and "LastEditedInstant" columns.
allPtNotes <- select(allNotes, PAT_ENC_ID, PAT_ID, LastEditedInstant)

# Retain only those rows where "PAT_ENC_ID" contains the string "visCurrent".
allPtNotes <- filter(allPtNotes, grepl("visCurrent", PAT_ENC_ID))

# For each unique "PAT_ENC_ID", keep only the row with the most recent "LastEditedInstant" value.
allPtNotes <- allPtNotes %>%
  group_by(PAT_ENC_ID) %>%
  slice_max(LastEditedInstant)

# Initialize an empty column "compiledAllText_currentAndPast" in "allPtNotes" to store aggregated notes.
allPtNotes$compiledAllText_currentAndPast = ""
counter = 1 # Initialize a counter to track the number of notes added during the loops.

# Loop through each unique "PAT_ENC_ID" in "allPtNotes".
for (n in 1:length(allPtNotes$PAT_ENC_ID)){
  # For each unique ID, loop through all rows in "allNotes" to find and append related notes.
  for (i in 1:length(allNotes$PAT_ENC_ID)){
    # Check if the current "PAT_ID" matches, and the "LastEditedInstant" is on or before the current encounter, 
    #    excluding notes of type "EKG" and "Labs".
    if (allPtNotes$PAT_ID[n] == allNotes$PAT_ID[i] & 
        allPtNotes$LastEditedInstant[n] >= allNotes$LastEditedInstant[i] &
        allNotes$Type[i] != "EKG" & allNotes$Type[i] != "Labs"){
      # Capture the currently aggregated text for the specific "PAT_ENC_ID".
      currentText = allPtNotes$compiledAllText_currentAndPast[n]
      # Append the new note to the aggregated text.
      allPtNotes$compiledAllText_currentAndPast[n] = paste0(currentText,
                                                            "#####################################",
                                                            "\nENCOUNTER NOTE #", counter, 
                                                            "\n", allNotes$compiledText[i], "\n\n\n")
      # Increment the counter.
      counter = counter + 1
    }
  }
  # Reset the counter for the next unique "PAT_ENC_ID".
  counter = 1
}

# Preview result:
# cat(allPtNotes$compiledAllText_currentAndPast[1])



################################################################################
################################################################################

# (1c) EKG_Interpretation: Collate EKG interpretation text associated with the current visit.

# Initialize an empty column "EKG_interpretation" in "currentVisitNotes" to store EKG interpretations.
currentVisitNotes$EKG_interpretation = ""
counter = 1 # Initialize a counter to track the number of EKG interpretations added during the loops.

# Loop through each unique "PAT_ENC_ID" in "currentVisitNotes". Note, if there are multiple EKGs, then this will result in multiple EKGs being provided to the LLM
for (n in 1:length(currentVisitNotes$PAT_ENC_ID)){
  # For each unique ID, loop through all rows in "allNotes" to find and append associated EKG interpretations.
  for (i in 1:length(allNotes$PAT_ENC_ID)){
    # Check if the current "PAT_ENC_ID" matches and the note type is "EKG".
    if (currentVisitNotes$PAT_ENC_ID[n] == allNotes$PAT_ENC_ID[i] &
        allNotes$Type[i] == "EKG"){
      # Capture the currently aggregated EKG interpretation for the specific "PAT_ENC_ID".
      currentText = currentVisitNotes$EKG_interpretation[n]
      # Append the new EKG interpretation to the aggregated interpretations.
      currentVisitNotes$EKG_interpretation[n] = paste0(currentText,
                                                       "#####################################",
                                                       "\n\n NOTE [#", counter, "] IN CURRENT ENCOUNTER",
                                                       "\n\n\n", allNotes$compiledText[i], "\n\n\n")
      # Increment the counter.
      counter = counter + 1
    }
  }
  # Reset the counter for the next unique "PAT_ENC_ID".
  counter = 1
}


################################################################################

# (1d) Duplicate rows for N simulated repeat trials for each patient (see research manuscript for details)

#Prepare overall df for results
results <- left_join(allPtNotes, select(currentVisitNotes, PAT_ENC_ID, compiledAllText_current, EKG_interpretation), by = "PAT_ENC_ID")

# Repeat each patient visit many-fold to study variability and determine mode:
# N: desired number of repeats (this is an adjustable parameter)
# NOTE: As an initial pre-test, we recommend starting with N = 1 before 
#     actual testing. This can confirm script is working as expected, not waste API
#     calls for troubleshooting, and also estimate the overall cost for when you
#     proceed to repeat trials for each patient's notes (e.g. N = 25)
N = 1
results <- results[rep(1:nrow(results), each = N), ]

# Create a new trialNum column and position it after PAT_ENC_ID
results <- results %>%
  group_by(PAT_ENC_ID) %>%
  mutate(trialNum = 1:N, .after = "PAT_ID") %>%
  ungroup()

# Randomize order of rows (only to spread burden of rate errors)
results <- results %>% sample_n(nrow(results), replace = FALSE)



################################################################################
################################################################################
################################################################################

### 2) Run GPT analysis with respective prompts on each text from step 1


################################################################################

# (2a) Prepare LLM settings

#Load prompts (adjust file path to the current version of the prompts you want to test, e.g., "FinalPrompts_V4.csv"):
prompts <- suppressWarnings(read.csv('/Users/conradsafranek/Documents/DECILE/Automated Heart Score via LLM/JAMA Submission/Supplemental/data/FinalPrompts_V4.csv', header = TRUE))


### Prepare GPT model settings:

# SELECT LLM MODEL (GPT-3.5-turbo-16k or GPT-4)
# Load preferred model with settings (uncomment out desired line, check price info for each model online)
# NOTE: Uncomment one of the two lines of code below to select model. Currently 
#   the code parallelizes for GPT-3.5 so that multiple queries can be efficiently 
#   tested. However, for GPT-4, at the time of conducting this research, the 
#   rate limits for GPT-4 were still too low for parallelization. In fact, even
#   without parallel queries, it is necessary to rate limit. See helperFunction 
#   for details (including explanation of n_future, which impacts parallelization).
#   We also include price information based on OpenAI's pricing (used later in 
#   pipeline; check online for udpated price information).
# Uncomment ONE of these two lines to select model:
MODEL <- "gpt-3.5-turbo-16k";   parallelizeYesNo = "Yes"; n_future = 5; input_price_per_1k_tokens <- 0.003;  output_price_per_1k_tokens <- 0.004;  
# MODEL <- "gpt-4"; parallelizeYesNo = "No"; input_price_per_1k_tokens <- 0.03;  output_price_per_1k_tokens <- 0.06;

# Only parallelize for gpt-3.5
if (parallelizeYesNo == "Yes"){
  #n_future = 5 
  plan(multisession, workers = n_future)
}

# Set model temperature (see documentation here: https://platform.openai.com/docs/api-reference/completions/create#completions/create-temperature and here: https://community.openai.com/t/cheat-sheet-mastering-temperature-and-top-p-in-chatgpt-api-a-few-tips-and-tricks-on-controlling-the-creativity-deterministic-output-of-prompt-responses/172683)
# More details on temperature also provided in the main research manuscript.
Temperature = 0.3

# Create data frame to store log of API rate limit error messages
logs_df <- data.frame(
  timestamp = character(),
  log_type = character(),
  message = character(),
  
  stringsAsFactors = FALSE
)

# Load the helper function file ("dataPipelineToQueryLLMs_helperFunction.R")
# See GitHub documentation and the helperFunction R file itself for details.
source("/Users/conradsafranek/Documents/DECILE/Automated Heart Score via LLM/JAMA Submission/Supplemental/code/dataPipelineToQueryLLMs_helperFunction.R")


################################################################################
################################################################################

# (2b) History

# Fetch relevant prompt for history
gptQuery_generalString <- prompts %>%
  filter(Step == "History") %>%
  pull(Prompt) %>%
  first()

#Prepare GPT query with encounter notes (only include current visit notes):
encounterNotes_list <- results$compiledAllText_current
results$historyQ <- glue(gptQuery_generalString, CurrentNotes = encounterNotes_list)
# cat(results$historyQ[1]) #Preview question

#Run GPT queries

# When MODEL = "GPT-4", the helperFunctions have code that will print monitoring information throughout the run, including ongoing remaining tokens/requests before hitting the rate limit, and any error messages along the way.
#See helperfunction file for more details
# Record the time before and after to track overall duration
histStartTime <- Sys.time()
if (parallelizeYesNo == "Yes"){
  results <- results %>% mutate(hist_gptResponse = future_map_chr(historyQ, call_chat_gpt, .progress = TRUE))
} else {
  results <- results %>% mutate(hist_gptResponse = map_chr(historyQ, call_chat_gpt, .progress = TRUE))
}
histEndTime <- Sys.time()
# Temporary save file included to save work along the way
write.csv(results, file = "/Users/conradsafranek/Desktop/results_temporaryProgressSave.csv")

# Check result:
#cat(results$hist_gptResponse[1]) #Preview result

################################################################################
################################################################################

# (2c) EKG

# Fetch relevant prompt for history
gptQuery_generalString <- prompts %>%
  filter(Step == "EKG") %>%
  pull(Prompt) %>%
  first()

#Prepare GPT query with encounter notes (only include EKG_interpretation):
encounterNotes_list <- results$EKG_interpretation
results$ekgQ <- glue(gptQuery_generalString, CurrentEKG = encounterNotes_list)
# cat(results$ekgQ[1]) #Preview question

### Run GPT queries
ekgStartTime <- Sys.time()
if (parallelizeYesNo == "Yes"){
  results <- results %>% mutate(ekg_gptResponse = future_map_chr(ekgQ, call_chat_gpt, .progress = TRUE))
} else {
  results <- results %>% mutate(ekg_gptResponse = map_chr(ekgQ, call_chat_gpt, .progress = TRUE))
}
ekgEndTime <- Sys.time()
write.csv(results, file = "/Users/conradsafranek/Desktop/results_temporaryProgressSave.csv")

# cat(results$ekg_gptResponse[1]) #Preview result

################################################################################
################################################################################

# (2d) Risks

# Fetch relevant prompt for risks
gptQuery_generalString <- prompts %>%
  filter(Step == "Risk_Factors") %>%
  pull(Prompt) %>%
  first()

#Prepare GPT query with encounter notes (only include current AND PAST visit notes):
encounterNotes_list <- results$compiledAllText_currentAndPast
results$risksQ <- glue(gptQuery_generalString, AllNotes = encounterNotes_list)
# cat(results$risksQ[1]) # Preview question

# Run GPT queries
risksStartTime <- Sys.time()
if (parallelizeYesNo == "Yes"){
  results <- results %>% mutate(risks_gptResponse = future_map_chr(risksQ, call_chat_gpt, .progress = TRUE))
} else {
  results <- results %>% mutate(risks_gptResponse = map_chr(risksQ, call_chat_gpt, .progress = TRUE))
}
risksEndTime <- Sys.time()
write.csv(results, file = "/Users/conradsafranek/Desktop/results_temporaryProgressSave.csv")

# cat(results$risks_gptResponse[1]) #Preview result


################################################################################

# Undo the earlier randomization of row order (order "results" by PAT_ENC_ID and then by trialNum
results <- arrange(results, PAT_ENC_ID, trialNum)



################################################################################
################################################################################
################################################################################

### (3) Calculate heart score

################################################################################

# (3a) Extract bracketed answers from GPT responses

# For functions to extract text, see helperFunction file

# Extract individual subscores from GPT responses for Hist, EKG, and Risks:
results$histScore <- sapply(results$hist_gptResponse, extract_number_from_brackets)
results$ekgScore <- sapply(results$ekg_gptResponse, extract_number_from_brackets)
results$risksScore <- sapply(results$risks_gptResponse, extract_number_from_brackets)


################################################################################
################################################################################

# (3b) Calculate Trop and Age subscores from parsing structured data in the synthetic EHR data extract


#Calculate age subscore from structured data in allNotes:
# Filter the relevant rows from allNotes
relevant_ages <- allNotes[allNotes$Type == "Labs", c("PAT_ENC_ID", "Age")]
# Calculate age sub-score
relevant_ages$ageScore <- ifelse(relevant_ages$Age < 45, 0,
                                 ifelse(relevant_ages$Age < 65, 1, 2))
# Merge with results dataframe
results <- merge(results, relevant_ages[, c("PAT_ENC_ID", "ageScore")], by = "PAT_ENC_ID", all.x = TRUE)


#Calculate troponin subscore from structured data in allNotes:
# Filter relevant rows from allNotes for troponin and normal max values
relevant_trop <- allNotes[allNotes$Type == "Labs", c("PAT_ENC_ID", "InitialTroponin", "NormalMax")]
# Calculate troponin sub-score
relevant_trop$tropScore <- ifelse(relevant_trop$InitialTroponin <= relevant_trop$NormalMax, 0,
                                  ifelse(relevant_trop$InitialTroponin <= (3 * relevant_trop$NormalMax), 1, 2))
# Merge with results dataframe
results <- merge(results, relevant_trop[, c("PAT_ENC_ID", "tropScore")], by = "PAT_ENC_ID", all.x = TRUE)

################################################################################
################################################################################

# (3c) Calculate HEART scores


# Calculate the overall heart score based on summed subscores.
results <- results %>%
  # Apply operations to each row individually.
  rowwise() %>%
  # Create a new 'heartSCORE' column.
  mutate(
    heartSCORE = {
      # Collect subscores for the current row.
      cols <- c(histScore, ekgScore, risksScore, ageScore, tropScore)
      # If all values in 'cols' are numeric, sum them; otherwise, assign NA.
      if (all(!is.na(suppressWarnings(as.numeric(cols))))) {
        sum(as.numeric(cols), na.rm = TRUE)
      } else {
        NA_real_
      }
    }
  )


# Rearrange columns so scores are more to the left of the dataframe:
results <- results %>%
  select(
    PAT_ENC_ID, PAT_ID, trialNum, 
    histScore, ekgScore, risksScore, ageScore, tropScore, heartSCORE, 
    everything()
  )



################################################################################
################################################################################
################################################################################


### 4) Calculate meta statistics (price and run-time)


################################################################################

# PRICE

# Already selected model above. However, If you want to check the hypothetical prices 
#     for a different model, uncomment the associated price row below:
# MODEL <- "gpt-3.5-turbo-16k"; input_price_per_1k_tokens <- 0.003;  output_price_per_1k_tokens <- 0.004; 
# MODEL <- "gpt-4"; input_price_per_1k_tokens <- 0.03;  output_price_per_1k_tokens <- 0.06;


# Calculate price via the 3 subscore prompts and responses:

# Source the Python script with tokenizer function (currently only Python offers a package, "tiktoken", to tokenize text in the same format as OpenAI's LLM tokenizers)
#     This "source_python" runs a brief Python script that includes a tokenizer function.
# reticulate::py_install("tiktoken") # The first time running this script, may need to install the package (but may work without this)
# Also may need to run "pip install --upgrade tiktoken" in terminal; see https://github.com/openai/tiktoken/blob/main/README.md; or https://github.com/openai/openai-cookbook/blob/main/examples/How_to_count_tokens_with_tiktoken.ipynb)
source_python("/Users/conradsafranek/Documents/DECILE/Automated Heart Score via LLM/JAMA Submission/Supplemental/code/tokenizerFunction_PythonScript.py")

# Step 1: Create price_df
price_df <- results %>%
  select(PAT_ENC_ID, PAT_ID, historyQ, ekgQ, risksQ, hist_gptResponse, ekg_gptResponse, risks_gptResponse) %>%
  group_by(PAT_ID) %>%
  slice(1) %>%
  ungroup()

# Step 2: Calculate tokens for each column
text_columns <- c("historyQ", "ekgQ", "risksQ", "hist_gptResponse", "ekg_gptResponse", "risks_gptResponse")
token_columns <- paste0(text_columns, "_tokens")
for(i in seq_along(text_columns)) {
  price_df <- price_df %>%
    rowwise() %>%
    mutate(!!token_columns[i] := num_tokens_from_string(!!sym(text_columns[i])))
}

# Step 3: Calculate price for each column
input_columns <- c("historyQ_tokens", "ekgQ_tokens", "risksQ_tokens")
output_columns <- c("hist_gptResponse_tokens", "ekg_gptResponse_tokens", "risks_gptResponse_tokens")
for(col in input_columns) {
  price_df <- price_df %>%
    mutate(!!paste0(col, "_price") := !!sym(col) / 1000 * input_price_per_1k_tokens)
}
for(col in output_columns) {
  price_df <- price_df %>%
    mutate(!!paste0(col, "_price") := !!sym(col) / 1000 * output_price_per_1k_tokens)
}

# Step 4: Calculate total input and output price for each row
price_df <- price_df %>%
  rowwise() %>%
  mutate(
    total_input_price = sum(c_across(ends_with("_tokens_price"))),
    total_output_price = sum(c_across(ends_with("gptResponse_tokens_price")))
  ) %>%
  ungroup()

overall_total_input_price <- sum(price_df$total_input_price) * N 
overall_total_output_price <- sum(price_df$total_output_price) * N
overall_total_subscore_price <- overall_total_input_price + overall_total_output_price

print(paste("Overall total input price for subscore calculations across", nrow(results), 
            " rows is: [ $", round(ceiling(overall_total_input_price * 100) / 100, 2), "]"))
print(paste("Overall total output price for subscore calculations across", nrow(results), 
            " rows is: [ $", round(ceiling(overall_total_output_price * 100) / 100, 2), "]"))
print(paste("Overall GRAND TOTAL for subscore calculations across", nrow(results), 
            " rows is: [ $", round(ceiling(overall_total_subscore_price * 100) / 100, 2), "]"))
print(paste("Price per individual row: [ $", round(overall_total_subscore_price / nrow(results), 4), "]"))


# Documentation on TOKENS: https://platform.openai.com/docs/guides/gpt/managing-tokens


################################################################################
################################################################################

# RUN TIME

histDuration <- histEndTime - histStartTime; aveTimePer_histQ <- histDuration/nrow(results)
ekgDuration <- ekgEndTime - ekgStartTime; aveTimePer_ekgQ <- ekgDuration/nrow(results)
risksDuration <- risksEndTime - risksStartTime;  aveTimePer_risksQ <- risksDuration/nrow(results)
totalDuration <- histDuration + ekgDuration + risksDuration;  aveTimePer_totalRowQueries <- totalDuration/nrow(results)  # + onepassDuration

# Print run times
cat("Duration for History: ", histDuration, " seconds;  Average time per History query: ", aveTimePer_histQ, " seconds\n")
cat("Duration for EKG: ", ekgDuration, " seconds;  Average time per EKG query: ", aveTimePer_ekgQ, " seconds\n")
cat("Duration for Risks: ", risksDuration, " seconds;  Average time per Risks query: ", aveTimePer_risksQ, " seconds\n")
cat("Total run-time: ", totalDuration, " seconds;  Average time per all queries in each row: ", aveTimePer_totalRowQueries, " seconds\n")



################################################################################
################################################################################
################################################################################

# 5) Examine results (Count of NA, Variability of responses):

################################################################################


# BAR GRAPH - Count non-numeric values for each score by PAT_ID


# Function to determine if value is non-numeric
is_non_numeric <- function(x) {return(is.na(x) | !is.na(as.character(x)) & is.na(as.numeric(as.character(x))))}

# Count non-numerical results for each subscore
non_numeric_counts <- results %>% summarise(
  histScore = sum(is.na(as.numeric(histScore))),
  ekgScore = sum(is.na(as.numeric(ekgScore))),
  risksScore = sum(is.na(as.numeric(risksScore))),
  ageScore = sum(is.na(as.numeric(ageScore))),
  tropScore = sum(is.na(as.numeric(tropScore))),
  heartSCORE = sum(is.na(as.numeric(heartSCORE)))
)

# Count NAs and non-numeric values for each score by PAT_ID
na_nonnum_counts_by_pat <- results %>%
  gather(key="score", value="value", histScore, ekgScore, risksScore, ageScore, tropScore, heartSCORE) %>% 
  group_by(score, PAT_ID) %>%
  summarize(count_invalid = sum(is_non_numeric(value)))

# Setting order
ordered_names <- names(non_numeric_counts)
na_nonnum_counts_by_pat$score <- factor(na_nonnum_counts_by_pat$score, levels = ordered_names)

# Create a stacked bar chart
ggplot(na_nonnum_counts_by_pat, aes(x=score, y=count_invalid, fill=PAT_ID)) + 
  geom_bar(stat="identity", position="stack") + #add color outline:  color="gray60", size=0.3
  scale_fill_brewer(palette="Pastel1") +
  labs(title = paste("Count of NA Results by Score by Patient   [N repeats per pt =", N, "]"),
       x="Score Type", 
       y="Count of Non-numerical and NA Results") +
  theme_minimal() +
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=12)) 


################################################################################
################################################################################

# BOX PLOT - Score Distribution

data_long_values <- gather(results, key="score", value="value", histScore, ekgScore, risksScore, heartSCORE)  # , ageScore, tropScore, heartSCORE #, onepass_histScore, onepass_ekgScore, onepass_risksScore, onepassHeartSCORE

# Ensure the data is numeric
data_long_values$value <- as.numeric(data_long_values$value)

# Setting order
data_long_values$score <- factor(data_long_values$score, levels = ordered_names)

# Calculate counts of numeric values for each score by PAT_ID
numeric_counts <- data_long_values %>%
  filter(!is.na(value) & !is.character(value)) %>%
  group_by(score, PAT_ID) %>%
  tally(name = "count")

# Create boxplot with adjusted outline and count labels
ggplot(data_long_values, aes(x=score, y=value, fill=PAT_ID)) + 
  geom_boxplot(color="gray45", lwd=0.3) + 
  geom_text(data=numeric_counts, aes(y=Inf, label=paste0("n=", count)), 
            position=position_dodge(width=0.75), vjust=2, size=3) +  # Place count label above each box
  scale_fill_brewer(palette="Dark2") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  # Ensure y-axis breaks are integers
  labs(title="Score Distribution by Patient", 
       x="Score Type", 
       y="Value") +
  theme_minimal() +
  theme(legend.position="top")

################################################################################
################################################################################

# JITTER PLOT - Score Distribution

ggplot(data_long_values, aes(x=score, y=value, color=PAT_ID)) + 
  geom_jitter(position=position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), 
              size=1, alpha=1) +  # Adjusted size
  geom_text(data=numeric_counts, aes(y=Inf, label=paste0("n=", count)), 
            position=position_dodge(width=0.75), vjust=1, size=2, show.legend=FALSE) +  # Added show.legend=FALSE
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_brewer(palette="Dark2") +    # # Lighter colors  # versus (but this is specific to 4 patients: scale_color_manual(values = c("#A93226", "#21618C", "#A8D5BA", "#e993c8")) +
  labs(title="Score Jitter Plot by Patient", 
       x="Score Type", 
       y="Value") +
  theme_minimal() +
  theme(legend.position="top", legend.title = element_blank()) # , axis.text.x = element_text(angle = 10, hjust = 1)


################################################################################
################################################################################

# MODE SCORE

# Convert scores in results to numeric or NA
cols_to_convert <- c("histScore", "ekgScore", "risksScore", "ageScore", "tropScore", "heartSCORE") 
resultsForMODE <- results
resultsForMODE[cols_to_convert] <- lapply(results[cols_to_convert], function(x) {
  num_values <- as.numeric(x)
  num_values[is.na(num_values)] <- NA
  return(num_values)
})

# Create the long-format data frame
new_data_long <- resultsForMODE %>%
  select(PAT_ID, histScore, ekgScore, risksScore, ageScore, tropScore, heartSCORE) %>% # , onepass_histScore, onepass_ekgScore, onepass_risksScore, onepassHeartSCORE
  pivot_longer(cols = -PAT_ID, names_to = "score", values_to = "value")

# Get mode
get_mode <- function(v) {
  uniqv <- unique(v)
  mode_val <- uniqv[which.max(tabulate(match(v, uniqv)))]
  return(mode_val)
}

mode_data <- new_data_long %>%
  group_by(score, PAT_ID) %>%
  summarise(mode = get_mode(value)) %>%
  ungroup()

# Adjust the factor levels for PAT_ID to make sure pt2 appears below pt1
mode_data$PAT_ID <- factor(mode_data$PAT_ID, levels = c("pt4", "pt3", "pt2", "pt1"))
mode_levels <- sort(unique(mode_data$mode[!is.na(mode_data$mode)]))

# Separate the color mapping for NA and other values
mode_data$fill_color <- as.factor(ifelse(is.na(mode_data$mode), "lightgray", as.factor(mode_data$mode)))

# Create the new color mapping, excluding NA
# Note, green is not correlated with true ratings, but rather just to more easily visualize the data
colors <- scales::seq_gradient_pal("white", "#8FBC8F")(seq(0, 1, length.out = length(mode_levels))) 
colors <- c(colors, "lightgray")


# Create heatmap with the new ordering and color mapping
ggplot(mode_data, aes(x = factor(score, levels = cols_to_convert), y = PAT_ID, fill = fill_color)) + 
  geom_tile(color = "gray40") +
  geom_text(aes(label = ifelse(is.na(mode), "NA", mode)), color = "black", size = 4) +
  scale_fill_manual(values = colors) +
  labs(title = "Mode of Subscores by Patient", 
       x = "Score Type", 
       y = "Patient ID") +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank())


################################################################################
################################################################################
################################################################################

# Save a "screenshot" of your results


# Get current date/time
current_datetime <- format(Sys.time(), "%m-%d_%Hh%Mm%Ss")

# Current prompt version
promptVersion <- prompts$VERSION[1]

# Combine details to create a filename
filename <- paste0("/Users/conradsafranek/Desktop/results_", current_datetime, "_prompt", promptVersion, "_", MODEL, ".RData")

# Save the current R environment/workspace to the specified file
save.image(filename)

# Also save results specifically
filename <- paste0("/Users/conradsafranek/Desktop/results_", current_datetime, "_prompt", promptVersion, "_", MODEL, ".csv")
write.csv(results, file = filename)


# To load an old saved environment, e.g., to examine mode of old test runs
# load("/Users/conradsafranek/Documents/DECILE/Automated\ Heart\ Score\ via\ LLM/Final\ Results/results_08-31_10h48m57s_promptV3_gpt-3.5-turbo-16k.RData")





