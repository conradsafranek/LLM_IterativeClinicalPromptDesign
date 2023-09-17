

# R script associated with: "Automated HEART Score Determination via ChatGPT: Honing a Framework for Iterative Prompt Development"

# File name: "dataPipelineToQueryLLMs_helperFunction.R" 
# Please see README file on GitHub repository for description: 
# https://github.com/conradsafranek/LLM_IterativeClinicalPromptDesign


# This code include a number of helper functions for the main script. Most 
#    importantly, it includes functions to manage the calls to OpenAI (see details below)


################################################################################

# Insert your OpenAI Key here (see GitHub README for instructions on how to 
#     generate your personal API key)
OPENAI_API_KEY <- "xx-xxxxxxxxxx"

# Note: For caution, consider setting a limit on max $$ usage amount: https://platform.openai.com/account/billing/limits
#       This way if there is a coding mistake that runs more API calls than
#       intended, it will hit the $ limit and stop the run. However, also don't
#       set the limit so low that it runs out mid query for an intended run.

################################################################################

# Logging function for errors 
add_log <- function(log_type, message) {
  new_entry <- data.frame(
    timestamp = Sys.time(),
    log_type = log_type,
    message = message,
    stringsAsFactors = FALSE
  )
  logs_df <<- rbind(logs_df, new_entry)
}

################################################################################
################################################################################

# Functions to make API calls to OpenAI LLMs:

# --------------------------------------------------------------
# Function Strategy Summary:
# 
# 1. `chat_completion`:
#    - Responsible for making the API request to OpenAI's ChatGPT.
#    - Monitors rate limits for the API:
#        - If the remaining requests or tokens are close to being exhausted, it waits until the rate limits reset.
#            - Currently is set to 5000 tokens, which is the longest combined prompt + expected response. May need to adjust depending on note length.
#        - After waiting, it retries the API request.
#    - Handles HTTP errors (e.g., 4xx, 5xx) by checking the response status and raising an error if necessary.
#    - Parses the content of the response and returns the desired message content.
# 
# 2. `call_chat_gpt`:
#    - Manages the retrying mechanism for the `chat_completion` function.
#    - Utilizes `tryCatch` to handle potential errors in `chat_completion`.
#    - If an error is caught, it logs the error and waits for a short duration before retrying.
#    - Continues retrying until the maximum number of retries (`max_retries`) is reached. Includes "exponential back-off" where the wait duration increases exponentially
#    - If all retries are exhausted and the request is still unsuccessful, it logs a max retries error and returns a related error message.
# 
# The combination of these functions ensures that API requests are made efficiently while managing potential issues related to rate limits and other errors.
# --------------------------------------------------------------


# Function to make the API call and handle rate limits
chat_completion <- function(message, counter_QueryAttemptNum) {
  
  Sys.sleep(0)
  
  url <- paste0("https://api.openai.com/v1/chat/completions")
  
  headers <- add_headers(
    `Content-Type` = "application/json",
    `Authorization` = paste("Bearer", OPENAI_API_KEY)
  )
  
  body <- list(
    model = MODEL,
    messages = list(
      list(role = "user", content = message)
    ),
    temperature = Temperature
  )
  
  response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
  
  # Check rate limits
  remaining_requests <- as.numeric(headers(response)[["x-ratelimit-remaining-requests"]])
  remaining_tokens <- as.numeric(headers(response)[["x-ratelimit-remaining-tokens"]])
  
  # Print the status
  if (parallelizeYesNo == "No"){
    print(paste("Remaining Requests:", remaining_requests))
    print(paste("Remaining Tokens:", remaining_tokens))
  }
  
  # If rate limits are close to being exhausted, wait
  # 5000 is a key value here, it should theoretically be longer than the longest note + expected response
  if (remaining_requests <= 5 || remaining_tokens <= 4500) {
    reset_requests <- as.numeric(gsub("s$", "", headers(response)[["x-ratelimit-reset-requests"]]))
    reset_tokens <- as.numeric(gsub("s$", "", headers(response)[["x-ratelimit-reset-tokens"]]))
    
    # Print the system pause
    if (parallelizeYesNo == "No"){
      print(paste("####################### SYSTEM PAUSE #######################"))
      print(paste("Remaining Requests:", remaining_requests))
      print(paste("Remaining Tokens:", remaining_tokens))
      print(paste("Reset Time Requests:", reset_requests))
      print(paste("Reset Time Tokens:", reset_tokens))
      print(paste("counter_QueryAttemptNum = ", counter_QueryAttemptNum))
      print(paste("SLEEPING FOR:", max(reset_requests, reset_tokens, na.rm = TRUE) * (2^counter_QueryAttemptNum)))
    }
    
    # Add to log
    add_log("SYSTEM PAUSE", sprintf("Time to Reset Requests (seconds): %.0f, Time to Reset Tokens (seconds): %.0f", reset_requests, reset_tokens))
    
    # Wait for the longer of the two reset times (if only one rate limit being exhausted, it will be selected because the other will be NA)
    # ALSO added "exponential backoff" where with the 2^i where i is the retry number from i : max_retries. See documentation on exponential back-off here: https://platform.openai.com/docs/guides/rate-limits/error-mitigation
    Sys.sleep(max(reset_requests, reset_tokens, na.rm = TRUE) * (2^counter_QueryAttemptNum))
    
    # Throw an error to let call_chat_gpt handle the retry
    stop("Rate limits close to exhaustion. Awaiting reset.")
  }
  
  # Error handling for bad responses
  if (http_error(response)) {
    stop("HTTP error: ", http_status(response)$message)
  }
  
  # Log the raw response (for troubleshooting)
  # if (parallelizeYesNo == "No"){
  #   print("Attempting to log raw response...")
  #   print(toString(rawToChar(response$content)))
  # }
  
  add_log("RAW_RESPONSE", toString(rawToChar(response$content)))
  
  content <- content(response, "parsed")
  return(content$choices[[1]]$message$content)
}

# Function to manage retries
call_chat_gpt <- function(gpt_request, max_retries = 3) {
  
  # Not currently implementing, but could add staggered sleep here to prevent rate error from rapid-fire simultaneous requests during parallelization (future_map_chr)
  # This sleep is just a buffer (15secs adds 40 total min to run time for 4 pt notes with 10 repeat rows each and 4 queries per row) 
  # NOT adding this here anymore, will do exponential backoff
  Sys.sleep(0) 
  
  #Reset:
  counter_QueryAttemptNum = 1
  
  # Loop to attempt API call up to max_retries times
  for (counter_QueryAttemptNum in 1:max_retries) {
    response <- tryCatch({
      # Make the API call
      chat_completion(gpt_request, counter_QueryAttemptNum)
    }, error = function(e) {
      
      if (parallelizeYesNo == "No"){
        # Log and print any errors encountered during the API call
        print(paste("####################### ERROR ATTEMPTS #######################"))
        
        # print(paste("Error on attempt #", counter_QueryAttemptNum, " for request query [[[", gpt_request, "]]] ERROR message: ", e$message)) # This line is helpful if you want to see the specific call that led to the error. However, it makes the printing of error tracking a bit messy so I prefer the following line
        print(paste("Error on attempt #", counter_QueryAttemptNum, " for request query. ERROR message: ", e$message))
      }
      
      add_log("WARNING", sprintf("Error on attempt %d for request '%s': %s", counter_QueryAttemptNum, gpt_request, e$message))
      return(NULL)  # Return NULL on error
    })
    
    # If a valid response is received, return it
    if (!is.null(response)) {
      return(response)
    } 
    # If max retries are reached without a valid response, log an error and return an error message
    else if (counter_QueryAttemptNum == max_retries) {
      
      if (parallelizeYesNo == "No"){
        print(paste("####################### MAX RETRIES #######################"))
      }
      
      add_log("ERROR", sprintf("Max retries reached for request '%s'", gpt_request))
      return(paste("Error: Max retries reached for request", gpt_request))
      
    }
  }
}

# --------------------------------------------------------------
# Important notes about Parallelization:
# 
# Parallelization in this context refers to making multiple API calls simultaneously to speed up the process. 
# Whether or not you're parallelizing is determined by using `map_chr` (no parallelization) vs `future_map_chr` (parallelization). 
# The degree of parallelization (i.e., how many calls are made simultaneously) is set by the `n_future` parameter, which defines the number of workers.
# 
# When parallelizing API calls using the `future_map_chr` function:
#    - Error logging seems to experience inconsistencies, potentially due to the simultaneous writing to the `logs_df`.
#    - Some truncation issues have been observed with `gpt-3.5-turbo-16k` responses. These issues appear more frequently when the parallelization level (`n_future`) is set high (e.g., 20). 
#      However, these truncations occur even when neither rate limits nor max tokens are being approached. We believe it's an error with the current the OpenAI API.
#    - # We suspect these truncation issues with `gpt-3.5-turbo-16k` might be temporary, and OpenAI is likely to improve the reliability and error messaging of their API in the coming months.
#    - For the `gpt-3.5-turbo-16k` model, setting `n_future` to a lower value, such as 4, appears to mitigate these truncation issues and is recommended.
#    - For the GPT-4 model, given its current rate limits, parallelization is not recommended. Instead, using `map_chr` directly is advised to avoid rate limit violations.
#    - Also note that there is a Python script available that dynamically throttles the rate of API queries based on rate limits. This is similar to what is accomplished in this code, but it is slightly more advanced and may handle error logging more consistently even with parallelization. (available at: https://github.com/openai/openai-cookbook/blob/main/examples/api_request_parallel_processor.py)
#
# --------------------------------------------------------------




################################################################################
################################################################################
################################################################################


# Functions to extract text from brackets:

# 1)     Extract ONLY the last bracketed phrase from a string
extract_bracketed_info <- function(input_string) {
  # Extract all bracketed phrases using regex
  bracketed_info <- str_extract_all(input_string, "\\[[^\\]]+\\]")[[1]]
  
  # Check if any bracketed phrases were found
  if (length(bracketed_info) > 0) {
    # Return the last bracketed phrase
    return(tail(bracketed_info, 1))
  } else {
    # If no bracketed phrases were found, return NA
    return("ERROR - No bracketed phrase")
  }
}

# 2)     Extract the number from the parentheses within the bracketed information
extract_number_from_brackets <- function(input_string) {
  # Get the bracketed information using the previous function
  bracketed_info <- extract_bracketed_info(input_string)
  
  # Check if the bracketed information contains "[Not enough information..."
  if (grepl("\\[Not enough information", bracketed_info)) {
    return("ERROR - Not enough info")
  }
  
  # Extract the number from within the parentheses using regex
  number_within_parentheses <- str_match(bracketed_info, "\\((\\d+)\\)")[, 2]
  
  # If the number is found, return it; otherwise, return "ERROR"
  if (length(number_within_parentheses) > 0) {
    return(number_within_parentheses)
  } else {
    return("ERROR - No number")
  }
}



################################################################################
################################################################################

