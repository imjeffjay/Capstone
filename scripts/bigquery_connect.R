# Load required libraries
library(bigrquery)
library(DBI)
library(here)

# Set authentication options
options(httr_oauth_cache = TRUE)

# Define the path to the service account JSON file
json_path <- here::here("credentials", "msds-795-3b7180d11ca3.json")

# Authenticate with Google BigQuery
if (file.exists(json_path)) {
  bq_auth(path = json_path)  # Use service account if available
  message("Successfully authenticated using service account.")
} else {
  message("Service account JSON not found. Switching to manual authentication.")
  bq_auth()  # Prompt user login if service account is missing
}

# Function to connect to a BigQuery dataset
connect_to_bigquery <- function(project, dataset, billing_project) {
  tryCatch({
    con <- dbConnect(
      bigrquery::bigquery(),
      project = project,
      dataset = dataset,
      billing = billing_project
    )
    message("Connected to BigQuery dataset: ", dataset)
    return(con)
  }, error = function(e) {
    message("Connection failed: ", e$message)
    return(NULL)
  })
}

# Connect to the public MIMIC-IV hospital dataset
con_hosp <- connect_to_bigquery(
  project = "physionet-data", 
  dataset = "mimiciv_3_1_hosp", 
  billing_project = "msds-795"
)

# Connect to the private MIMIC-IV derived dataset
con_derived <- connect_to_bigquery(
  project = "msds-795", 
  dataset = "mimiciv_derived", 
  billing_project = "msds-795"
)

# Verify active connections
if (!is.null(con_hosp)) message("Connection to `mimiciv_3_1_hosp` is active.")
if (!is.null(con_derived)) message("Connection to `mimiciv_derived` is active.")
