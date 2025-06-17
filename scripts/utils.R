# Load necessary package
library(here)


######### Data ################

# save a dataset
save_dataset <- function(data, filename, subdir = "data/processed") {
  # Ensure the directory exists
  dir.create(here(subdir), recursive = TRUE, showWarnings = FALSE)
  
  # full file path
  file_path <- here(subdir, filename)
  
  #Save dataset
  saveRDS(data, file = file_path)
  
  message("Dataset saved to: ", file_path)
}

# load a dataset
load_dataset <- function(filename, subdir = "data/processed") {
  # Construct full file path
  file_path <- here(subdir, filename)
  
  if (!file.exists(file_path)) {
    stop("Error: File does not exist - ", file_path)
  }
  data <- readRDS(file_path)
  message("Dataset loaded from: ", file_path)
  return(data)
}

# Clear Data

clear_data_objects <- function() {
  suppressWarnings({
    rm(data_hosp, data_ICU, data_cms, envir = .GlobalEnv)
  })
  gc(verbose = FALSE)
  message("Cleared data_hosp, data_ICU, data_cms from memory.")
}

######### Figures ############


save_named_plot <- function(plot_expr, filename, folder = "../presentations/figures", width = 8, height = 6, dpi = 300) {
  full_path <- file.path(folder, paste0(filename, ".png"))
  ggsave(full_path, plot = plot_expr, device = "png", width = width, height = height, dpi = dpi)
  message("Saved: ", full_path)
}



########  Models  ############

save_models <- function(models, subfolder = NULL, clear = FALSE) {
  dir_path <- here::here("models", subfolder)
  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  
  for (name in names(models)) {
    file_path <- file.path(dir_path, paste0(name, ".rds"))
    saveRDS(models[[name]], file = file_path)
    
    if (clear) {
      rm(list = name, envir = .GlobalEnv)
    }
  }
  
  message("Saved models in: ", dir_path)
}

# Load models from a subfolder inside /models/
load_models <- function(subfolder = NULL) {
  dir_path <- here::here("models", subfolder)
  files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
  models <- lapply(files, readRDS)
  names(models) <- sub("\\.rds$", "", basename(files))
  models
}

load_models_to_list <- function(subfolder, pattern = NULL, strip_train = TRUE) {
  dir_path <- here::here("models", subfolder)
  files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)
  
  if (!is.null(pattern)) {
    files <- files[grepl(pattern, basename(files))]
  }
  
  if (length(files) == 0) {
    message("No matching .rds files found in: ", dir_path)
    return(list())
  }
  models <- list()
  for (file in files) {
    model <- readRDS(file)
    if (strip_train && !is.null(model$train_data)) model$train_data <- NULL
    name <- sub("\\.rds$", "", basename(file))
    models[[name]] <- model
    message("Loaded model: ", name)
  }
    return(models)
}


