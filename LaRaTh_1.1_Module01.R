################################################################################
#                                                                              #
#           LaRaTh-Br - Landslide Rainfall Thresholds Brazil 1.1               #
#                                                                              #
#                Copyright (C) 2024  Rodrigo Augusto Stabile                   #
#                                                                              #
#                   E-mail: rodrigo.stabile@cemaden.gov.br                     #
#                                                                              #
#                       Phone: +55 (12) 3205-0126/0127                         #
#                                                                              #
# Adress: Centro Nacional de Monitoramento e Alertas de Desastres Naturais,    #
# Cemaden. Estrada Doutor Altino Bondesan, 500 - Distrito de Eugênio de Melo.  #
# São José dos Campos/SP, Brazil. CEP:12.247-016.                              #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
#                         SOFTWARE REGISTRATION                                #
#                                                                              #
#              Brazilian National Institute of Industrial Property             #
#                    (INPI): Registration No. BR512024001508-6                 #
#                         Registration date: 08 May 2024                       #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
#                                 LICENSE                                      #
#                                                                              #
#      This program was developed using R version 4.3.1 (2023-06-16 ucrt)      #
#                       URL https://www.r-project.org/                         #
#                                                                              #
#     This program is free software: you can redistribute it and/or modify     #
# it under the terms of the GNU Affero General Public License as published by  #
#      the Free Software Foundation, either version 3 of the License, or       #
#                   (at your option) any later version.                        #
#                                                                              #
#      This program is distributed in the hope that it will be useful,         #
#       but WITHOUT ANY WARRANTY; without even the implied warranty of         #
#        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#             GNU Affero General Public License for more details.              #
#                                                                              #
#   You should have received a copy of the GNU Affero General Public License   #
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.   #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
#                             USEFUL INFORMATION                               #
#                                                                              #
#    The code has been structured into modules and steps. Each module has an   #
# objective and can be executed separately provided that the necessary input   #
#           data is available. The steps within each module should             #
#                        be executed sequentially.                             #
#                                                                              #
#  The code has been structured for the rainfall and landslide databases from  #
#   Cemaden/Brazil, but it can be adapted for different databases as long as   #
# they contain the minimum information required for the calculation of the     #
#                                   steps.                                     #
#                                                                              #
#    It is recommended that the databases be adapted with column names and     #
#         similar information to the examples attached to this code.           #
#                                                                              #
################################################################################

#---------------------------------- MODULE 1 ----------------------------------# 
#---------------------- Determination of Rainfall Events ----------------------#

##### STEP 1 - JOIN AND ORGANIZE RAINFALL DATA SPREADSHEETS #####

# Install required packages if they are not already installed
required_packages <- c("dplyr", "readr", "tidyr", "lubridate", "shiny", "zoo", "stringr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load the required packages
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(shiny)
library(zoo)
library(stringr)

##### DATA TEMPORAL RESOLUTION #####
# Define the temporal resolution of the rainfall data (in minutes)
# CHANGE THIS VALUE if your data has a different resolution (e.g., 15 min, 30 min, 60 min)
dt_min <- 10                      # Temporal resolution in minutes
dt_hr  <- dt_min / 60             # Convert to hours for calculations

cat("Temporal resolution set to:", dt_min, "minutes (", dt_hr, "hours)\n")

# Function to translate column names from Portuguese to English
translate_column_names <- function(df) {
  portuguese_to_english <- c(
    "datahora" = "datetime",
    "valorMedida" = "measure_value",
    "valor" = "measure_value",
    "codEstacao" = "station_code",
    "codestacao" = "station_code",
    "nomeEstacao" = "station_name",
    "municipio" = "municipality",
    "cidade" = "municipality",
    "uf" = "state",
    "latitude" = "latitude",
    "longitude" = "longitude",
    "id_sensor" = "sensor_id",      # mapeia se existir
    "idsensor" = "sensor_id",
    
    # Additional aliases
    "date" = "datetime",
    "timestamp" = "datetime",
    "rainfall" = "measure_value",
    "precipitation" = "measure_value",
    "station_id" = "station_code"
  )
  
  translated_names <- c()
  for (col_name in names(df)) {
    translated_names <- c(translated_names, ifelse(col_name %in% names(portuguese_to_english), portuguese_to_english[col_name], col_name))
  }
  names(df) <- translated_names
  return(df)
}

# Function to automatically detect the CSV delimiter
detect_delimiter <- function(file) {
  
  first_line <- readLines(file, n = 1)
  
  semicolon_count <- stringr::str_count(first_line, ";")
  comma_count <- stringr::str_count(first_line, ",")
  
  if (semicolon_count > comma_count) {
    return(";")
  } else {
    return(",")
  }
}

# Informative message in the directory selection window
message2 <- "Choose the directory where the CSV files of the rainfall data are located"

# Ask the user for the directory where the rainfall data spreadsheets are located
directory <- choose.dir(default = "C:/Users/Public", caption = message2)

# List all files in the directory with the .csv extension
files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store the data frames
df_list <- list()

# Iterate over the files and read the data frames, adding them to the list
for (file in files) {
  
  # Automatically detect the delimiter
  detected_delimiter <- detect_delimiter(file)
  
  # Print the detected delimiter in the console
  cat("Detected delimiter for", basename(file), ":", detected_delimiter, "\n")
  
  # Read the file using the detected delimiter
  df <- read_delim(file, delim = detected_delimiter, col_types = cols())
  
  # Translate column names if they are in Portuguese
  if (any(names(df) %in% c("datahora", "valorMedida", "codEstacao", "nomeEstacao", "municipio", "uf", "latitude", "longitude", "id_sensor", "idsensor"))) {
    df <- translate_column_names(df)
  }
  
  df_list[[basename(file)]] <- df
}

# Combine all data frames into a single data frame
combined_rainfall <- bind_rows(df_list, .id = "Source")
rm(df_list)

# Check the column names
names(combined_rainfall)

##### OPTIONAL: FILTER BY SENSOR ID (if column exists) #####
# Some datasets contain multiple sensors per station (e.g., sensor 10 = rainfall accumulation every 10 min)
# We keep only sensor_id == 10 if the column exists.
if("sensor_id" %in% names(combined_rainfall)) {
  cat("Column 'sensor_id' found. Filtering to keep only sensor_id == 10 (rainfall accumulation).\n")
  combined_rainfall <- combined_rainfall %>%
    filter(sensor_id == 10)
  # Remove the sensor_id column as it is no longer needed
  combined_rainfall <- combined_rainfall %>%
    select(-sensor_id)
} else {
  cat("Column 'sensor_id' not found. Assuming all records are rainfall accumulation.\n")
}

# Create a municipality selection interface
selected_municipalities <- shiny::selectInput(
  "municipalities", 
  "Select municipalities of interest:", 
  choices = unique(combined_rainfall$municipality),
  multiple = TRUE
)

# Apply button
apply_button <- shiny::actionButton("apply", "Apply")

# Run the interface
shiny::shinyApp(
  ui = fluidPage(
    selected_municipalities,
    apply_button
  ),
  server = function(input, output, session) {
    observeEvent(input$apply, {
      # Filter the dataframe to include only the selected municipalities
      combined_rainfall <<- combined_rainfall %>%
        filter(municipality %in% input$municipalities)
      
      # Check the column names of the new dataframe
      print(names(combined_rainfall))
      
      # Print message in console about selected municipalities
      cat("Selected municipalities:", paste(input$municipalities, collapse = ", "), "\n")
      
      # Close the window
      shiny::removeUI(selector = ".modal-backdrop")
    })
  }
)

#------------------------------------------------------------------------------# 

##### STEP 2 - ADJUST THE DATAFRAME FOR FURTHER ANALYSIS #####

# Sort the data by datetime
combined_rainfall <- combined_rainfall %>%
  arrange(datetime)

# Change the "measure_value" field to numeric, handling commas and removing semicolons
combined_rainfall <- combined_rainfall %>%
  mutate(measure_value = suppressWarnings(as.numeric(gsub(",", ".", measure_value))))

# Exclude unnecessary variables (you can adjust this if you need to remove others later)
combined_rainfall <- combined_rainfall %>%
  select(station_code, datetime, measure_value, municipality, state, latitude, longitude)

# Ensure no missing or invalid data in important columns
combined_rainfall <- combined_rainfall %>%
  filter(!is.na(measure_value) & !is.na(datetime))

# Split the dataframe into a list of dataframes based on the "station_code" column
df_list <- split(combined_rainfall, combined_rainfall$station_code)

#------------------------------------------------------------------------------# 

##### STEP 3 - FILL MISSING RAIN MEASUREMENT DATA WITH 0 #####

# Sort the data by station and datetime
combined_rainfall <- combined_rainfall %>%
  arrange(station_code, datetime)

# Define temporal resolution of the rainfall series using dt_min
time_resolution <- paste(dt_min, "min")

# Create a function to fill missing timestamps
fill_missing_values <- function(df) {
  df %>%
    complete(datetime = seq(min(datetime), max(datetime), by = time_resolution)) %>%
    fill(station_code, municipality, state, latitude, longitude, .direction = "down") %>%
    replace_na(list(measure_value = 0))
}

# Apply the function to each dataframe in the list
for (station_name in names(df_list)) {
  df_list[[station_name]] <- fill_missing_values(df_list[[station_name]])
}

#------------------------------------------------------------------------------# 

##### STEP 4 - CALCULATE PRELIMINARY FIELDS FOR RAINFALL EVENTS ESTIMATION WITH USER-DEFINED RAINFALL THRESHOLD #####

# Define the user interface
ui <- fluidPage(
  titlePanel("Define Rain Noise Threshold"),
  sidebarLayout(
    sidebarPanel(
      numericInput("rain_threshold", "Set Rainfall Threshold (mm):", 
                   value = 0.2, min = 0, step = 0.1),
      actionButton("update", "Update Threshold")
    ),
    mainPanel(
      h3("Processing Precipitation Data..."),
      textOutput("status_text")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  observeEvent(input$update, {
    # Get the user-defined threshold
    rain_threshold <- input$rain_threshold
    
    # Initialize the status message
    output$status_text <- renderText("Processing...")
    
    # View the separate dataframes (one for each station value of station_code)
    for (station_name in names(df_list)) {
      assign(paste0("df_", station_name), df_list[[station_name]], envir = .GlobalEnv)
    }
    
    # Add "rain_or_not" column using the user-defined threshold
    for (station_name in names(df_list)) {
      assign(paste0("df_", station_name), 
             transform(df_list[[station_name]], rain_or_not = measure_value > rain_threshold), 
             envir = .GlobalEnv)
    }
    
    # Add "rain_or_not_count"
    for (station_name in names(df_list)) {
      df <- get(paste0("df_", station_name), envir = .GlobalEnv)
      df <- df %>%
        mutate(rain_or_not_count = cumsum(rain_or_not != lag(rain_or_not, default = first(rain_or_not))))
      assign(paste0("df_", station_name), df, envir = .GlobalEnv)
    }
    
    # Add "time_count" using dt_hr
    for (station_name in names(df_list)) {
      df <- get(paste0("df_", station_name), envir = .GlobalEnv)
      df <- df %>%
        mutate(
          time_count = cumsum(rain_or_not != lag(rain_or_not, default = first(rain_or_not))) * dt_hr
        )
      df$time_count <- unlist(sapply(split(df$time_count, 
                                           cumsum(df$rain_or_not != lag(df$rain_or_not, default = first(df$rain_or_not)))), 
                                     function(x) seq(dt_hr, length.out = length(x), by = dt_hr)))
      assign(paste0("df_", station_name), df, envir = .GlobalEnv)
    }
    
    # Update the status text
    output$status_text <- renderText({
      paste("Rain threshold set to:", rain_threshold, "mm. Data processed.")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

#------------------------------------------------------------------------------# 

##### STEP 5 - CHOOSE THE RAIN-FREE PERIOD THAT DEFINES THE RAIN EVENT #####

global_rain_free_period <- 12

ui <- fluidPage(
  sliderInput("rain_free_period", "Choose the rain-free period (in hours):", min = 0, max = 24, value = 12),
  actionButton("apply", "Apply")
)

server <- function(input, output, session) {
  observe({
    rain_free_period <- input$rain_free_period
    global_rain_free_period <<- rain_free_period
    print(paste("Chosen rain-free period:", rain_free_period))
  })
}

shinyApp(ui, server)

#------------------------------------------------------------------------------# 

##### STEP 6 - CALCULATE RAIN EVENTS FOR EACH STATION #####

for (station_name in names(df_list)) {
  df <- get(paste0("df_", station_name))
  df <- df %>%
    mutate(hours_sufficient_without_rain = rain_or_not == FALSE & time_count >= global_rain_free_period)
  assign(paste0("df_", station_name), df)
}

for (station_name in names(df_list)) {
  df <- get(paste0("df_", station_name))
  df <- df %>%
    mutate(if_rain_start = rain_or_not == TRUE & time_count == dt_hr,
           if_rain_end = rain_or_not == FALSE & lag(rain_or_not, default = FALSE) == TRUE)
  assign(paste0("df_", station_name), df)
}

for (station_name in names(df_list)) {
  df <- get(paste0("df_", station_name))
  df <- df %>%
    group_by(rain_or_not_count) %>%
    mutate(events_x_hours = any(time_count >= global_rain_free_period)) %>%
    ungroup()
  assign(paste0("df_", station_name), df)
}

for (station_name in names(df_list)) {
  df <- get(paste0("df_", station_name))
  df <- df %>%
    mutate(preceded_by_x_hours = if_rain_start & lag(events_x_hours, default = FALSE))
  assign(paste0("df_", station_name), df)
}

for (station_name in names(df_list)) {
  df <- get(paste0("df_", station_name))
  df <- df %>%
    mutate(followed_by_x_hours = if_rain_end & lead(events_x_hours, default = FALSE))
  assign(paste0("df_", station_name), df)
}

for (station_name in names(df_list)) {
  df <- get(paste0("df_", station_name))
  df <- df %>%
    mutate(
      new_event = ifelse(if_rain_start & (lag(time_count, default = 0) < global_rain_free_period), FALSE, if_rain_start)
    ) %>%
    mutate(count_events_x_hours_int = cumsum(new_event))
  assign(paste0("df_", station_name), df)
}

#------------------------------------------------------------------------------# 

##### STEP 7 - CREATE A NEW TABLE SYNTHESIZING RAIN EVENT DATA #####

rain_events_per_station <- list()

for (station_name in names(df_list)) {
  df <- get(paste0("df_", station_name))
  rain_events_df <- df %>%
    filter(rain_or_not) %>%
    group_by(count_events_x_hours_int) %>%
    summarize(
      start_event = min(datetime),
      end_event = max(datetime),
      latitude = first(latitude),
      longitude = first(longitude),
      municipality = first(municipality),
      state = first(state),
      .groups = "drop"
    ) %>%
    mutate(station = station_name)
  
  rain_events_per_station[[station_name]] <- rain_events_df
  assign(paste("rain_events_", station_name, sep = ""), rain_events_df, envir = .GlobalEnv)
}

#------------------------------------------------------------------------------# 

##### STEP 8 - CALCULATE EVENT RAINFALL, AVERAGE INTENSITY, AND DURATION #####

for (station_name in names(rain_events_per_station)) {
  events_df <- get(paste("rain_events_", station_name, sep = ""))
  original_df <- get(paste("df_", station_name, sep = ""))
  
  duration_hrs <- numeric()
  total_event_rain_mm <- numeric()
  average_intensity_mm_hr <- numeric()
  
  for (i in 1:nrow(events_df)) {
    event <- events_df[i, ]
    event_df <- original_df %>%
      filter(datetime >= event$start_event & datetime <= event$end_event)
    
    duration_hrs[i] <- nrow(event_df) * dt_hr
    total_event_rain_mm[i] <- sum(event_df$measure_value)
    average_intensity_mm_hr[i] <- total_event_rain_mm[i] / duration_hrs[i]
  }
  
  events_df$duration_hrs <- duration_hrs
  events_df$total_event_rain_mm <- total_event_rain_mm
  events_df$average_intensity_mm_hr <- average_intensity_mm_hr
  
  assign(paste("rain_events_", station_name, sep = ""), events_df, envir = .GlobalEnv)
}

#------------------------------------------------------------------------------# 

##### STEP 9 - CALCULATE PRECEDING ACCUMULATED RAINFALL #####

time_intervals <- c(24, 48, 72, 96, 120, 240, 360, 720, 2160)

for (station_name in names(rain_events_per_station)) {
  events_df <- get(paste("rain_events_", station_name, sep = ""))
  original_df <- get(paste("df_", station_name, sep = ""))
  new_events_df <- events_df
  
  for (i in 1:nrow(events_df)) {
    start_event <- events_df$start_event[i]
    for (interval in time_intervals) {
      start_interval <- start_event - hours(interval)
      interval_df <- original_df %>%
        filter(datetime >= start_interval & datetime < start_event)
      accumulated_rain <- sum(interval_df$measure_value)
      col_name <- paste("preceding_rain_", interval, "hrs", sep = "")
      new_events_df[i, col_name] <- accumulated_rain
    }
  }
  assign(paste("rain_events_", station_name, sep = ""), new_events_df, envir = .GlobalEnv)
}

#------------------------------------------------------------------------------# 

##### STEP 10 - CALCULATE MAXIMUM AVERAGE INTENSITY WITHIN EACH RAIN EVENT #####

new_time_intervals <- c(30, 60, 90, 120, 180, 360, 720, 1440)
intensity_conversion_factor <- 60 / dt_min   # e.g., 6 for dt_min = 10

for (station_name in names(rain_events_per_station)) {
  events_df <- get(paste("rain_events_", station_name, sep = ""))
  original_df <- get(paste("df_", station_name, sep = ""))
  new_events_df <- events_df
  
  for (i in 1:nrow(events_df)) {
    start_event <- events_df$start_event[i]
    end_event <- events_df$end_event[i]
    interval_data <- original_df[original_df$datetime >= start_event & original_df$datetime <= end_event, ]
    time_diff_hours <- as.numeric(difftime(end_event, start_event, units = "hours")) + dt_hr
    
    for (interval in new_time_intervals) {
      interval_hours <- interval / 60
      if (nrow(interval_data) > 0 && time_diff_hours >= interval_hours) {
        max_avg_intensity <- max(
          zoo::rollapply(interval_data$measure_value,
                         width = interval / dt_min,
                         FUN = mean,
                         align = "right",
                         fill = NA),
          na.rm = TRUE
        ) * intensity_conversion_factor
      } else if (nrow(interval_data) > 0 && time_diff_hours < interval_hours) {
        event_duration_hours <- nrow(interval_data) * dt_hr
        max_avg_intensity <- sum(interval_data$measure_value) / event_duration_hours
      } else {
        max_avg_intensity <- NA
      }
      col_name <- paste("max_avg_intensity_", interval, "min", sep = "")
      new_events_df[i, col_name] <- max_avg_intensity
    }
  }
  assign(paste("rain_events_", station_name, sep = ""), new_events_df, envir = .GlobalEnv)
}

#------------------------------------------------------------------------------# 

###### STEP 11 - FILTER RAIN EVENTS (OPTIONAL) #####

complete_events_df <- bind_rows(rain_events_per_station)
print("Size before filtering:")
print(dim(complete_events_df))

shiny::shinyApp(
  ui = fluidPage(
    sliderInput("min_total_time", "Minimum Total Event Duration (hours):", min = 0, max = 24, value = 0),
    sliderInput("min_avg_intensity", "Minimum Average Intensity of the Event (mm/hour):", min = 0, max = 10, value = 0),
    actionButton("apply", "Apply")
  ),
  server = function(input, output, session) {
    observeEvent(input$apply, {
      for (station_name in names(rain_events_per_station)) {
        events_df <- get(paste("rain_events_", station_name, sep = ""))
        filtered_events_df <- events_df %>%
          filter(duration_hrs >= input$min_total_time &
                   average_intensity_mm_hr >= input$min_avg_intensity)
        filtered_df_name <- paste("rain_events_", station_name, "_filtered", sep = "")
        assign(filtered_df_name, filtered_events_df, envir = .GlobalEnv)
      }
      shiny::updateActionButton(session, "apply", label = "Apply", icon = NULL)
    })
  }
)

#------------------------------------------------------------------------------# 

##### STEP 12 - SAVE THE FILTERED RAIN EVENT DATAFRAMES #####

base_output_dir <- file.path(getwd(), "outputs")
rain_events_dir <- file.path(base_output_dir, "rain_events")
if (!dir.exists(rain_events_dir)) dir.create(rain_events_dir, recursive = TRUE)

message <- "Choose or create a directory to save rain event files"
user_dir <- choose.dir(default = base_output_dir, caption = message)
if (!is.na(user_dir) && user_dir != "") {
  rain_events_dir <- file.path(user_dir, "rain_events")
  if (!dir.exists(rain_events_dir)) dir.create(rain_events_dir, recursive = TRUE)
}

for (station_name in names(rain_events_per_station)) {
  filtered_events_df <- get(paste("rain_events_", station_name, "_filtered", sep = ""))
  filtered_file_name <- paste0(station_name, "_rain_events.csv")
  filtered_file_path <- file.path(rain_events_dir, filtered_file_name)
  readr::write_csv2(filtered_events_df, file = filtered_file_path, col_names = TRUE)
  cat("Filtered rain event DataFrame for", station_name, "exported to:", filtered_file_path, "\n")
}

#END OF CODE ------------------------------------------------------------------#