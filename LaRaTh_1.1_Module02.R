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

#---------------------------------- MODULE 2 ----------------------------------# 
#------------------- Organization of the Landslide Database -------------------#
#-------------------- Relates Landslides to Rainfall Events -------------------#

##### STEP 1 - LOAD AND ADJUST LANDSLIDE DATABASE #####

# Install required packages if they are not already installed
required_packages <- c("readxl", "dplyr", "lubridate", "lubridate", "shiny", "geosphere")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load the library
library(readxl)
library(dplyr)
library(lubridate)
library(shiny)
library(geosphere)

# Allow the user to select the directory where the XLS files are located
directory <- choose.dir(default = "C:/Users/Public", caption = "Select the folder containing the XLS files with landslide database")

# List all XLS files in the directory
xls_files <- list.files(directory, pattern = ".xls", full.names = TRUE)

# Define a translation dictionary for temporal and spatial accuracy (for Cemaden database)
translation_dict <- c("AUSENTE" = "missing",
                      "BAIXA PRECISÃO" = "low",
                      "MÉDIA PRECISÃO" = "medium",
                      "ALTA PRECISÃO" = "high")

# Create a function to read each XLS file, convert it into a dataframe, and apply translations (for Cemaden database)
read_and_convert <- function(file, translation_dict) {
  # Read Excel file
  df <- read_excel(file)
  
  # Define expected columns for minimum database
  expected_min_columns <- c("Data Ocorrência", "Latitude", "Longitude")
  
  # Check if minimum columns exist
  if(!all(expected_min_columns %in% names(df))) {
    stop(paste("File", file, "does not contain required minimum columns:", 
               paste(expected_min_columns[!expected_min_columns %in% names(df)], collapse=", ")))
  }
  
  # Translation of column names (for Cemaden Database)
  translated_columns <- c(
    "UF" = "state",
    "Município" = "municipality",
    "Evento do Registro" = "process_type",
    "Data Ocorrência" = "date",
    "Precisão Temporal" = "temporal_accuracy",
    "Horário da Ocorrência" = "time",
    "Magnitude_evento" = "magnitude",
    "Precisão_localização" = "spatial_accuracy",
    "Latitude" = "latitude",
    "Longitude" = "longitude"
  )
  
  # Select and rename only columns that exist in the dataframe
  cols_to_select <- names(translated_columns)[names(translated_columns) %in% names(df)]
  df_selected <- df %>%
    dplyr::select(all_of(cols_to_select))
  
  # Rename columns
  for(old_name in cols_to_select) {
    new_name <- translated_columns[old_name]
    names(df_selected)[names(df_selected) == old_name] <- new_name
  }
  
  # Add missing columns with default values if they don't exist
  if(!"temporal_accuracy" %in% names(df_selected)) {
    df_selected$temporal_accuracy <- "missing"
  }
  
  if(!"spatial_accuracy" %in% names(df_selected)) {
    df_selected$spatial_accuracy <- "missing"
  }
  
  if(!"magnitude" %in% names(df_selected)) {
    df_selected$magnitude <- "not_informed"
  }
  
  if(!"process_type" %in% names(df_selected)) {
    df_selected$process_type <- "landslide"
  }
  
  if(!"state" %in% names(df_selected)) {
    df_selected$state <- "not_informed"
  }
  
  if(!"municipality" %in% names(df_selected)) {
    df_selected$municipality <- "not_informed"
  }
  
  if(!"time" %in% names(df_selected)) {
    df_selected$time <- "12:00"  # Default time if not provided
  }
  
  # Translate temporal and spatial accuracy if they exist in translation_dict
  df_selected <- df_selected %>%
    mutate(temporal_accuracy = ifelse(temporal_accuracy %in% names(translation_dict),
                                      translation_dict[temporal_accuracy],
                                      temporal_accuracy),
           spatial_accuracy = ifelse(spatial_accuracy %in% names(translation_dict),
                                     translation_dict[spatial_accuracy],
                                     spatial_accuracy))
  
  # Modify time column based on temporal accuracy
  df_selected$time <- case_when(
    df_selected$temporal_accuracy == "high" & grepl(":", df_selected$time) ~ substr(df_selected$time, 12, 16),
    df_selected$temporal_accuracy == "medium" & grepl(":", df_selected$time) ~ substr(df_selected$time, 17, 21),
    df_selected$temporal_accuracy == "low" & grepl("Madrugada", df_selected$time, ignore.case = TRUE) ~ "night",
    df_selected$temporal_accuracy == "low" & grepl("Manhã", df_selected$time, ignore.case = TRUE) ~ "morning",
    df_selected$temporal_accuracy == "low" & grepl("Tarde", df_selected$time, ignore.case = TRUE) ~ "afternoon",
    df_selected$temporal_accuracy == "low" & grepl("Noite", df_selected$time, ignore.case = TRUE) ~ "evening",
    TRUE ~ df_selected$time
  )
  
  # Create landslide_datetime column
  df_selected$landslide_datetime <- with(df_selected, 
                                         ifelse(temporal_accuracy == "missing",
                                                as.POSIXct(paste(date, "12:00"), format = "%d/%m/%Y %H:%M"),
                                                ifelse(grepl("night", time, ignore.case = TRUE),
                                                       as.POSIXct(paste(date, "03:00"), format = "%d/%m/%Y %H:%M"),
                                                       ifelse(grepl("morning", time, ignore.case = TRUE),
                                                              as.POSIXct(paste(date, "09:00"), format = "%d/%m/%Y %H:%M"),
                                                              ifelse(grepl("afternoon", time, ignore.case = TRUE),
                                                                     as.POSIXct(paste(date, "15:00"), format = "%d/%m/%Y %H:%M"),
                                                                     ifelse(grepl("evening", time, ignore.case = TRUE),
                                                                            as.POSIXct(paste(date, "21:00"), format = "%d/%m/%Y %H:%M"),
                                                                            as.POSIXct(paste(date, time), format = "%d/%m/%Y %H:%M")
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
  )
  
  return(df_selected)
}

# Use lapply to read, convert, and apply translations to all XLS files
df_landslides <- lapply(xls_files, read_and_convert, translation_dict) %>%
  bind_rows()

# Update the 'magnitude' column to use 'Low', 'Moderate', 'High' instead of 'PP', 'MP', 'GP'
df_landslides <- df_landslides %>%
  mutate(magnitude = case_when(
    magnitude == "PP" ~ "low",
    magnitude == "MP" ~ "moderate",
    magnitude == "GP" ~ "high",
    TRUE ~ magnitude
  ))

# Convert landslide_datetime to POSIXct
# Code assumes that the landslide's time are in the local time of the computer being used and converts them to GMT.
# Therefore, if the landslide's time are not in local time, the code should be modified to properly assign landslide_datetime.
df_landslides$landslide_datetime <- as.POSIXct(df_landslides$landslide_datetime, format = "%d/%m/%Y %H:%M", tz = "GMT")

#------------------------------------------------------------------------------# 

##### STEP 2: FILTER LANDSLIDES BASED ON TYPE, TEMPORAL AND SPATIAL ACCURACY, AND MAGNITUDE #####

# User interface
ui3 <- fluidPage(
  titlePanel("Filter Landslides"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for selecting event type
      selectizeInput("event_type", "Choose Event Type:",
                     choices = unique(df_landslides$process_type),
                     multiple = TRUE),
      
      # Input for selecting temporal precision
      selectizeInput("temporal_precision", "Choose Temporal Precision:",
                     choices = unique(df_landslides$temporal_accuracy),
                     multiple = TRUE),
      
      # Input for selecting spatial precision
      selectizeInput("spatial_precision", "Choose Spatial Precision:",
                     choices = unique(df_landslides$spatial_accuracy),
                     multiple = TRUE),
      
      # Input for selecting magnitude
      selectizeInput("magnitude", "Choose Magnitude:",
                     choices = unique(df_landslides$magnitude),
                     multiple = TRUE),
      
      # Filter button
      actionButton("filter_button", "Filter")
    ),
    
    mainPanel(
      # Table to display filtered landslides
      tableOutput("df_landslides_table")
    )
  )
)

# Server logic
server3 <- function(input, output) {
  observeEvent(input$filter_button, {
    # Collect user-selected values
    event_types <- input$event_type
    temporal_precision <- input$temporal_precision
    spatial_precision <- input$spatial_precision
    magnitudes <- input$magnitude
    
    # Display user selections in the console
    cat("Event Types:", paste(event_types, collapse = ", "), "\n")
    cat("Temporal Precision:", paste(temporal_precision, collapse = ", "), "\n")
    cat("Spatial Precision:", paste(spatial_precision, collapse = ", "), "\n")
    cat("Magnitudes:", paste(magnitudes, collapse = ", "), "\n")
    
    # Filter landslides based on user-selected criteria
    df_landslides_filtered <- df_landslides %>%
      filter(
        grepl(paste0("(?i)", paste(event_types, collapse = "|")), process_type) &
          temporal_accuracy %in% temporal_precision &
          spatial_accuracy %in% spatial_precision &
          magnitude %in% magnitudes
      )
    
    # Update the filtered dataset in the global environment
    assign("df_landslides", df_landslides_filtered, envir = .GlobalEnv)
  })
  
  # Render the filtered landslides table
  output$df_landslides_table <- renderTable({
    head(df_landslides)
  })
}

# Launch the Shiny app
shinyApp(ui3, server3)

#------------------------------------------------------------------------------# 

##### STEP 3: LOAD  RAINFALL EVENTS #####

# Load the library
library(dplyr)
library(lubridate)
library(shiny)

# Allow the user to choose the rain events folder
rain_events_directory <- choose.dir(default = "C:/Users/Public", caption = "Select the rainfall events folder")

# List of CSV files for rain events
rain_events_files <- list.files(rain_events_directory, full.names = TRUE, pattern = ".csv")

# Initialize empty lists to store rain events
rain_events_list <- list()

# Import the rain events data
for (rain_events_file in rain_events_files) {
  rain_events <- readr::read_csv2(rain_events_file)
  rain_events_list <- c(rain_events_list, list(rain_events))
}

# Combine all rain events into a single dataframe
df_rain_events <- do.call(rbind, rain_events_list)

#------------------------------------------------------------------------------# 

##### STEP 4: CREATE THE SEASON FIELD #####

# Handle cases where 'start_event' is NA without losing the date format
df_rain_events$start_event <- coalesce(df_rain_events$start_event, df_rain_events$end_event)

# Check for invalid values in the date columns
invalid_dates <- which(!complete.cases(df_rain_events$start_event, df_rain_events$end_event))

# Create a new variable indicating the season based on the start date
df_rain_events$season <- case_when(
  between(month(df_rain_events$start_event), 3, 5) | (month(df_rain_events$start_event) == 6 & day(df_rain_events$start_event) < 21) ~ "Autumn",
  between(month(df_rain_events$start_event), 6, 8) | (month(df_rain_events$start_event) == 9 & day(df_rain_events$start_event) < 23) ~ "Winter",
  between(month(df_rain_events$start_event), 9, 11) | (month(df_rain_events$start_event) == 12 & day(df_rain_events$start_event) < 22) ~ "Spring",
  TRUE ~ "Summer"
)

#------------------------------------------------------------------------------# 

##### STEP 5: ADJUST COORDINATES OF RAIN EVENTS #####
# NOTE: Different rainfall data sources may use distinct coordinate formats:
# 
# 1. CEMADEN - Interactive Map / Database:
#    - Format: Decimal degrees (e.g., -23.5504, -46.6333)
#    - Already correct, NO adjustment needed
#
# 2. CEMADEN - API / Raw Data:
#    - Format: 6-digit coordinates (e.g., -235504, -466333)
#    - Requires conversion by dividing by 10000
#
# 3. Conventional Stations / INMET:
#    - Format: Decimal degrees (e.g., -23.5504)
#    - NO adjustment needed
#
# 4. Hydrological Data / ANA:
#    - Format: Decimal degrees or Degrees/Minutes/Seconds
#    - Check conversion requirements

# Function to detect coordinate format
detect_coordinate_format <- function(coords) {
  # Remove NAs for analysis
  coords_clean <- coords[!is.na(coords)]
  if(length(coords_clean) == 0) return("unknown")
  
  # Calculate typical magnitude of values (excluding zeros)
  non_zero <- coords_clean[abs(coords_clean) > 0.01]
  if(length(non_zero) == 0) return("unknown")
  
  median_abs <- median(abs(non_zero))
  
  # Detection criteria:
  if(median_abs > 180) {
    # Very large values (>180) indicate raw CEMADEN format (e.g., -235504)
    return("cemaden_raw")
  } else if(median_abs > 90) {
    # Values between 90 and 180 - invalid for latitude, likely raw CEMADEN format
    return("cemaden_raw")
  } else if(median_abs > 1 && median_abs <= 90) {
    # Values between 1 and 90 - standard decimal format (e.g., -23.5504)
    return("decimal")
  } else if(median_abs <= 1) {
    # Very small values (<1) - could be decimal near Equator or invalid format
    return("possibly_decimal")
  }
  
  return("unknown")
}

# Function to convert raw CEMADEN coordinates to decimal
convert_cemaden_raw_to_decimal <- function(coord) {
  # Raw CEMADEN format: 6-digit coordinates
  # Example: -235504 (represents -23.5504)
  #          -466333 (represents -46.6333)
  
  if(is.na(coord)) return(NA)
  
  # Preserve the sign
  sign <- ifelse(coord < 0, -1, 1)
  abs_coord <- abs(coord)
  
  # Convert to decimal by dividing by 10000
  # This transforms -235504 into -23.5504
  decimal_coord <- sign * (abs_coord / 10000)
  
  return(decimal_coord)
}

# Main coordinate adjustment function with automatic detection
adjust_coordinates_auto <- function(df_rain_events, verbose = TRUE) {
  
  # Detect format of latitudes (using a sample)
  sample_lat <- df_rain_events$latitude[1:min(100, nrow(df_rain_events))]
  lat_format <- detect_coordinate_format(sample_lat)
  
  sample_lon <- df_rain_events$longitude[1:min(100, nrow(df_rain_events))]
  lon_format <- detect_coordinate_format(sample_lon)
  
  if(verbose) {
    cat("\n=== COORDINATE DIAGNOSTIC ===\n")
    cat("Detected format for latitude:", lat_format, "\n")
    cat("Detected format for longitude:", lon_format, "\n")
    cat("Example values:\n")
    cat("  Latitude:", head(df_rain_events$latitude, 3), "\n")
    cat("  Longitude:", head(df_rain_events$longitude, 3), "\n")
  }
  
  # Apply appropriate conversion based on detected format
  if(lat_format == "cemaden_raw" || lon_format == "cemaden_raw") {
    if(verbose) cat("\n✓ Applying raw CEMADEN to decimal conversion...\n")
    df_rain_events$latitude <- sapply(df_rain_events$latitude, convert_cemaden_raw_to_decimal)
    df_rain_events$longitude <- sapply(df_rain_events$longitude, convert_cemaden_raw_to_decimal)
  } else {
    if(verbose) cat("\n✓ Coordinates already in decimal format. No conversion needed.\n")
    # If in decimal format, just ensure they are numeric
    df_rain_events$latitude <- as.numeric(df_rain_events$latitude)
    df_rain_events$longitude <- as.numeric(df_rain_events$longitude)
  }
  
  # Final coordinate validation
  valid_lat <- between(df_rain_events$latitude, -90, 90)
  valid_lon <- between(df_rain_events$longitude, -180, 180)
  
  if(verbose) {
    cat("\n=== POST-CONVERSION VALIDATION ===\n")
    cat("Valid latitudes:", sum(valid_lat, na.rm = TRUE), "out of", nrow(df_rain_events), "\n")
    cat("Valid longitudes:", sum(valid_lon, na.rm = TRUE), "out of", nrow(df_rain_events), "\n")
    
    if(sum(!valid_lat, na.rm = TRUE) > 0 || sum(!valid_lon, na.rm = TRUE) > 0) {
      cat("\n⚠️ WARNING: Some coordinates became invalid after conversion!\n")
      cat("Example of invalid values:\n")
      invalid_idx <- which(!valid_lat | !valid_lon)[1:min(5, sum(!valid_lat | !valid_lon))]
      for(idx in invalid_idx) {
        cat("  Row", idx, "- Lat:", df_rain_events$latitude[idx], 
            "Lon:", df_rain_events$longitude[idx], "\n")
      }
    }
  }
  
  return(df_rain_events)
}

# Apply automatic coordinate adjustment
df_rain_events <- adjust_coordinates_auto(df_rain_events, verbose = TRUE)

# Filter valid landslide entries
df_landslides <- df_landslides[!is.na(df_landslides$latitude) & 
                                 !is.na(df_landslides$longitude) & 
                                 !is.na(df_landslides$landslide_datetime), ]

df_landslides <- df_landslides %>%
  filter(between(latitude, -90, 90) & between(longitude, -180, 180))

# Filter valid rainfall events
df_rain_events <- df_rain_events %>%
  filter(between(latitude, -90, 90) & between(longitude, -180, 180))

#------------------------------------------------------------------------------# 

##### STEP 6: CREATE THE FUNCTION TO ASSIGN TO THE RAIN EVENTS THE NUMBER OF OCCURRENCES WITHIN OR NEARBY #####
library(shiny)
library(dplyr)
library(lubridate)
library(geosphere)  # For spatial distance calculations

# Function to calculate the distance between two geographic points (in km)
haversine <- function(lat1, lon1, lat2, lon2) {
  distVincentySphere(c(lon1, lat1), c(lon2, lat2)) / 1000  # Distance in km
}

# Function to relate landslides to rain events
relate_rain_to_landslides <- function(df_landslides, df_rain_events, time_window, distance_radius) {
  
  # Add a column to count landslides related to each rain event
  df_rain_events$num_related_landslides <- 0
  
  # Loop through all landslides
  for (i in 1:nrow(df_landslides)) {
    landslide <- df_landslides[i, ]
    
    # Loop through all rain events
    for (j in 1:nrow(df_rain_events)) {
      rain_event <- df_rain_events[j, ]
      
      # Calculate time difference between landslide and rain event start and end
      time_diff_start <- difftime(landslide$landslide_datetime, rain_event$start_event, units = "secs")
      time_diff_end <- difftime(landslide$landslide_datetime, rain_event$end_event, units = "secs")
      
      # Calculate the distance between landslide and rain event location
      distance <- haversine(landslide$latitude, landslide$longitude, rain_event$latitude, rain_event$longitude)
      
      # Debugging: Print intermediate values for checking
      #cat("Landslide:", i, "Rain Event:", j, "Time Diff Start:", time_diff_start, "Time Diff End:", time_diff_end, "Distance:", distance, "\n")
      
      # Check if the landslide occurred within the time window and distance radius
      if (time_window == 0) {
        # If time_window is 0, check if the landslide occurred strictly within the rain event's time
        if (distance <= distance_radius && 
            landslide$landslide_datetime >= rain_event$start_event && 
            landslide$landslide_datetime <= rain_event$end_event) {
          
          # Increment the count of landslides related to this rain event
          df_rain_events$num_related_landslides[j] <- df_rain_events$num_related_landslides[j] + 1
          
          # Debugging: Confirm the landslide is within the rain event time window
          #cat("Landslide", i, "related to rain event", j, "\n")
        }
      } else {
        # Otherwise, check if the landslide occurred within the time window before or after the rain event
        if ((landslide$landslide_datetime >= rain_event$start_event - time_window * 3600) && 
            (landslide$landslide_datetime <= rain_event$end_event + time_window * 3600) &&
            distance <= distance_radius) {
          
          # Increment the count of landslides related to this rain event
          df_rain_events$num_related_landslides[j] <- df_rain_events$num_related_landslides[j] + 1
          
          # Debugging: Confirm the landslide is within the adjusted time window
          #cat("Landslide", i, "related to rain event", j, "within time window.\n")
        }
      }
    }
  }
  
  # Return the updated dataframe with the number of related landslides
  return(df_rain_events)
}

#------------------------------------------------------------------------------# 

##### STEP 7: ASSIGN TO THE RAIN EVENTS THE NUMBER OF OCCURRENCES WITHIN OR NEARBY #####
# UI for Shiny app
ui <- fluidPage(
  titlePanel("Rain to Landslide Relation Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("time_window", "Time Window (in hours):", 24, min = 1, step = 1),  # Input for time window
      helpText("Define the time window (in hours) to consider landslides that occurred before or after the rain event within this range."),
      
      numericInput("distance_radius", "Distance Radius (in km):", 10, min = 1, step = 1),  # Input for distance radius
      helpText("Specify the radius (in kilometers) within which landslides are considered related to a rain event based on proximity."),
      actionButton("compute_btn", "Compute")  # Button to trigger calculation
    ),
    
    mainPanel(
      tableOutput("result_table")  # Table to display the results
    )
  )
)

# Server function for the Shiny app
server <- function(input, output) {
  
  # Assuming df_landslides and df_rain_events are already loaded in the global environment
  # Ensure these data frames are available before running the app
  
  observeEvent(input$compute_btn, {
    # Call the function to associate rain events with landslides
    df_rain_events_with_landslides <<- relate_rain_to_landslides(
      df_landslides, 
      df_rain_events, 
      time_window = input$time_window, 
      distance_radius = input$distance_radius
    )
    
    # Display the updated results in the Shiny interface
    output$result_table <- renderTable({
      df_rain_events_with_landslides  # Show the updated dataframe with the related landslides count
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

#------------------------------------------------------------------------------# 

##### STEP 8: ANALYSIS OF RAIN EVENTS AND LANDSLIDES #####

# Count how many rainfall events have at least one related landslide
num_rain_events_with_landslides <- sum(df_rain_events_with_landslides$num_related_landslides > 0, na.rm = TRUE)

# Display the result of rainfall events with related landslides
print(paste("Number of rainfall events with related landslides: ", num_rain_events_with_landslides))

# Calculate the total number of landslides related to rainfall events (cumulative)
total_landslides <- sum(df_rain_events_with_landslides$num_related_landslides, na.rm = TRUE)

# Display the cumulative total of related landslides
print(paste("Total number of landslides related to rainfall events (cumulative): ", total_landslides))

#------------------------------------------------------------------------------# 

##### STEP 9: EXPORT THE DATAFRAME OF RAIN EVENTS WITH LANDSLIDES #####

# Allow the user to select the main directory where they want to save the CSV file
main_directory <- choose.dir(default = "C:/Users/Public", caption = "Select the main folder where the 'rain_events_landslides' subfolder will be created")

# Check if user selected a directory
if (is.na(main_directory) || main_directory == "") {
  cat("No directory selected. Using current working directory.\n")
  main_directory <- getwd()
}

# Define the subfolder within the selected directory
subfolder_name <- "rain_events_landslides"

# Create the full path combining the selected directory and subfolder
output_directory <- file.path(main_directory, subfolder_name)

# Create the subfolder if it doesn't exist
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
  cat("Created directory:", output_directory, "\n")
} else {
  cat("Using existing directory:", output_directory, "\n")
}

# Define the output file name
output_file_name <- "rain_events_with_landslides.csv"

# Create the full output path by combining the directory and the file name
output_csv_path <- file.path(output_directory, output_file_name)

# Export the dataframe "df_rain_events_with_landslides" to the CSV file
# Using write_csv2 (semicolon separator) for compatibility with Brazilian Excel
readr::write_csv2(df_rain_events_with_landslides, file = output_csv_path, col_names = TRUE)

# Confirm that the CSV file was saved
if (file.exists(output_csv_path)) {
  cat("CSV file exported successfully to:", output_csv_path, "\n")
} else {
  cat("An error occurred while exporting the CSV file.\n")
}

#END OF CODE ------------------------------------------------------------------# 
