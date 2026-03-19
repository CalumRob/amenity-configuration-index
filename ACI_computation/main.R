setwd(dir = dirname(rstudioapi::getActiveDocumentContext()$path))

source("1_routing.R")
source("2_treating.R")
source("3_configuration_indices.R")

####### Inputs

# Table of origin points (buildings?) with columns id, lon, lat
# OR sf points object with column id
# CRS must be EPSG:4326
origins <- fread("data/building_as_origins_reduced.csv")

# Table of amenities with columns id, lon, lat, year, type
# OR sf points object with columns id, year, type
# CRS must be EPSG:4326
amenities <- fread("data/BDCOM_14_to_23_cleaned.csv")

# Directory containing the pbf extract for street networks
pbf_dir <- "data/pbf"

# Run this once to download the pbf file
# Find the url on geofabrik.de
# pbf_url <- "URL_OF_YOUR_PBF_FILE"
# pbf_dest_file <- file.path(pbf_dir, basename(pbf_url))
# download.file(pbf_url, destfile = pbf_dest_file, mode = "wb")


# Maximum walking time threshold (in minutes at 4km/h)
# This is the upperbound time for routing. If you're not sure of the right value yet,
# it's better to set it higher than lower. You can always filter out longer travel times later when combining chunks.
max_time <- 25

# Gigs of RAM to allocate to rJava for r5r routing
rjava_memory <- 8

# Number of origins to process at a time
chunk_size <- 10000

######## Main

# This call will create a folder called R5R in the data directory and save the travel times chunks in it.
# It will take a while to run depending on your RAM setting, the number of origins, the size of the PBF, of chunks, and on your CPU. Be patient.
output_dir <- "data/R5R"
compute_travel_times(
    origins = origins,
    amenities = amenities,
    pbf_dir = pbf_dir,
    max_time = max_time,
    chunk_dir = output_dir
)

# Combine travel time chunks and aggregate amenity counts in a RAM-safe way
# This processes each chunk iteratively to avoid a massive intermediate table.
time_threshold <- 15
agg_data_dir <- "data/aggregated"
origins_to_types <- combine_and_aggregate(
    chunk_dir = output_dir,
    agg_data_dir = agg_data_dir,
    time_threshold = time_threshold
)

# Calculate TCI (Type Complexity Index)
# Rolling window (number of years) to smooth results
rolling_window <- 3
tci_results <- compute_tci(origins_to_types, rolling_window = rolling_window)

# Calculate ACI (Area Configuration Index) for buildings
aci_results <- compute_aci(origins_to_types, tci_data = tci_results)

# Optional: Save results
fwrite(tci_results, "data/tci_results.csv")
fwrite(aci_results, "data/aci_results.csv")
