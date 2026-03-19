library(data.table)
library(arrow)
library(fs)

setwd(dir = dirname(rstudioapi::getActiveDocumentContext()$path))
# Source the functions
source("ACI computation/2_treating.R")
source("ACI computation/3_configuration_indices.R")

# 1. Setup Test Directories
chunk_dir <- "data/test_chunks"
agg_dir <- "data/test_agg"
if (dir_exists(chunk_dir)) dir_delete(chunk_dir)
if (dir_exists(agg_dir)) dir_delete(agg_dir)
dir_create(chunk_dir)
dir_create(agg_dir)

# 2. Generate 3 Synthetic Chunks
set.seed(42)
num_chunks <- 3
buildings_per_chunk <- 500
types <- paste0("T", 1:50)
years <- 2020:2022

for (i in 1:num_chunks) {
    message(paste("Generating synthetic chunk", i))
    
    # Each building reaches a random number of amenities
    obs_per_chunk <- buildings_per_chunk * 20 
    
    chunk_data <- data.table(
        origin_id = rep(paste0("B", ((i-1)*buildings_per_chunk + 1):(i*buildings_per_chunk)), each = 20),
        dest_type = sample(types, obs_per_chunk, replace = TRUE),
        year = sample(years, obs_per_chunk, replace = TRUE),
        travel_time_p50 = runif(obs_per_chunk, 1, 20)
    )
    
    write_parquet(chunk_data, file.path(chunk_dir, paste0(i, ".parquet")))
}

# 3. Run Aggregation
message("\n--- Running combine_and_aggregate ---")
origins_to_types <- combine_and_aggregate(
    chunk_dir = chunk_dir,
    agg_data_dir = agg_dir,
    time_threshold = 15
)

print(dim(origins_to_types))
print(head(origins_to_types[, 1:5]))

# 4. Run TCI
message("\n--- Running compute_tci ---")
tci_results <- compute_tci(origins_to_types, rolling_window = 1)
print(dim(tci_results))
print(head(tci_results))

# 5. Run ACI
message("\n--- Running compute_aci ---")
aci_results <- compute_aci(origins_to_types, tci_data = tci_results)
print(dim(aci_results))
print(head(aci_results))

# 6. Verification
cat("\n--- Verification Summary ---\n")
cat(paste("Total Buildings:", length(unique(aci_results$origin_id)), " (Expected: 1500)\n"))
cat(paste("Total Types in TCI:", length(unique(tci_results$type)), " (Expected: 50)\n"))
cat(paste("ACI Mean (should be near 0 due to Z-score):", mean(aci_results$ACI), "\n"))
cat(paste("ACI SD (should be near 1):", sd(aci_results$ACI), "\n"))
