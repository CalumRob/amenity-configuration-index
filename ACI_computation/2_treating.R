combine_and_aggregate <- function(chunk_dir, agg_data_dir, time_threshold) {
    require(data.table)
    require(arrow)
    require(fs)

    # List all travel time chunks
    all_chunk_files <- list.files(chunk_dir, pattern = "*.parquet", full.names = TRUE)
    
    # Create output directory if it doesn't exist
    if (!fs::dir_exists(agg_data_dir)) {
        fs::dir_create(agg_data_dir, recursive = TRUE)
    }

    message(paste("Starting aggregation for", length(all_chunk_files), "chunks..."))
    
    # Initialize list to store aggregated wide chunks
    list_of_wide_chunks <- list()

    for (i in seq_along(all_chunk_files)) {
        chunk_file <- all_chunk_files[i]
        message(paste0("[", i, "/", length(all_chunk_files), "] Processing: ", basename(chunk_file)))
        
        # Read the routing chunk
        current <- arrow::read_parquet(chunk_file)
        setDT(current)
        
        # Filter travel times and aggregate counts immediately
        # Grouping by origin_id, year, and type (dest_type)
        chunk_agg <- current[travel_time_p50 <= time_threshold, 
                           .(count = .N), 
                           by = .(origin_id, year, type = dest_type)]
        
        # Pivot to WIDE format for this chunk
        chunk_wide <- dcast(chunk_agg, origin_id + year ~ type, value.var = "count", fill = 0)
        
        # Append to our list
        list_of_wide_chunks[[i]] <- chunk_wide
        
        # Clean up
        rm(current, chunk_agg)
        gc()
    }

    message("Combining all chunks into a single wide table...")
    
    # Combine everything using fill = TRUE to handle varying types across chunks
    all_origins_to_types <- rbindlist(list_of_wide_chunks, fill = TRUE)
    
    # Replace NAs with 0 for types not present in specific chunks
    # This identifies all numeric (count) columns and fills NAs
    type_cols <- names(all_origins_to_types)[!(names(all_origins_to_types) %in% c("origin_id", "year"))]
    for (col in type_cols) {
        set(all_origins_to_types, which(is.na(all_origins_to_types[[col]])), col, 0)
    }

    # Save the combined wide result
    combined_output_file <- file.path(agg_data_dir, "origins_to_types_allyears.parquet")
    arrow::write_parquet(all_origins_to_types, combined_output_file)
    message(paste("Complexity-ready aggregation saved to:", combined_output_file))

    return(all_origins_to_types)
}
