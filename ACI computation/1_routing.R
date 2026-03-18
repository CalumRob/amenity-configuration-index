### Routing functions. Computes travel times and rejoins the chunks.


compute_travel_times <- function(origins,
                                 amenities,
                                 pbf_dir = "data/pbf",
                                 max_time = 15,
                                 chunk_size = 10000,
                                 chunk_dir = "data/R5R",
                                 rjava_memory = 8,
                                 overwite = FALSE) {
    options(java.parameters = paste0("-Xmx", rjava_memory, "G"))

    if (file.exists(file.path(chunk_dir, "1.parquet")) & !overwite) {
        stop("First output chunk file already exists. Please remove it, change the output directory or set overwrite to TRUE.")
    }

    require(rJava) # For garbage collection
    require(r5r) # For routing
    require(data.table) # For data manipulation
    require(fs) # For file system operations
    require(arrow) # For arrow file operations

    # Check if origins is sf or dataframe, stop if it is neither
    if (!inherits(origins, "data.frame") && !inherits(origins, "sf")) {
        stop("Origins must be a data.frame or sf POINTS object.")
    }

    if (!"id" %in% names(origins)) {
        stop("Origins must contain an id column.")
    }

    if ((!"lon" %in% names(origins) | !"lat" %in% names(origins)) && (!inherits(origins, "sf") || !any(sf::st_geometry_type(origins) %in% c("POINT", "MULTIPOINT")))) {
        stop("Origins must contain a lon and lat column.")
    }

    # Check if amenities is sf or dataframe, stop if it is neither
    if (!inherits(amenities, "data.frame") && !inherits(amenities, "sf")) {
        stop("Amenities must be a data.frame or sf POINTS object.")
    }

    if (!"id" %in% names(amenities)) {
        stop("Amenities must contain an id column.")
    }

    if ((!"lon" %in% names(amenities) | !"lat" %in% names(amenities)) && (!inherits(amenities, "sf") || !any(sf::st_geometry_type(amenities) %in% c("POINT", "MULTIPOINT")))) {
        stop("Amenities must contain a lon and lat column.")
    }

    if (!"year" %in% names(amenities)) {
        stop("Amenities must contain an year column. If you are only using one year, please add it anyway.")
    }

    if (!"type" %in% names(amenities)) {
        stop("Amenities must contain an type column.")
    }

    # Check if pbf_dir exists, stop if it does not
    if (!dir.exists(pbf_dir)) {
        stop("PBF directory does not exist. Many PBF OSM extracts are available on geofabrik.")
    }

    # Check if output_dir exists, create it if it does not
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        message("Output directory created.")
    }

    if (inherits(origins, "sf") & st_crs(origins)$epsg != 4326) {
        message("Transforming origins to EPSG:4326.")
        origins <- st_transform(origins, crs = 4326)
    }

    if (inherits(amenities, "sf") & st_crs(amenities)$epsg != 4326) {
        message("Transforming amenities to EPSG:4326.")
        amenities <- st_transform(amenities, crs = 4326)
    }

    chunks <- split(origins, (seq_len(nrow(origins)) - 1) %/% chunk_size)

    destinations[, new_id := row_number()]

    r5_core <- setup_r5(
        paste0(pbf_dir),
        verbose = FALSE,
        temp_dir = FALSE,
        elevation = "NONE",
        overwrite = F
    )

    for (chunk in 1:length(chunks)) {
        new_origins <- chunks[[chunk]]
        new_origins[, new_id := row_number()]
        # An argument could be made to use the accessibility R5R function instead.
        # This would be slightly faster and avoid the need apply cutoffs to the travel times down the line.
        # However, the slight gain in speed is not worth the loss in flexibility to me.
        # If it is worth it to you, feel free to create a PR to allow for this option!
        travel_time <- travel_time_matrix(
            r5r_core = r5_core, origins = new_origins, destinations = Paris_sp_sf, mode = "WALK",
            verbose = F, progress = T, max_trip_duration = max_time, walk_speed = 4
        )

        print(paste0("Chunk ", chunk, " of ", length(chunks), " completed."))

        travel_time[new_origins, on = .(from_id = new_id), origin_id := i.id]
        travel_time[destinations, on = .(to_id = new_id), dest_type := i.type]
        travel_time[destinations, on = .(to_id = new_id), dest_id := i.id]
        travel_time[destinations, on = .(to_id = new_id), year := i.year]

        arrow::write_parquet(travel_time, paste0(chunk_dir, "/", chunk, ".parquet"))

        gc()
        rJava::.jgc(R.gc = TRUE)
    }

    r5r::stop_r5(r5_core)
    rJava::.jgc(R.gc = TRUE)
    rm(travel_time)
    gc()
}
