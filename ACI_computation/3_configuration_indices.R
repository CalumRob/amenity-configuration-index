compute_tci <- function(origins_to_types, rolling_window, return_similarity = FALSE) {
    require(data.table)
    require(arrow)

    years <- unique(origins_to_types$year)
    full_dt <- data.table()
    
    # Identify amenity type columns (all columns except origin_id and year)
    type_cols <- names(origins_to_types)[!(names(origins_to_types) %in% c("origin_id", "year"))]

    for (current_year in years) {
        # Window of years for the rolling calculation
        good_years <- years[years %in% (current_year - rolling_window):(current_year + rolling_window)]

        # Stack data from all years in the window (already wide)
        temp_build <- origins_to_types[year %in% good_years]
        
        # Identify columns with data in this window
        window_mat_cols <- type_cols[colSums(temp_build[, ..type_cols], na.rm = TRUE) > 0]
        
        # Create numeric matrix (observations are building-years)
        mat <- as.matrix(temp_build[, ..window_mat_cols])
        rownames(mat) <- paste0(temp_build$year, "_", temp_build$origin_id)
        mat[is.na(mat)] <- 0

        gc()

        # Cleaning: Remove entirely empty rows/cols
        mat <- mat[rowSums(mat) > 0, ]
        mat <- mat[, colSums(mat) > 0]
        mat <- mat[complete.cases(mat), ]

        # Presence-absence matrix for ubiquity and co-occurrence computation
        rca <- mat
        rca[rca < 1] <- 0
        rca[rca > 0] <- 1

        # Ubiquity : The share of buildings that have access to a given amenity type, on avg across years
        # Adjusted for the multi-year stack
        num_obs <- nrow(rca)
        ubiquity <- (colSums(rca) / length(good_years)) / (num_obs / length(good_years))

        # Markov-chain-like similarity matrix
        tm <- (t(mat) / colSums(mat)) %*% (mat / rowSums(mat))

        if (isTRUE(return_similarity)) {
            return(tm)
        }

        # Eigen decomposition
        eig <- eigen(tm)
        magnitudes <- Mod(eig$values)
        ordering <- order(magnitudes, decreasing = TRUE)
        second_leading_eigenvalue <- eig$values[ordering[2]]
        second_leading_eigenvector <- eig$vectors[, ordering[2]]

        if (any(Im(second_leading_eigenvalue) > 0) | any(Im(second_leading_eigenvector) > 0)) {
            stop("The second leading eigenvalue or eigenvector is complex.")
        } else {
            TCI <- Re(second_leading_eigenvector)
        }

        # Orientation fix based on ubiquity
        if (cor(TCI, ubiquity, use = "pairwise.complete.obs", method = "spearman") > 0) {
            TCI <- TCI * (-1)
        }

        TCI_unsc <- TCI
        TCI <- (TCI - abs(mean(TCI, na.rm = TRUE))) / sd(TCI)

        dt <- data.table(
            type = colnames(mat),
            Complexity = as.numeric(TCI),
            Complexity_unscaled = as.numeric(TCI_unsc),
            ubiquity = ubiquity,
            year = current_year
        )

        dt[, rank := rank(-Complexity)]
        
        # avg_diversity: Average diversity of buildings that have this amenity type
        diversity_vec <- rowSums(rca)
        avg_diversity <- (1 / colSums(rca)) * colSums(rca * diversity_vec)
        dt$avg_diversity <- avg_diversity

        full_dt <- rbind(full_dt, dt)
    }

    return(full_dt)
}

compute_aci <- function(origins_to_types, tci_data) {
    require(data.table)

    years <- unique(origins_to_types$year)
    full_dt <- data.table()
    
    # Identify type columns in origins_to_types
    type_cols <- names(origins_to_types)[!(names(origins_to_types) %in% c("origin_id", "year"))]

    for (current_year in years) {
        # Subset wide building data and long TCI data for the year
        mat_dt <- origins_to_types[year == current_year]
        tci_yr <- tci_data[year == current_year]
        
        # Intersect keys to align matrix and vectors
        active_types <- intersect(type_cols, tci_yr$type)
        
        # Extract numeric matrix
        mat <- as.matrix(mat_dt[, ..active_types])
        mat[is.na(mat)] <- 0
        
        # Align TCI/Ubiquity vectors to the matrix columns
        TCI_vec <- tci_yr[match(active_types, type), Complexity_unscaled]
        UBI_vec <- tci_yr[match(active_types, type), ubiquity]
        
        # Performance algebra: Weighted sums / Total accessibility (rowSums)
        RS <- rowSums(mat)
        ACI_unsc <- as.numeric((mat %*% TCI_vec) / RS)
        AVG_UBI <- as.numeric((mat %*% UBI_vec) / RS)
        DIV <- as.numeric(rowSums(mat > 0))
        
        # Compile yearly building results
        year_results <- data.table(
            origin_id = mat_dt$origin_id,
            year = current_year,
            ACI_unscaled = ACI_unsc,
            diversity = DIV,
            avg_ubiquity = AVG_UBI
        )
        
        # Handle zero-accessibility buildings
        year_results[is.na(ACI_unscaled), ACI_unscaled := 0]
        year_results[is.na(avg_ubiquity), avg_ubiquity := 0]

        # Standardize ACI for cross-year comparison
        year_results[, ACI := (ACI_unscaled - mean(ACI_unscaled)) / sd(ACI_unscaled)]
        year_results[, rank := rank(-ACI)]
        
        full_dt <- rbind(full_dt, year_results)
    }

    return(full_dt)
}
