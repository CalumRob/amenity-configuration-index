library(data.table)
library(tidyverse)

data_base_dir <- "Replicate Paris/data"
bdcom_dir <- file.path(data_base_dir, "BDCom")

all_data <- data.table(
    CODE_ACTIVITE = character(),
    X = numeric(),
    Y = numeric(),
    BIO = character(),
    id = character(),
    Year = character()
)

for (destination_year in c(2014, 2017, 2020, 2023)) {
    # 2014 has diff csv construction
    if (destination_year == 2014) {
        dest_csv_path <- file.path(bdcom_dir, "commercesparis.csv")
        if (!file.exists(dest_csv_path)) stop(paste("2014 destinations CSV not found:", dest_csv_path))
        dest_data_raw <- readr::read_delim(dest_csv_path, delim = ";", show_col_types = FALSE) %>% # Handle potential weird X column name
            rename(CODE_ACTIVITE = any_of(c("CODE_ACTIVITE", "CODE.ACTIVITE", "Code activité (224 postes)", "CODE ACTIVITE"))) %>%
            mutate(BIO = "") %>%
            select(ORDRE, CODE_ACTIVITE, X, Y, BIO) %>%
            rename(id = ORDRE) %>%
            mutate(Year = as.character(destination_year)) # Handle variable col name
    } else {
        # For 2017, 2020, 2023
        dest_csv_path <- file.path(bdcom_dir, paste0("BDCOM_", destination_year, ".csv"))
        if (!file.exists(dest_csv_path)) stop(paste(destination_year, "destinations CSV not found:", dest_csv_path))
        dest_data_raw <- readr::read_csv(dest_csv_path, show_col_types = FALSE) %>% # Handle potential weird X column name
            rename(
                CODE_ACTIVITE = any_of(c("CODE_ACTIVITE", "CODE.ACTIVITE", "codact")),
                ORDRE = any_of(c("c_ord", "ORDRE")),
                BIO = any_of(c("BIO", "Bio", "bio"))
            ) %>%
            select(ORDRE, CODE_ACTIVITE, X, Y, BIO) %>%
            rename(id = ORDRE) %>%
            mutate(Year = as.character(destination_year))
    }

    all_data <- rbind(all_data, dest_data_raw)
}

# all_data <- all_data %>% pivot_wider(id_cols = c(1:5), names_from = "Year",
#                                      values_from = "Year", values_fill = "0")
# length(unique(all_data$id))

# BIO variable only in some years.
# In a specific year, organic grocery stores had their own code (CA302). In others, BIO variable (with different syntaxes for "yes") was used.
# We don't differentiate organic/non-organic for others that didn't have the "own code" issue
all_data <- all_data %>%
    mutate(CODE_ACTIVITE = ifelse(((BIO %in% c("Oui", "OUI", "oui", "1")) & CODE_ACTIVITE == "CA302"),
        "CA115", CODE_ACTIVITE
    )) %>%
    group_by(id, CODE_ACTIVITE) %>%
    summarise(
        X = median(X),
        Y = median(Y),
        Year_2014 = ifelse("2014" %in% Year, 1, 0),
        Year_2017 = ifelse("2017" %in% Year, 1, 0),
        Year_2020 = ifelse("2020" %in% Year, 1, 0),
        Year_2023 = ifelse("2023" %in% Year, 1, 0)
    )

# Filtered out amenities (mainly public services)
out_all <- c(
    "AA101", "AA102", "CI201", "SA506",
    "AB102", "AC102", "AF102", "AA102", "AF104", "AB106",
    "AB103", "AB104", "AB105", "AF103", "AB101", "af104",
    "AF104", "aa101", "AA101", "Af104", "Af102", "af102",
    "SB301", "",
    "", "CA305", "SA506", "CI201"
)

all_data <- all_data %>%
    mutate(CODE_ACTIVITE = toupper(CODE_ACTIVITE)) %>%
    mutate(id = as.character(id)) %>%
    dplyr::filter(!(CODE_ACTIVITE %in% out_all) & !is.na(X)) %>%
    dplyr::rename(lon = X, lat = Y, type = CODE_ACTIVITE) %>%
    dplyr::select(id, lon, lat, type)

all_data[, "Car_related" := "CG102" + "CG103" + "CG104" + "CG201" + "CG202"] # Aggregating car related amenities
all_data[, "Motorbike_related" := "CG106" + "CG204"] # Aggregating motorbike related amenities

fwrite(all_data, file.path(bdcom_dir, "BDCOM_14_to_23_cleaned.csv"))
