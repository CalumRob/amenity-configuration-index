# UNDER CONSTRUCTION

This repository currently holds the data treatment used to generate the results in Robertson, et al. (2026) ([SSRN: 5864410](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5864410)) and the interactive web application [aciparis.calumrobertson.fr](https://aciparis.calumrobertson.fr).

It is currently undergoing changes to make it more modular and easier to use for other cities.

# ACI/TCI Computation

The pipeline computes the **Area Configuration Index (ACI)** and **amenity-Type Configuration Index (TCI)** using walking distances around residential buildings.

## Requirements

### Software
- **R**: Including packages `data.table`, `sf`, `dplyr`, `r5r`, and `rJava`.
- **Java JDK**: An `r5r`-compatible Java JDK (>= 21.0) for routing calculations.
- **Tippecanoe** (Optional): Required only if generating web-ready PMTiles map artifacts for the webapp.

### Inputs
- **Residential Building Origins**: A CSV (e.g., `building_as_origins_reduced.csv`) with at least `id`, `lon`, and `lat`.
- **Amenity Destinations**: Point-of-interest data for desired years containing coordinates and an amenity class code (`CODE_ACTIVITE`).
- **Transport Network**: OpenStreetMap `.pbf` network graph for `r5r` routing.
- **Environment config (.env)**: A `.env` file configuring `ACI_PARIS_ROOT` and optional database connections required by `utils.R`.

## Usage

1. **Prepare Geodata & Config**: Ensure inputs are placed in expected directories and update hardware limits (`-Xmx`) and path references in the `.R` scripts.
2. **Generate Travel Times**: Run `r5r_bat_to_amen_func.R` to generate origin-to-destination accessibility matrices.
3. **Compile Amenity Counts**: Run `r5r_count_type_per_bat.R` and `count_type_bat_allyears.R` to aggregate the reachable amenities within a selected time threshold (e.g., a 15-minute walk).
4. **Compute ACI/TCI**: Run `replicate_aci.R` to apply the calculation, outputting `ACI_all_rolling.csv` and `TCI_all_rolling.csv`.
5. **(Optional) Generate Web Map Data**: Run `export_pmtiles_geojson.R` to merge topologies with complexity scores and generate a `tippecanoe` bash script to output `.pmtiles`.
6. **(Optional) Replicate Paper Figures**: Run `paper_figures.R` and `random_forest.R` for ACI/TCI analysis.
