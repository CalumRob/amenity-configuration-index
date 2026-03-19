# Amenity Configuration Index (ACI)

This repository contains the tools and data treatment used to compute the **Area Configuration Index (ACI)** and **Type Configuration Index (TCI)** as described in Robertson, et al. (2026) ([SSRN: 5864410](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5864410)) and presented via an interactive dashboard at [aciparis.calumrobertson.fr](https://aciparis.calumrobertson.fr).

## Project Structure

- **[ACI_computation/](file:///e:/ACI_app/ACI_Paris/ACI_computation)**: A modular pipeline designed to be applied to any city.
- **[Replicate_Paris/](file:///e:/ACI_app/ACI_Paris/Replicate_Paris)**: Scripts and data used to replicate the specific results and figures for Paris presented in our paper.

## Requirements

### Software
- **R**: Required versions of libraries: `data.table`, `sf`, `dplyr`, `r5r`, and `rJava`.
- **Java JDK**: A compatible Java JDK (**>= 21.0.0**) is required for routing calculations via `r5r`. See the [r5r documentation](https://ipeagit.github.io/r5r/) for more details.


---

## Replicate Paper Results

The `Replicate_Paris` directory is designed for those wishing to reproduce the findings in our paper. It requires only the software listed above and the data provided within the folder.

1.  Navigate to `Replicate_Paris/`.
2.  Open `main.R`.
3.  Ensure paths and memory limits (e.g., `rjava_memory`) are appropriate for your machine.
4.  Run the script to generate ACI/TCI results and replicate figures.

---

## Compute ACI for Any City

The `ACI_computation` directory provides a modular framework for applying the ACI to new urban contexts.

### Inputs Required

1.  **Origins Set**: A dataset of origin points (e.g., building points, centroids, or grid points) with columns `id`, `lon`, and `lat` or an sfpoints object with column `id`.
2.  **Amenities Set**: A dataset of amenities containing coordinates (`lon`, `lat`), a `year`, and a `type` classification or an sfpoints object with columns `id`, `year`, and `type`.
3.  **Street Network**: An OpenStreetMap extract in `.pbf` format for the target area.

### Usage

1.  Place your data in the `ACI_computation/data/` directory.
2.  Configure your settings in `ACI_computation/main.R` (e.g., paths, walking speed, time thresholds).
3.  The pipeline will:
    - **Route**: Compute travel times between origins and amenities.
    - **Aggregate**: Count reachable amenities per type within a time threshold (default 15 mins).
    - **Compute**: Calculate TCI (amenity type configuration index) and ACI (amenity configuration index of places) scores.

