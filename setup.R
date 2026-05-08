suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(sf)
  # As of 2025, tigris requires a patch to work:
  # https://github.com/walkerke/tigris#is-the-package-not-working-april-2025
  library(tigris)
})

# ── Constants ─────────────────────────────────────────────────────────────────

state <- "il"
crs <- 4269 # NAD83, the CRS used by tigris
years <- c(2002, 2022)

# LODES OD: all jobs (JT00), in-state (main) workers
lodes_base_url <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
lodes_part <- "main"
lodes_type <- "JT00"

# ── Paths ─────────────────────────────────────────────────────────────────────

data_dir <- "data"
ccas_path <- file.path(data_dir, "ccas.geojson")
cca_distances_path <- file.path(data_dir, "cca_distances.csv")
tracts_path <- file.path(data_dir, "tracts.geojson")

dir.create(data_dir, showWarnings = FALSE)

# Large downloads can exceed 60 s; allow up to 5 minutes
options(timeout = 300)

# ── 1. Community area (CCA) shapes ────────────────────────────────────────────

if (file.exists(ccas_path)) {
  message(glue("{ccas_path} already exists, skipping download."))
  ccas <- read_sf(ccas_path)
} else {
  message("Fetching Chicago community area (CCA) shapes...")
  # The city publishes this as CSV with WKT geometry, not as a shapefile.
  # https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
  tmp_file <- tempfile(fileext = ".csv")
  download.file("https://data.cityofchicago.org/api/views/igwz-8jzy/rows.csv", destfile = tmp_file)

  ccas <- read_csv(tmp_file, show_col_types = FALSE) %>%
    transmute(
      name     = str_to_title(COMMUNITY),
      num      = as.integer(AREA_NUMBE), # AREA_NUMBE is truncated in the source, not a typo here
      geometry = the_geom,
    ) %>%
    st_as_sf(wkt = "geometry") %>%
    st_set_crs(crs)

  unlink(tmp_file)
  write_sf(ccas, ccas_path, delete_dsn = TRUE)
  message(glue("Saved CCA shapes to {ccas_path}"))
}

# ── 2. Pairwise distances between CCA centroids ───────────────────────────────

if (file.exists(cca_distances_path)) {
  message(glue("{cca_distances_path} already exists, leaving as-is."))
  cca_distances <- read_csv(cca_distances_path, col_types = "ccd")
} else {
  message("Calculating pairwise distances between all CCA centroids...")
  cca_centroids <- suppressWarnings(st_centroid(ccas)) %>% select(name)
  # crossing() produces both A→B and B→A for every pair
  cca_distances <- crossing(
    cca_centroids %>% rename(from = name, from_centroid = geometry),
    cca_centroids %>% rename(to = name, to_centroid = geometry),
  ) %>%
    mutate(
      .keep    = "unused",
      distance = st_distance(from_centroid, to_centroid, by_element = TRUE, which = "Great Circle"),
    )
  write_csv(cca_distances, cca_distances_path)
  message(glue("Saved pairwise CCA distances to {cca_distances_path}"))
}

if (!("distance_from_loop" %in% colnames(ccas))) {
  ccas <- ccas %>%
    left_join(
      cca_distances %>%
        filter(from == "Loop") %>%
        transmute(name = to, distance_from_loop = distance),
      by = "name"
    )
  write_sf(ccas, ccas_path, delete_dsn = TRUE)
  message("Added distance_from_loop column to CCAs.")
}

# ── 3. Census tract shapes ────────────────────────────────────────────────────

if (file.exists(tracts_path)) {
  message(glue("{tracts_path} already exists, skipping download."))
  tracts <- read_sf(tracts_path)
  # Needed later to aggregate LODES block flows up to the CCA level
  tract_cca_relationships <- tracts %>%
    filter(!is.na(cca)) %>%
    as_tibble() %>%
    transmute(tract = GEOID, cca, cca_num)
} else {
  message(glue("Fetching tract shapes for {toupper(state)}..."))
  # cb = TRUE uses the smaller cartographic boundary file (sufficient for joins)
  tracts <- tigris::tracts(state, cb = TRUE) %>% select(GEOID, geometry)

  message("Assigning each tract to its CCA...")
  # A tract belongs to the CCA that contains its centroid
  tracts_with_cca <- suppressWarnings(st_centroid(tracts)) %>%
    st_join(ccas, join = st_within, left = FALSE) # left = FALSE drops tracts outside any CCA

  dupes <- tracts_with_cca %>%
    count(GEOID) %>%
    filter(n > 1)
  if (nrow(dupes) > 0) {
    warning(glue("These tracts matched multiple CCAs: {paste(dupes$GEOID, collapse = ', ')}"))
  }

  tract_cca_relationships <- tracts_with_cca %>%
    as_tibble() %>%
    transmute(tract = GEOID, cca = name, cca_num = num)

  tracts <- tracts %>%
    # CCA columns will be NA for tracts outside Chicago
    left_join(tract_cca_relationships, by = join_by(GEOID == tract))

  write_sf(tracts, tracts_path, delete_dsn = TRUE)
  message(glue("Saved tract shapes (with CCA assignments) to {tracts_path}"))
}

# ── 4. LODES origin-destination flows ─────────────────────────────────────────

cca_flows_by_year <- list()

for (year in years) {
  block_flows_path <- file.path(data_dir, glue("block_flows_{year}.csv.gz"))
  tract_flows_path <- file.path(data_dir, glue("tract_flows_{year}.csv"))
  cca_flows_path <- file.path(data_dir, glue("cca_flows_{year}.csv"))

  # Block-level flows ----------------------------------------------------------
  if (file.exists(block_flows_path)) {
    message(glue("{block_flows_path} already exists, skipping download."))
  } else {
    message(glue("Downloading LODES OD data for {toupper(state)} ({year})..."))
    url <- glue("{lodes_base_url}{state}/od/{state}_od_{lodes_part}_{lodes_type}_{year}.csv.gz")
    download.file(url, destfile = block_flows_path)
    message(glue("Saved {block_flows_path}"))
  }

  # Aggregate to census tracts -------------------------------------------------
  if (file.exists(tract_flows_path)) {
    message(glue("{tract_flows_path} already exists, leaving as-is."))
    tract_flows <- read_csv(tract_flows_path, col_types = "cci")
  } else {
    message(glue("Aggregating block flows to tracts ({year})..."))
    # ~5 million rows; load only the three columns we need
    tract_flows <- read_csv(
      block_flows_path,
      col_select = c("w_geocode", "h_geocode", "S000"),
      col_types  = "cci"
    ) %>%
      mutate(
        h_tract = substr(h_geocode, 1, 11),
        w_tract = substr(w_geocode, 1, 11),
      ) %>%
      group_by(w_tract, h_tract) %>%
      summarize(n = sum(S000), .groups = "drop")
    write_csv(tract_flows, tract_flows_path)
    message(glue("Saved {tract_flows_path}"))
  }

  # Aggregate to community areas -----------------------------------------------
  if (file.exists(cca_flows_path)) {
    message(glue("{cca_flows_path} already exists, leaving as-is."))
    cca_flows_by_year[[as.character(year)]] <- read_csv(cca_flows_path, col_types = "cci")
  } else {
    message(glue("Aggregating tract flows to CCAs ({year})..."))
    # h = home tract (where people live); w = work tract (where they commute to)
    cca_flows <- tract_flows %>%
      left_join(tract_cca_relationships, by = join_by(h_tract == tract)) %>%
      rename(from = cca) %>%
      left_join(tract_cca_relationships, by = join_by(w_tract == tract)) %>%
      rename(to = cca) %>%
      filter(!is.na(from), !is.na(to), from != to) %>%
      group_by(from, to) %>%
      summarize(n = sum(n), .groups = "drop")
    write_csv(cca_flows, cca_flows_path)
    cca_flows_by_year[[as.character(year)]] <- cca_flows
    message(glue("Saved {cca_flows_path}"))
  }
}

cca_flows_2002 <- cca_flows_by_year[["2002"]]
cca_flows_2022 <- cca_flows_by_year[["2022"]]

message("Ready!")
