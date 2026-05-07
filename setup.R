suppressPackageStartupMessages({
  library(tidyverse) # Standard stuff
  library(glue) # String concatenation
  library(sf) # Geospatial data
  # Note: As of 2025, you need to install a patch to get tigris to work. See
  # https://github.com/walkerke/tigris#:~:text=Is%20the%20package%20not%20working%20(April%202025)%3F
  library(tigris)
})

# Constants
lodes_base_url <- "https://lehd.ces.census.gov/data/lodes/LODES8/"
tiger_year <- 2024
tracts_base_url <- glue("https://www2.census.gov/geo/tiger/TIGER{tiger_year}/TRACT/")
state <- "il"
# NAD83, the CRS used by Tigris. We use this when we create the CCAs sf.
crs <- 4269
# Years to get LODES for
years <- c(2002, 2022)
# Paths
data_dir <- file.path("data")
tracts_path <- file.path(data_dir, "tracts.geojson")
cca_shapes_path <- file.path(data_dir, "cca_shapes_original.csv")
ccas_path <- file.path(data_dir, "ccas.geojson")
cca_distances_path <- file.path(data_dir, "cca_distances.csv")

# Create data directory if it doesn't exist
dir.create(data_dir, showWarnings = FALSE)

# 1. SPATIAL DATA

if (file.exists(ccas_path)) {
  message(glue("{ccas_path} already exists, so skipping download."))
  ccas <- read_sf(ccas_path)
} else {
  message("Fetching Chicago community area (CCA) shapes...")
  # This is geospatial data but the file is a CSV for some reason
  tmp_file <- tempfile(fileext = ".csv")
  download.file(
    # See https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
    "https://data.cityofchicago.org/api/views/igwz-8jzy/rows.csv",
    destfile = tmp_file,
  )

  ccas <- read_csv(tmp_file, show_col_types = FALSE)
  ccas <- ccas %>%
    mutate(
      .keep = "none",
      # Change from all upper case to title case
      name = str_to_title(COMMUNITY),
      # It's AREA_NUMBER. Not a typo.
      num = as.integer(AREA_NUMBE),
      geometry = the_geom,
    ) %>%
    st_as_sf(wkt = "geometry") %>%
    st_set_crs(crs)

    unlink(tmp_file)
    write_sf(ccas, ccas_path, delete_dsn = TRUE)
  message(glue("Saved community area shapes to {ccas_path}"))
}

if (file.exists(cca_distances_path)) {
  message(glue("{cca_distances_path} already exists, so leaving it as-is."))
  cca_distances <- read_csv(cca_distances_path, col_types = "ccd")
} else {
  message("Calculating pairwise distances between all CCAs...")
  # Get the distance between centroids of every pair of community areas
  cca_centroids <- suppressWarnings(ccas %>% st_centroid()) %>%
    select(name)
  # Note this includes from-to and to-from for every dyad
  cca_distances <- crossing(
    cca_centroids %>% rename(from = name, from_centroid = geometry),
    cca_centroids %>% rename(to = name, to_centroid = geometry),
  ) %>%
    mutate(
      .keep = "unused",
      distance = st_distance(from_centroid, to_centroid, by_element = TRUE, which = "Great Circle"),
    )
  write_csv(cca_distances, cca_distances_path)
  message(glue("Saved pairwise distances between CCA centroids to {cca_distances_path}"))
}

if (!("distance_from_loop" %in% colnames(ccas))) {
  distances_from_loop <- cca_distances %>%
    filter(from == "Loop") %>%
    select(to, distance) %>%
    rename(name = to, distance_from_loop = distance)
  ccas <- ccas %>%
    left_join(distances_from_loop, by = "name")
  write_sf(ccas, ccas_path, delete_dsn = TRUE)
  message("Added distance_from_loop column to the CCAs data frame")
}

if (file.exists(tracts_path)) {
  message(glue("{tracts_path} already exists, so skipping download."))
  # Load the already-fetched tracts shapefile
  tracts <- read_sf(tracts_path)
  # We need the tract-CCA relationships so we can aggregate the commuting data by CCA
  tract_cca_relationships <- tracts %>%
    filter(!is.na(cca)) %>%
    as_tibble() %>%
    mutate(.keep = "none", tract = GEOID, cca, cca_num)
} else {
  message(glue("Fetching tract shapes for state {toupper(state)}..."))
  # cb = FALSE is higher quality; cb = TRUE is faster
  tracts <- tigris::tracts(state, cb = TRUE) %>%
    select(GEOID, geometry)

  message("Determining relationships between tracts and community areas...")
  # This is done by finding the CCA containing the tract's centroid.
  tract_centroids <- suppressWarnings(tracts %>% st_centroid())
  # One row for every tract in Chicago
  tracts_with_cca <- tract_centroids %>%
    # left = FALSE causes it to drop tracts outside any CCA
    st_join(ccas, join = st_within, left = FALSE)

  # Double check that no tracts matched multiple CCAs
  dupes <- tracts_with_cca %>%
    count(GEOID) %>%
    filter(n > 1)
  if (nrow(dupes) > 0) {
    warning(glue("Some tracts matched multiple community areas: {paste(dupes$GEOID, collapse=', ')}"))
  }

  # Simple data frame relating tracts to CCAs
  tract_cca_relationships <- tracts_with_cca %>%
    as_tibble() %>%
    # Drops the geometry column
    select(GEOID, name, num) %>%
    rename(tract = GEOID, cca = name, cca_num = num)
  tracts <- tracts %>%
    # Join the CCA names and numbers. These will be NA for tracts outside of Chicago.
    left_join(tract_cca_relationships, by = join_by(GEOID == tract))
  message("Found the CCA for every tract!")

  write_sf(tracts, tracts_path, delete_dsn = TRUE)
  message(glue("Saved tract shapes to {tracts_path}, including a CCA column."))
}

# 2. LODES

for (year in years) {
  block_flows_path <- file.path(data_dir, glue("block_flows_{year}.csv.gz"))
  tract_flows_path <- file.path(data_dir, glue("tract_flows_{year}.csv"))
  cca_flows_path <- file.path(data_dir, glue("cca_flows_{year}.csv"))

  # Fetch LODES block-to-block flows in Illinois
  if (file.exists(block_flows_path)) {
    message(glue("{block_flows_path} already exists, so skipping download."))
  } else {
    part <- "main"
    type <- "JT00"

    message(glue("Fetching LODES data for {toupper(state)} in {year}..."))
    url <- glue("{lodes_base_url}{state}/od/{state}_od_{part}_{type}_{year}.csv.gz")
    # Sometimes it takes more than 60 seconds to download. This is 5 minutes.
    options(timeout = 300)
    download.file(url, destfile = block_flows_path)
    message(glue("Downloaded data to {block_flows_path}"))
  }

  # Aggregate from census blocks to tracts
  if (file.exists(tract_flows_path)) {
    message(glue("{tract_flows_path} already exists, so leaving it as-is."))
    tract_flows <- read_csv(tract_flows_path, col_types = "cci")
  } else {
    message(glue("Aggregating commuting flows to the census tract level (year = {year})..."))

    # This has about 5 million rows FYI
    original <- read_csv(
      block_flows_path,
      # Only load the columns we really need
      col_select = c("w_geocode", "h_geocode", "S000"),
      col_types = "cci"
    )

    # This has about 1 million rows (only hehe)
    tract_flows <- original %>%
      mutate(
        h_tract = substr(h_geocode, 1, 11),
        w_tract = substr(w_geocode, 1, 11)
      ) %>%
      group_by(w_tract, h_tract) %>%
      summarize(n = sum(S000), .groups = "drop")
    write_csv(tract_flows, tract_flows_path)
    message(glue("Saved {tract_flows_path}"))
  }

  if (file.exists(cca_flows_path)) {
    message(glue("{cca_flows_path} already exists, so leaving it as-is."))
    cca_flows <- read_csv(cca_flows_path, col_types = "ccid")
  } else {
    message(glue("Aggregating commuting flows to the community area level (year = {year})..."))
    # Next, we're going to aggregate the flows to the community area level, just
    # like we did when we aggregated blocks to tracts.
    cca_flows <- tract_flows %>%
      left_join(
        tract_cca_relationships,
        by = join_by(h_tract == tract)
      ) %>%
      # `from` is where people live; `to` is where they work
      rename(from = cca) %>%
      left_join(
        tract_cca_relationships,
        by = join_by(w_tract == tract)
      ) %>%
      rename(to = cca) %>%
      # Only include flows between Chicago community areas
      filter(!is.na(from), !is.na(to), from != to) %>%
        group_by(from, to) %>%
        summarize(
          n = sum(n),
          .groups = "drop"
        )

    write_csv(cca_flows, cca_flows_path)
    message(glue("Saved {cca_flows_path}"))
  }
}

# 3. LOAD THE LODES (hehe)
cca_flows_2022 <- read_csv(file.path(data_dir, "cca_flows_2022.csv"), col_types = "cci")
cca_flows_2002 <- read_csv(file.path(data_dir, "cca_flows_2002.csv"), col_types = "cci")

message("Ready!")
