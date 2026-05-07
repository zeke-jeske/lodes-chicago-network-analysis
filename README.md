# Network Analysis of Chicago Inter-Community Area Commuting

To run the analysis, simply run all cells of `main.Rmd`, which sources `setup.R` to fetch the
needed data.

I apologize that this code is a bit of a mess. Normally my coding style is fairly perfectionist, but once
in a while I think it's good to let myself "move fast and break things." :)

## Data Sources

### LODES

**[LODES Documentation](https://lehd.ces.census.gov/doc/help/onthemap/LODESTechDoc.pdf)**

- 3 data files. OD is origin-destination data, which is the one we need
- File name: `[ST]_od_[PART]_[TYPE]_[YEAR].csv.gz`, where
  - `[ST]`: the state code (e.g. `il` for Illinois)
  - `[PART]`: `main` for residence and workplace in the state, or `aux` for
    workplace in the state but residence outside the state
  - `[TYPE]`: job type. `JT00` for All Jobs
  - `[YEAR]`: 4-digit year. 2000-2022.
- Columns in the data:
  - `w_geocode`: workplace census block code. This is a 15-digit FIPS code. The
    first 11 digits represent the tract.
  - `h_geocode`: residence census block code
  - `S000`: number of jobs
  - `SXXX`: other related variables that could be useful later on

### Census tract shapefiles

We need these to graph the census tracts. [Documentation
here.](https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/tiger-geo-line.html)

### Census tract to Chicago Community Areas (CCAs) relationships

These data are provided by [UChicago's Spatial Data
Resources](https://guides.lib.uchicago.edu/c.php?g=720045&p=8072546).
