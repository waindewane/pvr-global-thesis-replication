# Bounded source inspection only: no joins to the research panel or regressions.
library(readxl)
library(jsonlite)
root <- 'sources/literature_review/thesis_concepts_20260911'
out <- file.path(root, 'extracted')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
obi <- read_excel(file.path(root, 'originals/obs_full_timeseries_2006_2025.xlsx'),
                  sheet = 'OBS_Data_AllYears', skip = 3)
stopifnot(all(c('Country', 'ISO', 'Year', 'OBI (unrounded)') %in% names(obi)))
stopifnot(!anyDuplicated(obi[c('ISO', 'Year')]))
obi_selected <- data.frame(country = obi$Country, iso3 = obi$ISO,
  survey_year = obi$Year, source_region = obi$Region,
  open_budget_index = obi[['OBI (unrounded)']],
  source_row = seq_len(nrow(obi)) + 4L,
  source_sheet = 'OBS_Data_AllYears', stringsAsFactors = FALSE)
obi_selected <- obi_selected[obi_selected$survey_year >= 2012 &
                               obi_selected$survey_year <= 2024, ]
write.csv(obi_selected, file.path(out, 'open_budget_index_2012_2024_available_rounds.csv'),
          row.names = FALSE, na = '')
obi_counts <- do.call(rbind, lapply(sort(unique(obi_selected$survey_year)), function(y) {
  z <- obi_selected[obi_selected$survey_year == y, ]
  data.frame(dataset = 'OBS', year = y, rows = nrow(z),
             nonmissing_values = sum(is.finite(z$open_budget_index)),
             unique_nonempty_iso3 = length(unique(z$iso3[nzchar(z$iso3)])))
}))
wgi_raw <- fromJSON(file.path(root, 'originals/wgi_regulatory_quality_current_2012_2024.json'),
                    simplifyVector = FALSE)
stopifnot(length(wgi_raw) == 2L, !is.null(wgi_raw[[1]]$sourceid))
wgi <- do.call(rbind, lapply(wgi_raw[[2]], function(z) data.frame(
  country = z$country$value, iso3 = z$countryiso3code, year = as.integer(z$date),
  regulatory_quality_estimate = if (is.null(z$value)) NA_real_ else z$value,
  indicator = z$indicator$id, stringsAsFactors = FALSE)))
# Empty API country codes are preserved, not guessed or automatically mapped.
stopifnot(!anyDuplicated(wgi[c('country', 'year')]))
write.csv(wgi, file.path(out, 'wgi_regulatory_quality_2012_2024.csv'), row.names = FALSE, na = '')
wgi_counts <- do.call(rbind, lapply(sort(unique(wgi$year)), function(y) {
  z <- wgi[wgi$year == y, ]
  data.frame(dataset = 'WGI_2025_revision', year = y, rows = nrow(z),
             nonmissing_values = sum(is.finite(z$regulatory_quality_estimate)),
             unique_nonempty_iso3 = length(unique(z$iso3[nzchar(z$iso3)])))
}))
write.csv(rbind(obi_counts, wgi_counts), file.path(out, 'public_covariate_coverage.csv'),
          row.names = FALSE)
write_json(list(obs_rows_all_rounds = nrow(obi), obs_rows_2012_2024 = nrow(obi_selected),
  obs_years = sort(unique(obi_selected$survey_year)),
  obs_countries_2012_2024 = length(unique(obi_selected$iso3)),
  wgi_metadata = wgi_raw[[1]], wgi_rows = nrow(wgi),
  wgi_nonmissing = sum(is.finite(wgi$regulatory_quality_estimate)),
  wgi_nonempty_iso3_count = length(unique(wgi$iso3[nzchar(wgi$iso3)])),
  wgi_countries_with_empty_iso3 = unique(wgi$country[!nzchar(wgi$iso3)]),
  analysis_status = 'Source inspection only; no panel join or explanatory model; no interpolated observations'),
  file.path(out, 'public_covariate_inspection.json'), pretty = TRUE, auto_unbox = TRUE)
print(rbind(obi_counts, wgi_counts), row.names = FALSE)
