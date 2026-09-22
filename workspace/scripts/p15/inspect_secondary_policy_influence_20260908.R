# Read-only diagnostic. Run from the project root. No data or selection changes.
# Outcome-ranked omissions measure influence, not an admissible filtered sample.
path <- "data-derived/p15_official_policy_comparison_20260908_v1/paired_details.csv"
d <- read.csv(path)
x <- subset(d, tier == "secondary" & policy == "dac_modern_2018")
stopifnot(nrow(x) == 117L, !anyDuplicated(x[c("iso3", "analysis_year")]))
stopifnot(all(abs(x$improvement -
  (abs(x$policy_error) - abs(x$tier_error))) < 1e-10))
x <- x[order(-abs(x$tier_error), x$iso3, x$analysis_year), ]
print(head(x[c("iso3", "analysis_year", "primary", "tier_rate",
               "policy_rate", "tier_error", "improvement")], 12), row.names = FALSE)
for (k in c(0L, 1L, 3L, 6L)) {
  z <- if (k == 0L) x else x[-seq_len(k), ]
  print(data.frame(omitted_largest_gaps = k, n = nrow(z),
    secondary_mae = mean(abs(z$tier_error)),
    policy_mae = mean(abs(z$policy_error)), gain = mean(z$improvement)))
}
print(data.frame(win_share = mean(x$improvement > 0),
  median_gain = median(x$improvement),
  top_six_share_of_absolute_gap = sum(abs(x$tier_error[1:6])) /
    sum(abs(x$tier_error))))
restricted <- with(x, (iso3 == "LBN" & analysis_year %in% 2020:2023) |
  (iso3 == "BLR" & analysis_year %in% 2022:2024) |
  (iso3 == "RUS" & analysis_year == 2022))
print(data.frame(previous_eight_restricted_cases_in_this_sample = sum(restricted)))
