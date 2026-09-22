# Independent published examples and mathematical integrity checks.
source("experiments/p15_wb_scorecard_20260912/scorecard_rules.R")
out_dir <- "experiments/p15_wb_scorecard_20260912/methodology"
m <- scorecard_load_matrices()

# Moody's 14 Aug 2019 Uruguay annual credit analysis, printed p17, Exhibit27.
# The publication explicitly cites the 27 November 2018 methodology. These
# examples check the historical matrices against independent later outcomes.
examples <- data.frame(
  country = c("Uruguay", "Bahamas", "Colombia", "Romania", "Indonesia", "South Africa"),
  f1 = c("M", "M-", "H", "M+", "H+", "M+"),
  f2 = c("M+", "M+", "M", "M", "M", "M+"),
  f3 = c("M-", "L", "M-", "M+", "M", "M+"),
  f4 = c("L", "L+", "L+", "M-", "L", "L+"),
  published_better = c("Baa2", "Baa3", "Baa1", "Baa2", "Baa1", "Baa1"),
  published_worse = c("Ba1", "Ba2", "Baa3", "Ba1", "Baa3", "Baa3"))
got <- with(examples, scorecard_aggregate(f1, f2, f3, f4, m))
examples$computed_better <- got$rating_range_better
examples$computed_worse <- got$rating_range_worse
examples$passed <- examples$published_better == examples$computed_better &
  examples$published_worse == examples$computed_worse
stopifnot(all(examples$passed))
write.csv(examples, file.path(out_dir, "independent_country_example_checks.csv"), row.names = FALSE)

# Worsening one input must never improve the result. Tests every adjacent
# horizontal and vertical cell, catching transcription and axis-direction errors.
checks <- data.frame(check = character(), passed = logical(), detail = character())
record <- function(check, passed, detail) {
  checks[nrow(checks) + 1L, ] <<- list(check, isTRUE(passed), detail)
  if (!isTRUE(passed)) stop(check, " failed: ", detail)
}
for (name in c("economic_resiliency", "government_financial_strength", "rating_midpoint")) {
  a <- m[[name]][if (name == "rating_midpoint") rev(scorecard_strength_labels) else
                  scorecard_strength_labels, scorecard_strength_labels]
  b <- matrix(match(a, if (name == "rating_midpoint") scorecard_rating_labels else
                      scorecard_strength_labels), nrow = 15L)
  record(paste(name, "all_cells_defined"), !anyNA(b), "225 source cells")
  record(paste(name, "monotonic_rows"), all(apply(b, 1L, function(z) all(diff(z) >= 0))),
         "210 adjacent column comparisons")
  record(paste(name, "monotonic_columns"), all(apply(b, 2L, function(z) all(diff(z) >= 0))),
         "210 adjacent row comparisons")
}
record("economic_resiliency_symmetric", identical(m$economic_resiliency, t(m$economic_resiliency)),
       "Published first-stage equal treatment of economic and institutional strength")
missing <- scorecard_aggregate("M", NA_character_, "M")
record("missing_factor_propagates", is.na(missing$rating_notch), "No midpoint imputation")
bad <- tryCatch({ scorecard_aggregate("made_up", "M", "M"); FALSE }, error = function(e) TRUE)
record("invalid_label_rejected", bad, "No silent coercion into a published category")
z <- scorecard_factor_bounds(c(7L, 8L), c(7L, 8L), c(7L, 9L), c(5L, 8L), m)
record("uncertainty_enumerates_all_combinations", z$combinations == 48L,
       "2 x 2 x 3 x 4 feasible factor combinations")
record("independent_country_examples", all(examples$passed), "Six published 2019 ranges reproduced exactly")
b <- scorecard_empty_2018_bundle()
blocked <- scorecard_score_2018(data.frame(country = c("Example1", "Example2")), b)
record("incomplete_bundle_never_emits_ratings", all(is.na(blocked$rating_notch)) &&
         all(blocked$scorecard_status == "rules_incomplete"), "Two input rows retained; neither obtains invented rating")
b$complete <- TRUE
record("complete_flag_cannot_override_missing_rules", length(scorecard_bundle_gaps(b)) > 0L,
       "Evidence and callbacks still required after complete flag is set")
b$method_id <- "PBC_1158631"
record("2019_method_cannot_pass_as_2018", "exact_method_identity" %in% scorecard_bundle_gaps(b),
       "Explicit method-ID/date gate")
write.csv(checks, file.path(out_dir, "component_check_results.csv"), row.names = FALSE)
cat(nrow(checks), "component checks and", nrow(examples), "independent country examples passed.\n")
