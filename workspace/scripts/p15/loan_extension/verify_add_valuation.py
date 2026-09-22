#!/usr/bin/env python3
"""Independent standard-library arithmetic audit of an ADD comparison output."""
import csv
import math
import statistics
import sys
from pathlib import Path

path = Path(sys.argv[1] if len(sys.argv) > 1 else
            "data-derived/p15_loan_comparisons_20260910_v2")
rows = list(csv.DictReader((path / "loan_valuations.csv").open()))
sample = [r for r in rows if r["dataset"] == "add" and
          r["matched"] == "TRUE" and r["non_peer"] == "TRUE" and
          r["central_scope_eligible"] == "TRUE"]
assert all(r["borrower_type"] == "Central government" for r in sample)
assert not any(r["benchmark_selected_tier"] == "peer" for r in sample)
errors = []
for r in sample:
    coupon, maturity, first, rate = map(float, [r["interest_rate_pct"],
        r["maturity_years"], r["first_principal_payment_years"],
        r["benchmark_selected_rate_pct"]])
    if abs(2 * maturity - round(2 * maturity)) > 1e-9 or \
            abs(2 * first - round(2 * first)) > 1e-9:
        continue
    number = 1 + round(2 * (maturity - first))
    balance = 100.0
    discounted = []
    for i in range(round(2 * maturity) + 1):
        interest = 0 if i == 0 else balance * coupon / 200
        principal = 100 / number if i >= round(2 * first) else 0
        balance -= principal
        discounted.append((interest + principal) / (1 + rate / 100) ** (i / 2))
    assert abs(balance) < 1e-8
    errors.append(abs(100 - math.fsum(discounted) - float(r["ge_market_pct"])))
assert len(errors) >= 800 and max(errors) < 1e-9
print(f"ADD central non-peer sample: {len(sample)}; directly summed schedules: "
      f"{len(errors)}; maximum discrepancy: {max(errors):.3g} percentage points")
for label, column in [("fixed5", "delta_fixed5_ge_pp"),
                      ("modern_DAC", "delta_policy_ge_pp")]:
    full = [r for r in sample if r[column] and
            (label == "fixed5" or int(r["commitment_year"]) >= 2018)]
    quality = [r for r in full if r["zero_rate_uncertain"] != "TRUE"]
    print(label, "main", len(full), statistics.mean(float(r[column]) for r in full),
          "excluding uncertain zeros", len(quality),
          statistics.mean(float(r[column]) for r in quality))
