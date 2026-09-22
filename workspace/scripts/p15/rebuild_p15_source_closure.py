"""Re-extract the bounded IDS evidence from immutable raw workbooks offline."""
import subprocess
import sys
from pathlib import Path

out = Path("data-derived/p15_source_closure_extract_20260907_v1")
if out.exists():
    raise RuntimeError("Raw rebuild requires an absent extraction directory")
for name in ("a_d", "e_k", "n_q", "r_u"):
    subprocess.run([sys.executable, "scripts/p15/extract_p15_source_closure.py",
                    name + ".xlsx", str(out / (name + ".csv"))], check=True)
