"""Stream preserved IDS workbooks for the 30 review keys; never overwrite inputs."""
import csv
import gzip
import hashlib
import sys
import zipfile
from pathlib import Path
from lxml import etree

NS = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"
filename, output = sys.argv[1:3]
path = Path("data-raw/p15_ids_terms_followup_20260906") / filename
out = Path(output)
if out.exists():
    raise RuntimeError("Refusing to overwrite " + str(out))
with gzip.open("data-derived/p15_reference_review_20260907_v1/p15_ids_secondary_cases.csv.gz", "rt") as f:
    keys = {(r["iso3"], r["analysis_year"]) for r in csv.DictReader(f)}
countries = {c for c, y in keys}
series = {"DT.MAT.DPPG", "DT.GPA.DPPG", "DT.INR.DPPG", "DT.COM.DPPG.CD"}

def value(cell, strings):
    v = cell.find(NS + "v")
    if cell.get("t") == "inlineStr":
        return "".join(cell.itertext())
    if v is None:
        return ""
    return strings[int(v.text)] if cell.get("t") == "s" else v.text

rows = []
with zipfile.ZipFile(path) as z:
    strings = ["".join(t.itertext()) for t in etree.fromstring(z.read("xl/sharedStrings.xml"))]
    workbook = etree.fromstring(z.read("xl/workbook.xml"))
    sheet = workbook.find(NS + "sheets")[0].get("name")
    with z.open("xl/worksheets/sheet1.xml") as f:
        for _, row in etree.iterparse(f, events=("end",), tag=NS + "row"):
            cells = row.findall(NS + "c")
            if row.get("r") == "1":
                header = {c.get("r").rstrip("0123456789"): value(c, strings) for c in cells}
                assert [header[k] for k in "ABCDEF"] == ["Country Code", "Country Name",
                    "Counterpart-Area Name", "Counterpart-Area Code", "Series Name", "Series Code"]
            else:
                first = {c.get("r").rstrip("0123456789"): value(c, strings).strip() for c in cells[:6]}
                if first.get("A") in countries and first.get("D") == "BND" and first.get("F") in series:
                    assert first["C"] == "Bondholders"
                    bycol = {c.get("r").rstrip("0123456789"): c for c in cells}
                    for col, year in header.items():
                        if (first["A"], year) in keys:
                            cell = bycol.get(col)
                            rows.append(dict(iso3=first["A"], analysis_year=year,
                                series=first["F"], counterpart="BND",
                                value="" if cell is None else value(cell, strings),
                                source_file=str(path), sheet=sheet, cell=col+row.get("r"),
                                source_snapshot_id="SRC-P15-IDS-XLSX-"+path.stem.upper()+"-20260906"))
            row.clear()
            while row.getprevious() is not None:
                del row.getparent()[0]
assert rows
out.parent.mkdir(parents=True, exist_ok=True)
with out.open("w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)
print(filename, len(rows), "source cells extracted", flush=True)
