"""Read-only extraction from preserved official XLSX/ZIP exports; no data repair.

Use bundled Python. XML streaming avoids loading the very large bulk worksheets
into memory. Source cell coordinates and full original archives remain available.
Economic comparisons are made separately in R.
"""
import csv
import hashlib
import io
import sys
import zipfile
from pathlib import Path
from lxml import etree

ROOT = Path("data-raw/p15_ids_terms_followup_20260906")
NS = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"
COUNTRIES = {"CHN", "IDN", "KAZ", "PER", "RWA", "TUR"}
SERIES = {"DT.MAT.DPPG", "DT.GPA.DPPG", "DT.INR.DPPG", "DT.COM.DPPG.CD"}


def value(cell, strings):
    v = cell.find(NS + "v")
    if cell.get("t") == "inlineStr":
        return "".join(cell.itertext())
    if v is None:
        return ""
    return strings[int(v.text)] if cell.get("t") == "s" else v.text


def extract(filename):
    path = ROOT / filename
    with zipfile.ZipFile(path) as z:
        strings = ["".join(t.itertext()) for t in
                   etree.fromstring(z.read("xl/sharedStrings.xml"))]
        workbook = etree.fromstring(z.read("xl/workbook.xml"))
        sheet_name = workbook.find(NS + "sheets")[0].get("name")
        rows, header, seen = [], None, set()
        with z.open("xl/worksheets/sheet1.xml") as f:
            for _, row in etree.iterparse(f, events=("end",), tag=NS + "row"):
                cells = row.findall(NS + "c")
                if row.get("r") == "1":
                    header = {c.get("r").rstrip("0123456789"): value(c, strings)
                              for c in cells}
                    assert [header[k] for k in "ABCDEF"] == [
                        "Country Code", "Country Name", "Counterpart-Area Name",
                        "Counterpart-Area Code", "Series Name", "Series Code"]
                else:
                    first = {c.get("r").rstrip("0123456789"): value(c, strings).strip()
                             for c in cells[:6]}
                    if (first.get("A") in COUNTRIES and first.get("D") == "BND"
                            and first.get("F") in SERIES):
                        assert first["C"] == "Bondholders", first
                        key = (first["A"], first["D"], first["F"])
                        assert key not in seen, key
                        seen.add(key)
                        by_col = {c.get("r").rstrip("0123456789"): c for c in cells}
                        for col, year in header.items():
                            if year.isdigit() and 2012 <= int(year) <= 2024:
                                cell = by_col.get(col)
                                rows.append(dict(iso3=first["A"], country=first["B"],
                                    counterpart="BND", series=first["F"], year=year,
                                    value="" if cell is None else value(cell, strings),
                                    source_file=str(path), sheet=sheet_name,
                                    cell=col + row.get("r"),
                                    source_snapshot_id="SRC-P15-IDS-XLSX-" + path.stem.upper() + "-20260906"))
                row.clear()
                while row.getprevious() is not None:
                    del row.getparent()[0]
        assert rows, filename
    out = ROOT / (path.stem + "_bnd_extracted.csv")
    with out.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0])); w.writeheader(); w.writerows(rows)
    print(filename, len(rows), "observations;", len(seen), "unique series", flush=True)


def metadata():
    z = zipfile.ZipFile(ROOT / "ids_csv_long.zip")
    matches = []
    for name in ["IDS_FootNoteMetaData.csv", "Country-Series - Metadata.csv",
                 "IDS_CountryMetaData.csv"]:
        with z.open(name) as f:
            # The provider's metadata uses Windows-1252 (e.g. Cote d'Ivoire).
            # Decode explicitly, never discard undecodable source characters.
            for row in csv.DictReader(io.TextIOWrapper(f, encoding="cp1252")):
                if any("(" + c + ")" in row.get("Country Code", "") for c in COUNTRIES):
                    matches.append(dict(member=name, **row))
    fields = sorted(set().union(*(r.keys() for r in matches)))
    with (ROOT / "six_country_bulk_metadata.csv").open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields); w.writeheader(); w.writerows(matches)
    print("Metadata rows", len(matches))


if __name__ == "__main__":
    if sys.argv[1] == "metadata":
        metadata()
    else:
        extract(sys.argv[1])
