#!/usr/bin/env Rscript

download_dir <- file.path("data-raw", "oecd_crs", "2026-05-25")
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

downloads <- data.frame(
  file = c(
    "crs_readme_en_v20260408.txt",
    "crs_2024_dotstat_v20260408.zip",
    "dac_tables_crs_codebook_2025-08-19.xlsx",
    "dac_crs_codes_2026-05-07.xlsx",
    "de_user_guide_dac_crs_2026-04-24.pdf",
    "crs_dataflow_references_v1_6.json",
    "crs_grant_equivalent_dataflow_references_v1_6.json"
  ),
  url = c(
    "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=aa82b330-f4e5-4230-ba0c-fd3441bb80bb",
    "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=fad5450f-e2ef-4f2b-ad7a-f9d70c8f6124",
    "https://webfs.oecd.org/oda/DataCollection/Resources/DAC-tables-CRS-codebook.xlsx",
    "https://webfs.oecd.org/oda/DataCollection/Resources/DAC-CRS-CODES.xlsx",
    "https://webfs.oecd.org/oda/DataExplorer/DE_User_Guide_for_DAC-CRS_statistics.pdf",
    "https://sdmx.oecd.org/dcd-public/rest/v1/dataflow/OECD.DCD.FSD/DSD_CRS%40DF_CRS/1.6?references=all",
    "https://sdmx.oecd.org/dcd-public/rest/v1/dataflow/OECD.DCD.FSD/DSD_GREQ%40DF_CRS_GREQ/1.6?references=all"
  ),
  source = c(
    "OECD CRS related files",
    "OECD CRS related files",
    "OECD DataCollection resources",
    "OECD DataCollection resources",
    "OECD Data Explorer resources",
    "OECD SDMX dataflow metadata",
    "OECD SDMX dataflow metadata"
  ),
  stringsAsFactors = FALSE
)

download_with_curl <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message("Already present: ", dest)
    return(invisible(dest))
  }

  tmp <- paste0(dest, ".part")
  if (file.exists(tmp)) {
    unlink(tmp)
  }
  args <- c(
    "-L", "--fail", "--retry", "3",
    "--output", tmp, url
  )
  status <- system2("curl", args)
  if (!identical(status, 0L)) {
    stop("curl failed for ", url, call. = FALSE)
  }
  file.rename(tmp, dest)
  invisible(dest)
}

for (i in seq_len(nrow(downloads))) {
  download_with_curl(downloads$url[i], file.path(download_dir, downloads$file[i]))
}

bulk_downloads <- data.frame(
  file = c(
    "crs_reduced_parquet_v20260408.zip",
    "crs_parquet_v20260408.zip"
  ),
  url = c(
    "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=3ff88887-18dc-43c8-abfc-ae49fe52c577",
    "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=50f0355e-8f61-4230-85f3-90b4db45bfc9"
  ),
  source = "OECD CRS related files",
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(bulk_downloads))) {
  download_with_curl(bulk_downloads$url[i], file.path(download_dir, bulk_downloads$file[i]))
}

downloads <- rbind(downloads, bulk_downloads)

downloads$download_date <- as.character(Sys.Date())
downloads$local_path <- file.path(download_dir, downloads$file)
downloads$bytes <- file.info(downloads$local_path)$size

write.csv(
  downloads,
  file.path(download_dir, "source_manifest.csv"),
  row.names = FALSE,
  na = ""
)

message("Downloaded OECD CRS source files to: ", download_dir)
