# Active analyses resolve current versioned inputs; historical builders keep their
# explicit snapshots. Master-provided environment routes always take precedence.
p15_current_input <- function(stage = "dataset", pointer = "data-derived/p15_master/current_run.json") {
  if (!file.exists(pointer)) stop("Current research pointer is unavailable: ", pointer)
  current <- jsonlite::fromJSON(pointer)
  path <- if (stage %in% c("dataset", "candidate")) current$candidate else current$stages[[stage]]$dir
  if (is.null(path) || !length(path) || !dir.exists(path))
    stop("Current analysis input is unavailable: ", stage, ". Run the master workflow.")
  path
}
