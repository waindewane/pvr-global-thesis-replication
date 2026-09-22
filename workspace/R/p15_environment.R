p15_environment_audit <- function() {
  lock <- jsonlite::fromJSON("renv.lock",simplifyVector=FALSE)
  rows <- lapply(names(lock$Packages),function(p) {
    v <- if(requireNamespace(p,quietly=TRUE)) as.character(utils::packageVersion(p)) else NA_character_
    data.frame(component=p,required=lock$Packages[[p]]$Version,actual=v,
      matches=!is.na(v)&&isTRUE(package_version(v)==package_version(lock$Packages[[p]]$Version)))
  })
  rows <- c(list(data.frame(component="R",required=lock$R$Version,actual=as.character(getRversion()),
    matches=isTRUE(getRversion()==package_version(lock$R$Version)))),rows)
  python <- Sys.which("python3")
  if(!nzchar(python))stop("Python3 is required for preserved rating workbooks")
  versions <- system2(python,c("-c",shQuote("import sys,openpyxl,et_xmlfile; print(sys.version.split()[0]); print(openpyxl.__version__); print(et_xmlfile.__version__)")),stdout=TRUE,stderr=FALSE)
  if(length(versions)!=3L)stop("Python dependency check failed; see requirements-p15.txt")
  rows <- c(rows,list(data.frame(component=c("Python","openpyxl","et_xmlfile"),required=c(">=3.9","3.1.5","2.0.0"),actual=versions,
    matches=c(package_version(versions[1])>=package_version("3.9"),versions[2]=="3.1.5",versions[3]=="2.0.0"))))
  result <- do.call(rbind,rows)
  if(any(!result$matches))stop("Environment differs from the lock: ",paste(result$component[!result$matches],collapse=", "),". Restore before building.")
  result
}
