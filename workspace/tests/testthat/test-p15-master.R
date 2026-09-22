source(file.path("..","..","R","p15_master.R"))
source(file.path("..","..","R","p15_master_findings.R"))

testthat::test_that("additional stages use registered references and current derived parents",{
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  cache<-file.path(d,"cache");dir.create(cache)
  stage<-file.path(cache,"existing_parent");dir.create(stage)
  parent<-file.path(stage,"rows.csv");writeLines(c("rate","5"),parent)
  external<-file.path(d,"source.csv");writeLines(c("weight","2"),external)
  design<-file.path(d,"design.md");writeLines("Fixed estimand",design)
  ref<-file.path(d,"new_reference");dir.create(ref)
  data.table::fwrite(data.frame(path=c(normalizePath(parent),external,design)),
    file.path(ref,"input_manifest.csv"))
  config<-list(reference_stages=list(new_analysis=ref),cache_root=cache,
    reference_candidate=file.path(d,"candidate"),reference_research=file.path(d,"research"),
    reference_dac=file.path(d,"dac"))
  testthat::expect_identical(p15_master_reference(config,"new_analysis"),ref)
  testthat::expect_setequal(p15_master_external(config,"new_analysis"),c(external,design))
  # A similarly named source outside the parent tree must remain a dependency.
  other<-paste0(cache,"_source.csv");writeLines(c("rate","8"),other)
  data.table::fwrite(data.frame(path=c(parent,other)),file.path(ref,"input_manifest.csv"))
  testthat::expect_identical(p15_master_external(config,"new_analysis"),other)
})

testthat::test_that("fingerprints propagate only declared input and parent changes",{
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  a<-file.path(d,"source");b<-file.path(d,"other");note<-file.path(d,"note")
  writeLines("1",a);writeLines("2",b);writeLines("prose",note)
  k<-p15_master_key(a);other<-p15_master_key(b)
  child<-p15_master_key(b,list(parent=k))
  writeLines("new prose",note)
  testthat::expect_identical(p15_master_key(a),k)
  writeLines("3",a);next_key<-p15_master_key(a)
  testthat::expect_false(identical(next_key,k))
  testthat::expect_false(identical(p15_master_key(b,list(parent=next_key)),child))
  testthat::expect_identical(p15_master_key(b),other)
})
testthat::test_that("cached outputs cannot be silently modified",{
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  input<-file.path(d,"input");writeLines("source",input)
  out<-file.path(d,"out");dir.create(out);f<-file.path(out,"table.csv")
  writeLines(c("rate","4"),f)
  p15_master_receipt(out,"test","key",input)
  testthat::expect_identical(p15_master_verify(file.path(out,"receipt.rds"))$key,"key")
  writeLines(c("rate","5"),f)
  testthat::expect_error(p15_master_verify(file.path(out,"receipt.rds")),"Cached output")
})
testthat::test_that("table changes retain rates and peer membership but ignore run metadata",{
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  a<-file.path(d,"a.csv");b<-file.path(d,"b.csv")
  data.table::fwrite(data.frame(build_id="old",rate=4,seed_ids="A;B;C"),a)
  data.table::fwrite(data.frame(build_id="new",rate=4,seed_ids="A;B;C"),b)
  testthat::expect_identical(p15_master_csv_signature(a),p15_master_csv_signature(b))
  data.table::fwrite(data.frame(build_id="new",rate=4,seed_ids="A;B;D"),b)
  testthat::expect_false(identical(p15_master_csv_signature(a),p15_master_csv_signature(b)))
  data.table::fwrite(data.frame(build_id="new",rate=5,seed_ids="A;B;C"),b)
  testthat::expect_false(identical(p15_master_csv_signature(a),p15_master_csv_signature(b)))
})
testthat::test_that("a no-change refresh cannot acknowledge unresolved notes",{
  registry<-data.table::data.table(analysis="regional",note="overview.md",status="live")
  receipt<-data.table::data.table(analysis="regional",note="overview.md",reviewed_fingerprint="old")
  results<-data.table::data.table(analysis="regional",result_fingerprint="new")
  a<-p15_master_note_alerts(registry,receipt,results)
  b<-p15_master_note_alerts(registry,receipt,results)
  testthat::expect_true(a$review_required)
  testthat::expect_true(b$review_required)
  receipt$reviewed_fingerprint<-"new"
  testthat::expect_false(p15_master_note_alerts(registry,receipt,results)$review_required)
})

testthat::test_that("compound provenance ignores cache paths but preserves row identities",{
  d<-tempfile();dir.create(d);on.exit(unlink(d,recursive=TRUE))
  a<-file.path(d,"old.csv");b<-file.path(d,"new.csv")
  data.table::fwrite(data.frame(n=2,rate=4,lineage_parent_ids="data-derived/old/rows.csv#PAK::2022;P15-PEER::PAK",
    parent_pair_locator="/project/old/pairs.csv#PAK::2022::China__IDA"),a)
  data.table::fwrite(data.frame(n=2,rate=4,lineage_parent_ids="/project/new/rows.csv#PAK::2022;P15-PEER::PAK",
    parent_pair_locator="/project/new/pairs.csv#PAK::2022::China__IDA"),b)
  testthat::expect_identical(p15_master_csv_signature(a),p15_master_csv_signature(b))
  x<-data.table::fread(b);x$parent_pair_locator<-"/project/new/pairs.csv#PAK::2023::China__IDA"
  data.table::fwrite(x,b)
  testthat::expect_false(identical(p15_master_csv_signature(a),p15_master_csv_signature(b)))
})

testthat::test_that("downstream inference code cannot trigger raw archive copying",{
  spec<-list(leaves=c("sources/raw.csv","R/p15_raw_foundations.R","R/p15_thesis_benchmark_inference.R"),
    outputs="data-derived/raw_reference.csv")
  testthat::expect_setequal(p15_master_raw_files(spec,"config/source_snapshot.csv"),
    c("sources/raw.csv","R/p15_raw_foundations.R","data-derived/raw_reference.csv","config/source_snapshot.csv"))
})
