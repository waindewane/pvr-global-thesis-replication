source('scripts/p15/activate_p15_environment.R')
files<-c(
 'dataset_construction/peer_plan_refresh_20260920/build_display_tables.R',
 'dataset_construction/illustration_refresh_20260920/verify_example.R',
 'benchmark_evaluation/evaluation_design_20260920/verify_metrics.R',
 'benchmark_evaluation/coverage_20260920/verify_and_prepare.R',
 'benchmark_evaluation/coverage_prose_20260920/build_coverage_figure.R',
 'benchmark_evaluation/accuracy_20260920/prepare_evidence.R',
 'benchmark_evaluation/source_comparability_20260920/prepare_evidence.R',
 'benchmark_evaluation/source_comparability_prose_20260920/prepare_median_comparison.R',
 'benchmark_evaluation/applicability_20260920/prepare_evidence.R',
 'benchmark_evaluation/temporal_use_20260921/prepare_evidence.R',
 'borrowing_conditions/content_5_1_5_2_20260921/verify_content.R',
 'borrowing_conditions/prose_5_1_5_2_20260921/build_figures.R',
 'borrowing_conditions/content_5_3_20260921/verify_content.R',
 'official_loan_data_and_valuation/content_20260921/verify_evidence.R',
 'concessionality_results/prose_20260922/prepare_and_check.R',
 'discussion_and_conclusion/prose_20260922/verify_evidence.R')
for(f in files){
 cat('DISPLAY:',f,'\n');p<-file.path('docs/thesis_design/sections',f)
 status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(p)))
 if(status!=0L)stop('Display preparation failed: ',f)
}
