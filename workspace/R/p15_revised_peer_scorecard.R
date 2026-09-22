# Conditional quantitative adaptation; analyst judgments are never certified.
# Exact numerical tables: PBC_1151027 (27 November 2018), pp4–6,9,14,18.
# World Bank WPS9649 pp5–6/21 supplies seven-year windows and event risk M.
p15_peer_strength_labels <- c('VH+','VH','VH-','H+','H','H-','M+','M','M-','L+','L','L-','VL+','VL','VL-')
p15_peer_rating_labels <- c('Aaa','Aa1','Aa2','Aa3','A1','A2','A3','Baa1','Baa2','Baa3','Ba1','Ba2','Ba3','B1','B2','B3','Caa1','Caa2','Caa3','Ca','C')
p15_peer_midpoints <- c(92.5,82.5,77.5,72.5,67.5,62.5,57.5,52.5,47.5,42.5,37.5,32.5,27.5,22.5,10.5)
# Every cutoff includes its lower endpoint. Indicator strength order: 1 best.
p15_peer_low_good <- function(x,cuts) {z<-findInterval(x,cuts)+1L;z[!is.finite(x)]<-NA_integer_;z}
p15_peer_high_good <- function(x,cuts) {z<-15L-findInterval(x,sort(cuts));z[!is.finite(x)]<-NA_integer_;z}
p15_peer_factor_index <- function(z) p15_peer_high_good(z,c(20,25,30,35,40,45,50,55,60,65,70,75,80,85))
p15_peer_inflation_index <- function(x) {
 z<-rep(NA_integer_,length(x));ok<-is.finite(x)
 z[ok & x>=1.3 & x<2.5]<-1L
 lo<-c(1.2,1.1,1,.9,.8,.7,.6,.5,.4,.3,.2,.1,0)
 hi<-c(2.5,3,3.5,4,5,6,8,10,12.5,15,17.5,20,22.5)
 for(j in 1:13){z[ok & x>=lo[j] & x<if(j==1)1.3 else lo[j-1]]<-j+1L
   z[ok & x>=hi[j] & x<if(j==13)25 else hi[j+1]]<-j+1L}
 z[ok & (x<0|x>=25)]<-15L;z
}
p15_peer_cutoffs <- list(
 growth=list(direction='high',cuts=c(4.5,4,3.5,3,2.75,2.5,2.25,2,1.75,1.5,1.25,1,.75,.5)),
 growth_sd=list(direction='low',cuts=c(1.44,1.66,1.76,1.96,2.11,2.20,2.29,2.49,2.64,2.85,3.14,3.36,3.72,3.95)),
 gci=list(direction='high',cuts=c(4.98,4.61,4.52,4.45,4.39,4.31,4.26,4.22,4.1,4.03,3.95,3.9,3.84,3.75)),
 gdp=list(direction='high',cuts=c(1000,500,400,300,250,200,175,150,125,100,75,50,25,10)),
 gdppc=list(direction='high',cuts=c(35175,30130,25918,24045,20402,18001,16297,13587,11863,10656,8577,7708,5919,4320)),
 inflation_sd=list(direction='low',cuts=c(1.2,1.4,1.7,2,2.1,2.5,2.6,2.7,3.1,3.4,3.6,3.8,4.5,5.6)),
 debt_gdp=list(direction='low',cuts=c(30,35,40,45,50,55,60,65,70,80,90,100,120,140)),
 debt_revenue=list(direction='low',cuts=c(120,140,160,180,200,220,240,260,280,320,360,400,480,560)),
 interest_gdp=list(direction='low',cuts=c(1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,4,4.5,5,6,7)),
 interest_revenue=list(direction='low',cuts=c(6,7,8,9,10,11,12,13,14,16,18,20,24,28)),
 ge=list(direction='high',cuts=c(1.14,1.01,.85,.48,.34,.25,.11,-.01,-.10,-.17,-.35,-.41,-.50,-.72)),
 rl=list(direction='high',cuts=c(.98,.81,.64,.48,.26,.06,-.08,-.15,-.29,-.35,-.45,-.57,-.71,-.82)),
 cc=list(direction='high',cuts=c(1.03,.82,.56,.32,.13,-.06,-.19,-.29,-.39,-.44,-.58,-.64,-.79,-.91)))
p15_peer_indicator <- function(x,name){c<-p15_peer_cutoffs[[name]];if(c$direction=='high')p15_peer_high_good(x,c$cuts) else p15_peer_low_good(x,c$cuts)}
p15_peer_weighted_factor <- function(indices,weights) {
 m<-matrix(p15_peer_midpoints[as.matrix(indices)],nrow=nrow(indices));w<-as.matrix(weights)
 stopifnot(identical(dim(m),dim(w)),all(abs(rowSums(w)-1)<1e-10),all(w>=0))
 m[w==0]<-0;p15_peer_factor_index(rowSums(m*w))
}
p15_peer_read_exact_matrices <- function(paths) {
 lapply(setNames(c('economic_resiliency','government_financial_strength','rating_midpoint'),
                c('economic_resiliency','government_financial_strength','rating_midpoint')),function(nm){
  d<-read.csv(paths[[nm]],check.names=FALSE);m<-as.matrix(d[-1]);rownames(m)<-d[[1]]
  stopifnot(nrow(m)==15,ncol(m)==15,identical(colnames(m),p15_peer_strength_labels));m})
}
p15_peer_aggregate_factors <- function(f1,f2,f3,matrices) {
 n<-max(length(f1),length(f2),length(f3));f1<-rep_len(f1,n);f2<-rep_len(f2,n);f3<-rep_len(f3,n)
 er<-matrices$economic_resiliency[cbind(p15_peer_strength_labels[f2],p15_peer_strength_labels[f1])]
 gfs<-matrices$government_financial_strength[cbind(er,p15_peer_strength_labels[f3])]
 match(matrices$rating_midpoint[cbind(rep('M',n),gfs)],p15_peer_rating_labels)
}


p15_revised_peer_config <- function(config_path = "config/p15_revised_peer_20260912_v1.json") {
  config <- jsonlite::fromJSON(config_path, simplifyVector = TRUE)
  required <- c("macro_features", "governance_features", "historical_eligibility",
    "economic_resiliency", "government_financial_strength", "rating_midpoint")
  if (!all(required %in% config$sources$role) || anyDuplicated(config$sources$role))
    stop("Peer source roles are missing or duplicated")
  if (!identical(as.character(config$donor_priority), c("primary", "ids", "secondary", "moodys")) ||
      config$minimum_donors != 3L || config$caliper != 3L || !identical(as.integer(config$rules), 1:5) ||
      config$distance != "guaranteed_interval") stop("Config changes the accepted peer method")
  hashes <- vapply(config$sources$path, digest::digest, character(1), file = TRUE, algo = "sha256")
  if (!identical(unname(hashes), unname(config$sources$sha256))) stop("Peer source snapshot hash mismatch")
  config$paths <- setNames(config$sources$path, config$sources$role)
  config$config_path <- config_path
  config
}

p15_revised_peer_input_files <- function(config_path = "config/p15_revised_peer_20260912_v1.json") {
  config <- p15_revised_peer_config(config_path)
  unique(c(config_path, config$snapshot_manifest, config$sources$path))
}

# Reconstruct only the source features needed by the accepted conservative interval.
# Bracketed FC/default adjustments do not require point estimates of those inputs.
p15_revised_peer_score_inputs <- function(config, panel_keys) {
  stopifnot(!anyDuplicated(as.data.frame(panel_keys)[c("iso3", "analysis_year")]))
  cols <- c("iso3", "analysis_year", "reference_year", "growth_avg7", "growth_sd10",
    "gci_carry_max2", "nominal_gdp_usd_bn", "gdp_pc_ppp", "inflation_avg7", "inflation_sd10",
    "debt_gdp", "debt_revenue", "debt_trend_pp", "gfs_gross_interest_gdp_latest",
    "gfs_gross_interest_revenue_latest")
  macro <- data.table::fread(config$paths[["macro_features"]], select = cols)
  governance <- data.table::fread(config$paths[["governance_features"]],
    select = c("iso3", "analysis_year", "government_effectiveness", "rule_law", "corruption"))
  eligibility <- data.table::fread(config$paths[["historical_eligibility"]],
    select = c("iso3", "analysis_year", "ida", "hipc", "concessional_weight_exception"))
  for (z in list(macro, governance, eligibility))
    stopifnot(!anyDuplicated(z[, .(iso3, analysis_year)]))
  u <- data.table::as.data.table(data.table::copy(panel_keys))[, .(iso3, analysis_year)]
  for (z in list(macro, governance, eligibility))
    u <- merge(u, z, by = c("iso3", "analysis_year"), all.x = TRUE, sort = FALSE)
  # Unknown concessional classification must not silently acquire ordinary fiscal weights.
  known_weights <- !is.na(u$concessional_weight_exception)
  gross <- is.finite(u$gfs_gross_interest_gdp_latest) & is.finite(u$gfs_gross_interest_revenue_latest)
  u[, `:=`(interest_gdp = ifelse(gross, gfs_gross_interest_gdp_latest, NA_real_),
    interest_revenue = ifelse(gross, gfs_gross_interest_revenue_latest, NA_real_),
    interest_source = ifelse(gross, "GFS_general_government_revised_gross", "gross_interest_unknown_bounded"),
    debt_trend_penalty = p15_peer_low_good(debt_trend_pp, c(10, 20, 30)) - 1L)]
  u[interest_gdp < 0 | interest_revenue < 0, `:=`(interest_gdp = NA_real_, interest_revenue = NA_real_)]
  u[!known_weights, debt_gdp := NA_real_]
  data.table::setorder(u, iso3, analysis_year)
  u
}

p15_revised_peer_bounded_scores <- function(u, matrices, conservative_interest=TRUE) {
  n <- nrow(u)
  ei <- data.frame(growth=p15_peer_indicator(u$growth_avg7,'growth'),
    growth_sd=p15_peer_indicator(u$growth_sd10,'growth_sd'),gci=p15_peer_indicator(u$gci_carry_max2,'gci'),
    gdp=p15_peer_indicator(u$nominal_gdp_usd_bn,'gdp'),gdppc=p15_peer_indicator(u$gdp_pc_ppp,'gdppc'))
  ew <- matrix(rep(c(.25,.125,.125,.25,.25),each=n),ncol=5)
  elo <- ehi <- ei
  elo$gci[is.na(elo$gci)] <- 1L; ehi$gci[is.na(ehi$gci)] <- 15L
  f1lo <- p15_peer_weighted_factor(elo,ew); f1hi <- p15_peer_weighted_factor(ehi,ew)
  inst <- data.frame(ge=p15_peer_indicator(u$government_effectiveness,'ge'),
    rl=p15_peer_indicator(u$rule_law,'rl'),cc=p15_peer_indicator(u$corruption,'cc'),
    level=p15_peer_inflation_index(u$inflation_avg7),vol=p15_peer_indicator(u$inflation_sd10,'inflation_sd'))
  iw <- matrix(rep(c(.375,.1875,.1875,.125,.125),each=n),ncol=5)
  f2lo <- p15_peer_weighted_factor(inst,iw); f2hi <- pmin(15L,f2lo+3L)
  fi <- data.frame(debt_gdp=p15_peer_indicator(u$debt_gdp,'debt_gdp'),
    debt_revenue=p15_peer_indicator(u$debt_revenue,'debt_revenue'),
    interest_gdp=p15_peer_indicator(u$interest_gdp,'interest_gdp'),
    interest_revenue=p15_peer_indicator(u$interest_revenue,'interest_revenue'))
  if(conservative_interest) {
    gross <- u$interest_source %in% 'GFS_general_government_revised_gross'
    fi$interest_gdp[!gross] <- NA_integer_; fi$interest_revenue[!gross] <- NA_integer_
  }
  fw <- matrix(.25,n,4)
  exception <- u$concessional_weight_exception %in% TRUE
  fw[exception,] <- matrix(rep(c(.5,.5,0,0),each=sum(exception)),ncol=4)
  reserve <- u$iso3 %in% c('JPN','CHE','GBR','USA','DEU','FRA')
  fw[reserve,] <- matrix(rep(c(.05,.05,.45,.45),each=sum(reserve)),ncol=4)
  flo <- fhi <- fi
  for(k in c('interest_gdp','interest_revenue')) {
    flo[[k]][is.na(flo[[k]])] <- 1L; fhi[[k]][is.na(fhi[[k]])] <- 15L
  }
  f3lo <- pmin(15L,p15_peer_weighted_factor(flo,fw)+u$debt_trend_penalty)
  f3hi <- pmin(15L,p15_peer_weighted_factor(fhi,fw)+pmin(6L,u$debt_trend_penalty+ifelse(u$debt_gdp<25,3L,6L)))
  lower <- p15_peer_aggregate_factors(f1lo,f2lo,f3lo,matrices)
  upper <- p15_peer_aggregate_factors(f1hi,f2hi,f3hi,matrices)
  data.table::data.table(iso3=u$iso3,analysis_year=u$analysis_year,scenario='wgi_bounded',
    shadow_notch=(lower+upper)/2,shadow_notch_lower=lower,shadow_notch_upper=upper,
    scorecard_complete=FALSE,status=if(conservative_interest)
      'conditional_score_interval_gross_interest_uncertainty' else 'conditional_score_interval_v1_net_proxy')
}

p15_revised_peer_dependency_paths <- function(config_path = "config/p15_revised_peer_20260912_v1.json") {
  unique(c(p15_revised_peer_input_files(config_path),
    "R/p15_revised_peer_scorecard.R", "R/p15_revised_peer_matching.R", "R/p15_revised_peer_candidate.R",
    "scripts/p15/build_p15_revised_peer_candidate.R", "R/research_governance.R",
    "R/p15_local_completion.R", "R/p15_ladder_preview.R", "R/p15_reference_review.R",
    "R/p15_analysis_dataset.R", "scripts/p15/activate_p15_environment.R", "renv.lock"))
}
