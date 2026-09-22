# SCRIPT for estimating SIOP for rating data
# Output used for: Tables 6-7; Appendix tables B.1 and B.2

# last changed: 9/14/2021
#
# author: mei, gsz


###############################
# Read data; load necessary codes and packages
rm(list=ls())
wd <- getwd()

load(paste(wd,"/rating_data_calc.RData",sep=""))

varlist <- c(ls(),"varlist")

data.orig <- data.m
rm(list=varlist)
wd <- getwd()
source(paste(wd,"/siop.R",sep=""))
source(paste(wd,"/grad_oprob_bounded.R",sep=""))
source(paste(wd,"/hess_oprob_bounded.R",sep=""))
source(paste(wd,"/LL_oprob_bounded.R",sep=""))
source(paste(wd,"/LL_oprob_bounded.probs.R",sep=""))
source(paste(wd,"/LL_ziopc_bounded.R",sep=""))
source(paste(wd,"/loadpartial.R",sep=""))
source(paste(wd,"/fun_collection.R",sep=""))
source(paste(wd,"/select_data.R",sep=""))
source(paste(wd,"/select_mean_sd.R",sep=""))
source(paste(wd,"/make_result_table.R",sep=""))

packages <- c("optimx","MASS","Matrix","miscTools","pbivnorm","numDeriv")
for (p in packages){
  check = require(p,character.only=TRUE)
  if (!check) {
    install.packages(p)
    require(p,character.only=TRUE)
  }  
}

###############################
# Data identifiers; rating agencies and agency-based variables
crossid <- "Country"
dateid <- "Date"       
agencies <- c("Moody","SP","Fitch")
cols.agency <- c("Rating.first","Rating.last","Rating.changes.pos","Rating.changes.neg",
                 "Outlook.first","Outlook.last","Announcement","Timesince")
cols.other.bin <- c("Rating.changes.pos","Rating.changes.neg","Announcement")
cols.other.meandiff <- c("Rating.first")
cols.other.posneg <- c("Outlook.first")

###############################
# Set "default" parameters
lag_setting = 12        # number of lags for lag modification
alpha_setting = 0.99    # windsorizing
alpha_2 <- c(1-alpha_setting,alpha_setting)
realcol <- "cpi_IMF"
b_fundchange <- 1
conf.ag <- "hess"
conf.pool <- "hess"

###############################
# Choice of pooled and unpooled variables
colfund <- c("gnipc","ip","inf","reer","yield","debt","fiscbal","current","reserves","corrupt","ka_open") #"volGDP",
col.pool <- c("rating.diff.pos","rating.diff.neg","Up12.other","Down12.other",
              "ip","inf","reer","debt","fiscbal","reserves","current","corrupt","ka_open","default","changefund")
b.constant.pool <- FALSE

load.pool <- c("Yoprob","Yprob","Ylevel","posc.low.prob","posc.high.prob","posc.low.oprob","posc.high.oprob","X","Z","Nobs","res","pos","adj_mat")
changefund.placement <- "jointXZ"

###############################
# list of elements to be saved from every specification
save.elems <- c("agencies","crossid","dateid","lag_setting","alpha_setting","alpha_2","realcol","interactcol","interactcol2","savename","b_fundchange","colfund",
                "additions","addX","addZ","addXZ","baseX","baseZ","baseXZ","pos","corr","adj_mat","b.constant.pool","conf",
                "data.orig","Yoprob","Yprob","Ylevel","posc.low.prob","posc.high.prob","posc.low.oprob","posc.high.oprob","X","Z","Nobs","res","ptm","restable","changefund.placement",
                "p.poolingtest")

###############################
# list of specifications to be run in the for-loop below
# These contain the baseline reported in the paper as well as all robustness checks reported in the appendix
spec.list <- list(list(savename="baseline",additions=c("baseadd","dynamics","spillover","competitor","ratingsq"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="baseline_corr",additions=c("baseadd","dynamics","spillover","competitor","ratingsq"),corr=TRUE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="NoRatingsq",additions=c("baseadd","dynamics","spillover","competitor"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="NoDynamics",additions=c("baseadd","spillover","competitor"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="NoSpilloverCompetitor",additions=c("baseadd","dynamics","ratingsq"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="NoCompetitors",additions=c("baseadd","dynamics","spillover","ratingsq"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="NoSpillovers",additions=c("baseadd","dynamics","competitor","ratingsq"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="RobustDefHist",additions=c("dynamics","spillover","competitor","ratingsq","RobustDefHist"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="outlook",additions=c("baseadd","dynamics","spillover","competitor","ratingsq","outlook"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="baseline.outlook",additions=c("baseadd","dynamics","spillover","ratingsq","competitor"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="political_full",additions=c("baseadd","dynamics","spillover","competitor","ratingsq","Political_full"),corr=FALSE,b_fundchange=b_fundchange,interactcol="dumoecd"),
                  list(savename="Homebias_short",additions=c("baseadd","dynamics","spillover","competitor","ratingsq","Homebias_short"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="Homebias_long",additions=c("baseadd","dynamics","spillover","competitor","ratingsq","Homebias_short","Homebias_add"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="oecd",additions=c("baseadd","dynamics","spillover","competitor","ratingsq","oecd"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL),
                  list(savename="eu",additions=c("baseadd","dynamics","spillover","competitor","ratingsq","eu"),corr=FALSE,b_fundchange=b_fundchange,interactcol=NULL)
                  )

# choice which of the above specifications to run explicitly
runmodels <- 1

###############################
# Definition of variable transformations for all scenarios
# Transformations are performed by functions in "fun_collection.R"
# They are based on the syntax "OLD_NAME:FUNCTION:NEW_NAME". Additional qualifiers (normalize; factor; demean) can be added using "!QUALIFIER"

# Set baseline scenario used for all estimations
#   variables in probit (X) AND ordered probit (Z)
baseXZ = c("Timesince:keep:years!normalize=TRUE!factor=12!demean=FALSE",
           "GDP_capita:norm_country:gnipc",
           "ip.comb:calc_gr:ip",
           "cpi_IMF:calc_windsor:inf",
           "reer.comb:calc_windgr:reer!alpha=alpha_2",
           "ext.balance.comb:keep:current!normalize=TRUE!factor=100",
           "debt.comb:keep:debt!normalize=TRUE!factor=100",
           "fiscal.balance.comb:keep:fiscbal!normalize=TRUE!factor=100",
           "Reserves:calc_windgr:reserves",
           "ryields.rEMBI:calc_windsor:yield",
           "corruption_TI::corrupt",
           "ka_open::ka_open")
baseaddXZ = c("DummyDefPast::default")

#   variables only in probit
baseX = NULL
baseaddX = NULL

#   variables only in ordered probit
baseZ = NULL
baseaddZ =NULL

# Robustness check with squared ratings
ratingsqXZ = c("rating:square:rating.sq")
ratingsqX = NULL
ratingsqZ = NULL

# Robustness check with rating dynamics
dynamicsXZ = c("Rating.first:keep:rating!normalize=TRUE!factor=24!demean=FALSE",
               "Rating.changes.pos:calc_cumsum:Up12!normalize=FALSE",
               "Rating.changes.neg:calc_cumsum:Down12!normalize=FALSE")
dynamicsX = NULL
dynamicsZ = NULL

# Robustness check with cross-country spillovers
spilloverXZ = c("Rating.changes.pos.total:calc_cumsum:UpAll12.tot!byCountry=FALSE!normalize=FALSE",
                "Rating.changes.neg.total:calc_cumsum:DownAll12.tot!byCountry=FALSE!normalize=FALSE")
spilloverX = NULL
spilloverZ = NULL


# Robustness check with competitor variables
competitorXZ = c("N.other::N.other",
                 "Rating.first.diff.other.pos:keep:rating.diff.pos!normalize=TRUE!factor=24",
                 "Rating.first.diff.other.neg:keep:rating.diff.neg!normalize=TRUE!factor=24",
                 "Rating.changes.pos.other:calc_cumsum:Up12.other!normalize=FALSE",
                 "Rating.changes.neg.other:calc_cumsum:Down12.other!normalize=FALSE")
competitorX = NULL
competitorZ = NULL

outlookXZ <- c("Outlook.pos::Outlook.pos",
               "Outlook.neg::Outlook.neg",
               "Outlook.first.other.pos::Outlook.other.pos",
               "Outlook.first.other.neg::Outlook.other.neg")
outlookX <- NULL
outlookZ <- NULL

RobustDefHistXZ <- c("DummyDef10::default")
RobustDefHistX <- NULL
RobustDefHistZ <- NULL

# OECD specification
oecdXZ <- c("oecd::dumoecd")
oecdX <- NULL
oecdZ <- NULL

# EU specification
euXZ <- c("EU::dumeu")
euX <- NULL
euZ <- NULL

# specification with political variables
Political_fullXZ = c("yrsoffc::yroffice",
                     "yrsoffc:square:yroffice.sq",
                     "maj",
                     "maj:square:maj.sq",
                     "exelec:calc_cumsum:exelecpost!lag=11!current=TRUE!normalize=FALSE",
                     "exelec:calc_cumsum:exelecpre!lag=-12!normalize=FALSE",
                     "legelec:calc_cumsum:legelecpost!normalize=FALSE",
                     "legelec:calc_cumsum:legelecpre!lag=-12!normalize=FALSE")
Political_fullX = NULL 
Political_fullZ = NULL

# specification with political variables
Political_execXZ = c("yrsoffc::yroffice",
                     "yrsoffc:square:yroffice.sq",
                     "maj",
                     "maj:square:maj.sq",
                     "exelec:calc_cumsum:exelecpost!lag=11!current=TRUE!normalize=FALSE",
                     "exelec:calc_cumsum:exelecpre!lag=-12!normalize=FALSE")

Political_execX = NULL 
Political_execZ = NULL


Political_legXZ = c("yrsoffc::yroffice",
                    "yrsoffc:square:yroffice.sq",
                    "maj",
                    "maj:square:maj.sq",
                    "legelec:calc_cumsum:legelecpost!normalize=FALSE",
                    "legelec:calc_cumsum:legelecpre!lag=-12!normalize=FALSE")
Political_legX = NULL 
Political_legZ = NULL


# robustness check with incumbent interactions
Political_ex_rincXZ = c("execright::rinc",
                        "rinc:interact:rinc.dumoecd!col2=interactcol",
                        "exelecpre:interact:rinc.X.exelecpre!col2=interactcol2",
                        "exelecpost:interact:rinc.X.exelecpost!col2=interactcol2")

Political_ex_rincX = NULL
Political_ex_rincZ = NULL

Political_leg_rincXZ = c("execright::rinc",
                         "rinc:interact:rinc.dumoecd!col2=interactcol",
                         "legelecpre:interact:rinc.X.legelecpre!col2=interactcol2",
                         "legelecpost:interact:rinc.X.legelecpost!col2=interactcol2")

Political_leg_rincX = NULL
Political_leg_rincZ = NULL

# robustness check with homebias
Homebias_shortXZ <- c("expshareL3::expshareL3",
                      "INLINEL3::INLINEL3",
                      "usmilaidshareL3::usmilaidshareL3",
                      "comlang::comlang",
                      "delf_language::delf_language",
                      "delf_ethnic::delf_ethnic")
Homebias_shortX <- NULL
Homebias_shortZ <- NULL

Homebias_addXZ <- c("bankexpL::bankexpL")
Homebias_addX <- NULL
Homebias_addZ <- NULL

# robustness check with homebias as factor
HomebiasFactor_shortXZ <- c("fac_homebias.short::fac_homebias.short")
HomebiasFactor_shortX <- NULL
HomebiasFactor_shortZ <- NULL

HomebiasFactor_longXZ <- c("fac_homebias.long::fac_homebias.long")
HomebiasFactor_longX <- NULL
HomebiasFactor_longZ <- NULL


##################################
##################################
##################################
# Start of the main program
#   For-loop over model specifications
for (k in runmodels){
  print(spec.list[[k]])
  
  # Retrieve model definitions
  additions <- spec.list[[k]]$additions
  corr <- spec.list[[k]]$corr
  interactcol <- spec.list[[k]]$interactcol
  interactcol2 <- spec.list[[k]]$interactcol2
  b_fundchange <- spec.list[[k]]$b_fundchange
  
  # Run the model four times: once for each agency, and once the (partially) pooled version
  # Reason: agency-specific coefficient estimates as starting values for the pooled version improve estimation efficiency
  for  (ag in c(agencies,"pool")){
    savename <- paste(spec.list[[k]]$savename,ag,sep=".")
    
    if (ag%in%agencies){
      # Construct data for agency-specific estimations
      
      data <- data.orig
      data[,cols.agency] <- data[,paste(cols.agency,ag,sep=".")]
      data[is.na(data[,"Outlook.first"]),"Outlook.first"] <- 0
      data[,"Outlook.pos"] <- 1*(data[,"Outlook.first"]>0)
      data[,"Outlook.neg"] <- 1*(data[,"Outlook.first"]<0)
      
      ag.other <- setdiff(agencies,ag)
      data[,"N.other"] <- rowSums(!is.na(data[,paste("Rating.first",ag.other,sep=".")]))
      for (cname in cols.other.bin){
        data[,cname] <- (data[,cname]>0)*1
        data[,paste(cname,"other",sep=".")] <- (rowSums(data[,paste(cname,ag.other,sep=".")],na.rm=TRUE)>0)*1
        data[,paste(cname,"total",sep=".")] <- (rowSums(data[,paste(cname,agencies,sep=".")],na.rm=TRUE)>0)*1
      }
      for (cname in cols.other.posneg){
        data[,paste(cname,"other.pos",sep=".")] <- (rowSums(data[,paste(cname,ag.other,sep=".")],na.rm=TRUE)>0)*1
        data[,paste(cname,"other.neg",sep=".")] <- (rowSums(data[,paste(cname,ag.other,sep=".")],na.rm=TRUE)<0)*1
      }
      for (cname in cols.other.meandiff){
        temp <- rowMeans(data[,paste(cname,ag.other,sep=".")],na.rm=TRUE)-data[,cname]
        temp[is.na(temp)] <- 0
        data[,paste(cname,"diff.other",sep=".")] <-  temp
        data[,paste(cname,"diff.other.pos",sep=".")] <- temp
        data[temp<0,paste(cname,"diff.other.pos",sep=".")] <- 0
        data[,paste(cname,"diff.other.neg",sep=".")] <- temp
        data[temp>0,paste(cname,"diff.other.neg",sep=".")] <- 0
      }
      addX <- NULL
      addZ <- NULL
      addXZ <- NULL
      
      # Define dependent variable
      if (savename==paste("baseline.outlook",ag,sep=".")){
        # Combine ratings and outlooks
        data[is.na(data[,"Outlook.last"]),"Outlook.last"] <- 0
        data[is.na(data[,"Outlook.first"]),"Outlook.first"] <- 0
        data[,"Rating.last"] <- data[,"Rating.last"] + data[,"Outlook.last"]
        data[,"Rating.first"] <- data[,"Rating.first"] + data[,"Outlook.first"]
      }
      Yprob <- data[,"Announcement"]
      Yoprob <- sign(data[,"Rating.last"]-data[,"Rating.first"])
      data[data[,"Rating.first"]<=2 & !is.na(data[,"Rating.first"]),"Rating.first"] <- 2
      Ylevel <- data[,"Rating.first"]
      
      # add all required specifications together
      if (length(additions)>0) {
        for (add in additions)  {
          addX = c(addX,get(paste(add,"X",sep="")))
          addZ = c(addZ,get(paste(add,"Z",sep="")))
          addXZ = c(addXZ,get(paste(add,"XZ",sep="")))
        }
      }
      
      jointXZ <- select_data(c(baseXZ,addXZ), data)
      onlyZ <- select_data(c(baseZ,addZ),cbind(data,jointXZ))
      onlyX <- select_data(c(baseX,addX),cbind(data,jointXZ,onlyZ))
      
      # Retrieve non-NA rows of the data
      pos <- which(rowSums(is.na(cbind(Yoprob,Yprob,jointXZ,onlyX,onlyZ,Ylevel)))==0)
      
      Nobs <- length(pos)
      jointXZ <- jointXZ[pos,]
      onlyX <- onlyX[pos,]
      onlyZ <- onlyZ[pos,]
      Yoprob = as.numeric(Yoprob[pos])
      Yprob = Yprob[pos]
      Ylevel <- Ylevel[pos]
      
      # Define threshold (positions) for latent variables based on observed rating (changes). Two examples:
      #   1) For an observation during an upgrade period, we are interested in the latent probit variable predicting a variable change, and the latent ordered probit variable predicting an upgrade
      #   2) For an observation with AAA-rating but without change, we are interested in the probit-probability of no rating change, and the joint oprob-probability of no change and an upgrade (see boundary adjustment in the paper)
      posc.low.prob <- Yprob + 1
      posc.high.prob <- Yprob + 2
      posc.low.oprob <- Yoprob + 2
      posc.low.oprob[(Ylevel==min(Ylevel)) & (Yoprob<1)] <- 1
      posc.high.oprob <- Yoprob + 3
      posc.high.oprob[(Ylevel==max(Ylevel)) & (Yoprob>-1)] <- 4
      
      # Add changefund as variable if required
      if (b_fundchange) {
        changefund <- as.matrix(calc_fundchange(cbind(jointXZ,onlyX,onlyZ),colfund,data[pos,crossid],Yprob,normalize=TRUE))
        colnames(changefund) <- "changefund"
        if (changefund.placement=="jointXZ"){
          jointXZ <- cbind(jointXZ,changefund)
        }else if (changefund.placement=="onlyX"){
          onlyX <- cbind(onlyX,changefund)
        }else if (changefund.placement=="onlyZ"){
          onlyZ <- cbind(onlyZ,changefund)
        }
      }
      X = cbind(jointXZ,onlyX)
      Z = cbind(jointXZ,onlyZ)
      
      # Calculating adj_mat for variable normalization (facilitating more efficient estimation)
      jointXZ.orig <- select_mean_sd(c(baseXZ,addXZ), data)
      onlyZ.orig <- select_mean_sd(c(baseZ,addZ),cbind(data,jointXZ.orig))
      onlyX.orig <- select_mean_sd(c(baseX,addX),cbind(data,jointXZ.orig,onlyZ.orig))
      jointXZ.orig <- jointXZ.orig[pos,]
      onlyZ.orig <- onlyZ.orig[pos,]
      onlyX.orig <- onlyX.orig[pos,]
      
      # Construction of full data
      FULL <- cbind(X,Z[,setdiff(colnames(Z),colnames(X))])
      FULL.orig <- cbind(jointXZ.orig,onlyX.orig,onlyZ.orig)[,intersect(colnames(FULL),colnames(cbind(jointXZ.orig,onlyX.orig,onlyZ.orig)))]
      if (b_fundchange){
        FULL.orig <- cbind(FULL.orig,FULL[,setdiff(colnames(FULL),colnames(FULL.orig))])
        colnames(FULL.orig)[colnames(FULL.orig)==""] <- setdiff(colnames(FULL),colnames(cbind(jointXZ.orig,onlyX.orig,onlyZ.orig)))
      }
      adj_mat <- NULL
      for (col.adj in colnames(FULL)){
        check <- FALSE
        while (!check){
          pos_ms <- sample(1:dim(X)[1],2)
          check <- !(diff(FULL[pos_ms,col.adj])==0)
        }
        y <- FULL[pos_ms,col.adj]
        x <- FULL.orig[pos_ms,col.adj]
        sd <- (x[1]-x[2])/(y[1]-y[2])
        mean <- x[1]-y[1]*sd
        adj_mat <- rbind(adj_mat,cbind(sd,mean))
      }
      rownames(adj_mat) <- colnames(FULL)
      adj_mat <- adj_mat[!grepl(".sq",rownames(adj_mat)) & !grepl(".X.",rownames(adj_mat)),]
      
      conf <- conf.ag
    }else{
      print("Estimating pooled model: drawing data from individual files")
      # Construct data from agency-specific estimations
      var.Moody <- loadpartial(paste(spec.list[[k]]$savename,"Moody.RData",sep="."),load.pool)
      var.SP <- loadpartial(paste(spec.list[[k]]$savename,"SP.RData",sep="."),load.pool)
      var.Fitch <- loadpartial(paste(spec.list[[k]]$savename,"Fitch.RData",sep="."),load.pool)
      
      # prior results for poolability tests
      if (any(is.null(var.Moody$res),is.null(var.SP$res),is.null(var.Fitch$res))){
        LL_ind <- -Inf
        Ncoeff_ind <- 0
      }else{
        LL_ind <- var.Moody$res$LL + var.SP$res$LL + var.Fitch$res$LL
        Ncoeff_ind <- length(c(var.Moody$res$opt_coeff,var.SP$res$opt_coeff,var.Fitch$res$opt_coeff))
      }
      
      
      Yoprob <- c(var.Moody$Yoprob,var.SP$Yoprob,var.Fitch$Yoprob)
      Yprob <- c(var.Moody$Yprob,var.SP$Yprob,var.Fitch$Yprob)
      Ylevel <- c(var.Moody$Ylevel,var.SP$Ylevel,var.Fitch$Ylevel)
      
      colpool.X <- intersect(colnames(var.Moody$X),col.pool)
      colpool.Z <- intersect(colnames(var.Moody$Z),col.pool)
      colind.X <- setdiff(colnames(var.Moody$X),colpool.X)
      colind.Z <- setdiff(colnames(var.Moody$Z),colpool.Z)
      
      X.pool <- rbind(var.Moody$X[,colpool.X],var.SP$X[,colpool.X],var.Fitch$X[,colpool.X])
      Z.pool <- rbind(var.Moody$Z[,colpool.Z],var.SP$Z[,colpool.Z],var.Fitch$Z[,colpool.Z])
      
      if (length(colind.X)>0){
        X.ind <- as.matrix(bdiag(var.Moody$X[,colind.X],var.SP$X[,colind.X],var.Fitch$X[,colind.X]))
        colnames(X.ind) <- c(colnames.add(colind.X,"Moody"),colnames.add(colind.X,"SP"),colnames.add(colind.X,"Fitch"))
      }else{
        X.ind <- NULL
      }
      if (length(colind.Z)>0){
        Z.ind <- as.matrix(bdiag(var.Moody$Z[,colind.Z],var.SP$Z[,colind.Z],var.Fitch$Z[,colind.Z]))
        colnames(Z.ind) <- c(colnames.add(colind.Z,"Moody"),colnames.add(colind.Z,"SP"),colnames.add(colind.Z,"Fitch"))
      }else{
        Z.ind <- NULL
      }
      X <- cbind(X.pool,X.ind)
      Z <- cbind(Z.pool,Z.ind)
      
      if (max(abs(var.Moody$adj_mat-var.SP$adj_mat),abs(var.Moody$adj_mat-var.Fitch$adj_mat))<10^-8){
        adj_mat <- var.Moody$adj_mat
      }else{
        stop("error in creation of adj_mat")
      }
      
      
      pos <- c(var.Moody$pos,var.SP$pos,var.Fitch$pos)
      Nobs <- c(var.Moody$Nobs,var.SP$Nobs,var.Fitch$Nobs)
      
      if (b.constant.pool){
        # Common thresholds -> can be used as above
        posc.low.prob <- c(var.Moody$posc.low.prob,var.SP$posc.low.prob,var.Fitch$posc.low.prob)
        posc.high.prob <- c(var.Moody$posc.high.prob,var.SP$posc.high.prob,var.Fitch$posc.high.prob)
        posc.low.oprob <- c(var.Moody$posc.low.oprob,var.SP$posc.low.oprob,var.Fitch$posc.low.oprob)
        posc.high.oprob <- c(var.Moody$posc.high.oprob,var.SP$posc.high.oprob,var.Fitch$posc.high.oprob)
      }else{
        #positions of thresholds: if thresholds are not to be pooled, they should be c(-Inf,c_1^Moody,c_2^Moody,c_1^SP,...,Inf)
        posshifter <- function(x,shift1,shift2){
          # position of thresholds in posc.low.oprob and posc.high.oprob need to be shifted:
          #   Position of Inf by 4
          #   Position of polr-thresholds depending on the agency: Moody=0,SP=2,Fitch=4
          # same holds for posc.low.prob and posc.high.prob
          xout <- x
          xout[x>1 & x<max(x)] <- x[x>1 & x<max(x)]+shift1
          xout[x==max(x)] <- x[x==max(x)]+shift2
          return(xout)
        }
        posc.low.prob <- c(posshifter(var.Moody$posc.low.prob,0,2),posshifter(var.SP$posc.low.prob,1,2),posshifter(var.Fitch$posc.low.prob,2,2))
        posc.high.prob <- c(posshifter(var.Moody$posc.high.prob,0,2),posshifter(var.SP$posc.high.prob,1,2),posshifter(var.Fitch$posc.high.prob,2,2))
        posc.low.oprob <- c(posshifter(var.Moody$posc.low.oprob,0,0),posshifter(var.SP$posc.low.oprob,2,2),posshifter(var.Fitch$posc.low.oprob,4,4))
        posc.high.oprob <- c(posshifter(var.Moody$posc.high.oprob,0,4),posshifter(var.SP$posc.high.oprob,2,4),posshifter(var.Fitch$posc.high.oprob,4,4))
      }
      
      conf <- conf.pool
    }
    
    if (corr){conf="none"}
    
    # Estimate the model
    ptm <- proc.time()
    res <- siop(Yoprob=Yoprob,Yprob=Yprob,Ylevel=Ylevel,X=X,Z=Z,posc.low.oprob=posc.low.oprob,posc.high.oprob=posc.high.oprob,corr=corr,conf=conf,method=c("BFGS"),b.constant.pool=b.constant.pool,Nobs=Nobs,agencies=agencies)
    ptm <- proc.time()-ptm
    
    # Save output
    if (!is.null(res)){
      print(paste("Likelihood Model",k,ag,": ",res$LL))
      
      if (ag %in% agencies){
        p.poolingtest <- NA
      }else{
        p.poolingtest <- 1-pchisq(2*(LL_ind-res$LL),Ncoeff_ind-length(res$opt_coeff))
      }
      
      restable <- make_result_table(res,ptm,agencies,p.poolingtest)
      write.csv(restable,file=paste(savename,".csv",sep=""),row.names=FALSE)
      save(list=intersect(save.elems,ls()),file=paste(savename,".RData",sep="")) 
      rm(res)
    }else{
      print("result could not be calculated")
      save(list=intersect(setdiff(save.elems,c("res","restable","ptm","p.poolingtest")),ls()),file=paste(savename,".RData",sep="")) 
    }
  }
}