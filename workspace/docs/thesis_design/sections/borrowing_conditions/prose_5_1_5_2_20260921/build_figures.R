suppressPackageStartupMessages({library(data.table);library(ggplot2);library(patchwork);library(jsonlite);library(digest)})
out<-'docs/thesis_design/sections/borrowing_conditions/prose_5_1_5_2_20260921'
j<-fromJSON('data-derived/p15_master/current_run.json')
p<-file.path(j$stages$regional$dir,'country_year_views.csv')
x<-fread(p);x[reviewed_eight_exclusion==TRUE,rate:=NA_real_]
x<-x[view=='no_peer' & is.finite(rate)]
a<-x[,.(countries=.N,mean_rate=mean(rate),median_rate=median(rate),
 q25=unname(quantile(rate,.25)),q75=unname(quantile(rate,.75))),by=analysis_year][order(analysis_year)]
r<-x[,.(countries=.N,mean_rate=mean(rate)),by=.(region,analysis_year)]
prior<-'docs/thesis_design/sections/borrowing_conditions/content_5_1_5_2_20260921'
ref<-fread(file.path(prior,'annual_rates.csv'));stopifnot(isTRUE(all.equal(a,ref,check.attributes=FALSE)))
rr<-merge(r,fread(file.path(prior,'regional_rates.csv')),by=c('region','analysis_year'))
stopifnot(nrow(rr)==78L,all(rr$countries.x==rr$countries.y),all(abs(rr$mean_rate.x-rr$mean_rate.y)<1e-10))
fwrite(a,file.path(out,'overall_figure_data.csv'));fwrite(r,file.path(out,'regional_figure_data.csv'))
base_theme<-theme_minimal(base_size=11,base_family='serif')+theme(panel.grid.minor=element_blank(),
 panel.grid.major.x=element_blank(),panel.grid.major.y=element_line(colour='#E5E5E5',linewidth=.3),
 axis.title=element_text(size=10),legend.position='top',legend.title=element_blank(),
 plot.margin=margin(7,12,5,7),strip.text=element_text(face='bold',size=10,hjust=0))
xs<-function() scale_x_continuous(breaks=seq(2012,2024,2),limits=c(2011.7,2024.3),expand=c(0,0))
top<-ggplot(a,aes(analysis_year))+geom_ribbon(aes(ymin=q25,ymax=q75),fill='#D9E3EA')+
 geom_line(aes(y=mean_rate,linetype='Mean'),colour='#173F57',linewidth=.8)+
 geom_line(aes(y=median_rate,linetype='Median'),colour='#173F57',linewidth=.6)+
 scale_linetype_manual(values=c(Mean='solid',Median='longdash'))+xs()+
 scale_y_continuous(limits=c(0,12),breaks=seq(0,12,3),expand=expansion(mult=c(0,.02)))+
 labs(x=NULL,y='Borrowing rate (%)')+base_theme+theme(axis.text.x=element_blank())
counts<-ggplot(a,aes(analysis_year,countries))+geom_col(fill='#8BA5B6',width=.55)+
 geom_text(aes(label=countries),vjust=-.5,size=3,family='serif')+xs()+
 scale_y_continuous(limits=c(0,105),breaks=c(0,50,100),expand=c(0,0))+
 labs(x=NULL,y='Countries')+base_theme
f<-top/counts+plot_layout(heights=c(3,1))
for(ext in c('png','pdf'))ggsave(file.path(out,paste0('overall_borrowing_conditions.',ext)),f,width=7,height=4.8,dpi=220,bg='white')
r[,region_label:=gsub(' & ',' and ',region,fixed=TRUE)]
r[,display_rate:=fifelse(countries>=3,mean_rate,NA_real_)]
lab<-r[analysis_year %in% c(2012,2016,2020,2024)]
g<-ggplot(r,aes(analysis_year,display_rate))+geom_line(colour='#173F57',linewidth=.7)+
 geom_point(colour='#173F57',size=.8)+facet_wrap(~region_label,ncol=2)+
 scale_x_continuous(breaks=c(2012,2016,2020,2024),limits=c(2011.6,2024.4),expand=c(0,0))+
 scale_y_continuous(breaks=c(0,5,10,15),limits=c(-2.4,15.4),expand=c(0,0))+
 geom_text(data=lab,aes(x=analysis_year,y=-1.7,label=paste0('n=',countries)),
 inherit.aes=FALSE,size=2.65,family='serif',colour='#505050')+
 labs(x=NULL,y='Mean borrowing rate (%)')+base_theme+theme(panel.spacing=grid::unit(1,'lines'))
for(ext in c('png','pdf'))ggsave(file.path(out,paste0('regional_borrowing_conditions.',ext)),g,width=7.4,height=6.3,dpi=220,bg='white')
inputs<-c('data-derived/p15_master/current_run.json',p,file.path(out,'build_figures.R'),
 file.path(prior,c('annual_rates.csv','regional_rates.csv')))
fwrite(data.table(path=inputs,sha256=vapply(inputs,digest,character(1),file=TRUE,algo='sha256')),file.path(out,'figure_input_manifest.csv'))
writeLines(capture.output(sessionInfo()),file.path(out,'environment.txt'))
cat('Both figure datasets reproduce checked content evidence.\n')
