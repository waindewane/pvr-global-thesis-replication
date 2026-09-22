# Reproduce the approved coverage display from independently verified count inputs.
library(ggplot2)
library(data.table)
base <- "docs/thesis_design/sections/benchmark_evaluation"
out <- file.path(base,"coverage_prose_20260920")
d <- fread(file.path(base,"coverage_20260920/annual_selected_sources.csv"))
vars <- c("primary","ids","secondary","rating_implied","peer","no_rate")
stopifnot(all(rowSums(d[,..vars])==d$country_years),all(d$country_years>0))
z <- melt(d,id.vars=c("analysis_year","country_years"),measure.vars=vars,
          variable.name="source",value.name="country_count")
z[,share:=100*country_count/country_years]
z[,source:=factor(source,levels=vars)]
labels <- c("Primary issuance","IDS bondholder terms","Secondary yields",
            "Rating-implied rates","Peer estimates","No selected rate")
colours <- c("#244762","#527A98","#95B7CC","#3D857C","#CCAE73","#E7E7E7")
p <- ggplot(z,aes(x=analysis_year,y=share,fill=source))+
 geom_col(width=.76,position=position_stack(reverse=TRUE),colour="white",linewidth=.18)+
 geom_text(data=d,aes(x=analysis_year,y=104,label=country_years),inherit.aes=FALSE,
           size=3.1,colour="#333333")+
 scale_fill_manual(values=setNames(colours,vars),labels=labels,drop=FALSE)+
 scale_x_continuous(breaks=2012:2024,expand=expansion(add=.65))+
 scale_y_continuous(breaks=seq(0,100,25),limits=c(0,109),expand=expansion(mult=0))+
 labs(x=NULL,y="Share of country-years (%)",fill=NULL)+
 guides(fill=guide_legend(nrow=2,byrow=TRUE))+
 theme_minimal(base_size=11,base_family="sans")+
 theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
       panel.grid.major.y=element_line(colour="#D9D9D9",linewidth=.3),
       axis.text=element_text(colour="#333333"),axis.text.x=element_text(size=9),
       axis.title.y=element_text(margin=margin(r=10)),legend.position="bottom",
       legend.text=element_text(size=9.5),legend.key.width=grid::unit(13,"pt"),
       legend.key.height=grid::unit(10,"pt"),plot.margin=margin(8,8,4,4))
ggsave(file.path(out,"annual_coverage.png"),p,width=9,height=4.6,dpi=220,bg="white")
ggsave(file.path(out,"annual_coverage.pdf"),p,width=9,height=4.6,device=pdf,useDingbats=FALSE,bg="white")
fwrite(z,file.path(out,"annual_coverage_plot_data.csv"))
