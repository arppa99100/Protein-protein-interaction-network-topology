require(ggplot2)
require(reshape2)
require(scales)
require(wq)
require(gridExtra)

setwd('../output')

network_stats_single_plot <- function (stats) {
	
	measure <- names(stats)[2]
	stat_plot <- ggplot(data=stats,aes(x=Proportion,y=value,colour = variable)) + geom_line() + 
	geom_point(aes())+theme(panel.border=element_rect(colour="black",fill=NA),panel.grid.minor=element_blank(),
	panel.grid.major=element_blank(),panel.background=element_blank(),legend.position="none",
	axis.text=element_text(size=8)) + xlab("")

	return(stat_plot)

	}


running_variance_plot <- function (stats) {
	
	measure <- names(stats)[2]
	stat_plot <- ggplot(data=stats,aes(x=Proportion,y=value,colour = variable)) + geom_line()+theme(panel.border=element_rect(colour="black",fill=NA),panel.grid.minor=element_blank(),
	panel.grid.major=element_blank(),panel.background=element_blank(),legend.position="none",
	axis.text=element_text(size=8)) + xlab("") + ylab("")

	return(stat_plot)

	}


normalize_column <- function (x) {
	(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
	}



normalize_df <- function (data_file,cols=c(1,len(data_file))) {
	
	 for (i in cols[1]:cols[2]) {
	 	data_file[,i]=normalize_column(data_file[,i])
	 	}
	 
	 return (data_file)
	 }


#------------------------------------------------------------------------------------------


topology_measures <- read.table("summary_co_complex_by_proportion_ntwk_covered.txt",header=T)
running_variances <- read.table("running_variance_co_complex_by_proportion_ntwk_covered.txt",header=T)
norm_rv <- normalize_df(running_variances,c(2,5))


topology_2 <- read.table("summary_binary_by_proportion_ntwk_covered.txt",header=T)
running_variances_2 <- read.table("running_variance_binary_by_proportion_ntwk_covered.txt",header=T)
norm_rv_2 <- normalize_df(running_variances_2,c(2,5))


#------------------------------------------------------------------------------------------


interactions <- cbind(topology_measures[1:2],topology_2[2])
names(interactions) <- c("Proportion","Co-complex assays","Binary assays")
transitivity <- cbind(topology_measures[1], topology_measures[3],topology_2[3])
names(transitivity) <- c("Proportion","Binary","Co-complex")
cpl <- cbind(topology_measures[1], topology_measures[4],topology_2[4])
names(cpl) <- c("Proportion","Co-complex assays","Binary assays")
assortativity <- cbind(topology_measures[1], topology_measures[5],topology_2[5])
names(assortativity) <- c("Proportion","Binary","Co-complex")

interactions <- melt(interactions, id="Proportion")
transitivity <- melt(transitivity, id="Proportion")
cpl <- melt(cpl, id="Proportion")
assortativity <- melt(assortativity, id="Proportion")

#int_plot <- network_stats_single_plot(interactions) + ylab("Interactions") + theme(legend.position="top",legend.title=element_blank())
measure <- names(interactions)[2]

int_plot <- ggplot(data= interactions,aes(x=Proportion,y=value,colour = variable)) + geom_line() + geom_point(aes()) + theme(panel.border=element_rect(colour="black",fill=NA),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),panel.background=element_blank(),legend.position="none",axis.text=element_text(size=8)) + xlab("")+scale_y_continuous(breaks=c(5000,10000,15000,20000),limits=c(0,22000)) + geom_hline(aes(yintercept=20000),linetype="dashed") + ylab("Interactions") +  annotate("text",x=.3,y=21500,label="Estimated complete binary PPI network interactions",cex=2.4)

trans_plot <- network_stats_single_plot(transitivity) + ylab("Transitivity")
cpl_plot <- network_stats_single_plot(cpl) + ylab("CPL") + theme(legend.position="top",legend.title=element_blank())
assort_plot <- network_stats_single_plot(assortativity) + ylab("Assortativity")


#------------------------------------------------------------------------------------------


interactions_var <- cbind(norm_rv[1:2],norm_rv_2[2])
names(interactions_var) <- c("Proportion","Binary","Co-complex")
transitivity_var <- cbind(norm_rv[1], norm_rv[3],norm_rv_2[3])
names(transitivity_var) <- c("Proportion","Binary","Co-complex")
cpl_var <- cbind(norm_rv[1], norm_rv[4],norm_rv_2[4])
names(cpl_var) <- c("Proportion","Binary","Co-complex")
assortativity_var <- cbind(norm_rv[1], norm_rv[5],norm_rv_2[5])
names(assortativity_var) <- c("Proportion","Binary","Co-complex")

interactions_var <- melt(interactions_var, id="Proportion")
transitivity_var <- melt(transitivity_var, id="Proportion")
cpl_var <- melt(cpl_var, id="Proportion")
assortativity_var <- melt(assortativity_var, id="Proportion")

int_plot_var <- running_variance_plot(interactions_var)
trans_plot_var <- running_variance_plot(transitivity_var) + xlab("Proportion of est. complete network") + ylab("Running variance")
cpl_plot_var <- running_variance_plot(cpl_var)
assort_plot_var <- running_variance_plot(assortativity_var)


#------------------------------------------------------------------------------------------


# multiplot(int_plot, int_plot_var, trans_plot,  trans_plot_var, cpl_plot, cpl_plot_var, assort_plot,assort_plot_var,cols=2)
layOut(list(int_plot,1:3,1:2),
		list(int_plot_var,4:5,1:2),
		list(cpl_plot,1:3,3:4),
		list(cpl_plot_var,4:5,3:4),
		list(trans_plot,6:8,1:2),
		list(trans_plot_var,9:10,1:2),
		list(assort_plot,6:8,3:4),
		list(assort_plot_var,9:10,3:4))


# g_legend<-function(p){
# tmp <- ggplotGrob(p)
# leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
# legend <- tmp$grobs[[leg]]
# return(legend)}

# legend_plot <- ggplot(data=interactions,aes(x=Year,y=value,colour = variable)) + geom_line()
# leg_plot <- ggplot(data=interaction)
# legend <- g_legend(legend_plot)


# grid.arrange(arrangeGrob(arrangeGrob(int_plot,trans_plot,cpl_plot,assort_plot,int_plot_var,trans_plot_var,cpl_plot_var,assort_plot_var),legend,ncol=2),heights=1:8)

