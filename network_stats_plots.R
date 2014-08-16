require(ggplot2)
require(reshape)
require(scales)

setwd('plots_summaries')

network_stats_single_plot <- function (stats) {
	
	measure=names(stats)[2]
	stat_plot = ggplot(data=stats,aes_string(x="Year",y=measure)) + geom_line(aes(),colour="#3182BD") + 
	geom_point(aes(),colour="#3182BD")+theme(panel.border=element_rect(colour="black",fill=NA),panel.grid.minor=element_blank(),
	panel.grid.major=element_blank(),panel.background=element_blank(),legend.position="none",
	axis.text=element_text(size=8)) + xlab("")+ scale_x_continuous(breaks=seq(2000,2014,2))

	return(stat_plot)

}


running_variance_plot <- function (stats) {
	
	measure=names(stats)[2]
	stat_plot = ggplot(data=stats,aes_string(x="Year",y=measure)) + geom_line(aes(),colour="#3182BD")+theme(panel.border=element_rect(colour="black",fill=NA),panel.grid.minor=element_blank(),
	panel.grid.major=element_blank(),panel.background=element_blank(),legend.position="none",
	axis.text=element_text(size=8)) + xlab("") + ylab("") + scale_x_continuous(breaks=seq(2000,2014,2))

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
	


topology_measures=read.table("network_topology_1999-2014_115_3-fold.txt",header=T)
running_variances=read.table("network_topology_1999-2014_115_3-fold_running_vars.txt",header=T)
norm_rv=normalize_df(running_variances,c(2,5))

interactions <- topology_measures[1:2]
transitivity = cbind(topology_measures[1], topology_measures[3])
cpl = cbind(topology_measures[1], topology_measures[4])
assortativity = cbind(topology_measures[1], topology_measures[5])

int_plot=network_stats_single_plot(interactions)
trans_plot=network_stats_single_plot(transitivity)
cpl_plot=network_stats_single_plot(cpl)
assort_plot=network_stats_single_plot(assortativity)

interactions_var <- norm_rv[1:2]
transitivity_var = cbind(norm_rv[1], norm_rv[3])
cpl_var = cbind(norm_rv[1], norm_rv[4])
assortativity_var = cbind(norm_rv[1], norm_rv[5])

int_plot_var=running_variance_plot(interactions_var)
trans_plot_var= running_variance_plot(transitivity_var) + xlab("Year") + ylab("Running variance")
cpl_plot_var= running_variance_plot(cpl_var)
assort_plot_var= running_variance_plot(assortativity_var)

multiplot(int_plot, int_plot_var, trans_plot,  trans_plot_var, cpl_plot, cpl_plot_var, assort_plot,assort_plot_var,cols=2)
