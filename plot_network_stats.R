require(ggplot2)
require(reshape)
require(scales)
require(RColorBrewer)
raw_data=read.table("summary.txt",header=T)

network_stats_single_plot <- function (stats) {
	
	colors=brewer.pal(3,"Blues")
	measure=names(stats)[2]
	stat_plot = ggplot(data=stats,aes_string(x="Year",y=measure)) + geom_line(aes(),colour=colors[3]) + geom_point(aes(),colour=colors[3])+theme(panel.border=element_rect(colour="black",fill=NA),panel.grid.minor=element_blank(),panel.grid.major=element_blank(),panel.background=element_blank(),legend.position="none",axis.text=element_text(size=8)) + xlab("")+ scale_x_continuous(breaks=seq(2000,2014,2))

	return(stat_plot)

	}

interactions <- raw_data[1:2]
transitivity = cbind(raw_data[1],raw_data[3])
cpl = cbind(raw_data[1],raw_data[4])
assortativity = cbind(raw_data[1],raw_data[5])

int_plot=network_stats_single_plot(interactions)
trans_plot=network_stats_single_plot(transitivity)
cpl_plot=network_stats_single_plot(cpl)
assort_plot=network_stats_single_plot(assortativity) + xlab("Year")


multiplot(int_plot, trans_plot, cpl_plot, assort_plot,cols=2)
