require(ggplot2)
require(reshape)
require(scales)

# setwd('../output')

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

network_stats_single_plot <- function (stats) {
	
	measure=names(stats)[2]
	stat_plot = ggplot(data=stats,aes_string(x="Proportion",y=measure)) + geom_line(aes(),colour="#3182BD") + 
	geom_point(aes(),colour="#3182BD")+theme(panel.border=element_rect(colour="black",fill=NA),panel.grid.minor=element_blank(),
	panel.grid.major=element_blank(),panel.background=element_blank(),legend.position="none",
	axis.text=element_text(size=8)) + xlab("")

	return(stat_plot)

	}


running_variance_plot <- function (stats) {
	
	measure=names(stats)[2]
	stat_plot = ggplot(data=stats,aes_string(x="Proportion",y=measure)) + geom_line(aes(),colour="#3182BD")+theme(panel.border=element_rect(colour="black",fill=NA),panel.grid.minor=element_blank(),
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
	


topology_measures=read.table("summary_binary_by_proportion_ntwk_covered.txt",header=T)
running_variances=read.table("running_variance_binary_by_proportion_ntwk_covered.txt",header=T)
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
trans_plot_var= running_variance_plot(transitivity_var) + xlab("Proportion") + ylab("Running variance")
cpl_plot_var= running_variance_plot(cpl_var)
assort_plot_var= running_variance_plot(assortativity_var)

multiplot(int_plot, int_plot_var, trans_plot,  trans_plot_var, cpl_plot, cpl_plot_var, assort_plot,assort_plot_var,cols=2)