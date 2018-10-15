plot_hist_density_analysis <- function(ve_dataset_histo,legend_location){
  text_size = 9
  # set plot labels
  fd_ticks_values <- c(0, 10, 20, 30, 40)
  fd_ticks_labels <- nERB2kHz(fd_ticks_values)
  fd_ticks_labels <- round(fd_ticks_labels,digits=2)
  
  stim_min_nERB = 3.3589
  stim_max_nERB = 33.1892
  stim_middle_nERB = 18.2740
  
  fhist_plot <- ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, fill = analysis, colour = analysis)) + 
    geom_density(alpha=0.3)
  
  fhist_plot <- fhist_plot +
    scale_x_continuous(breaks = fd_ticks_values, label = fd_ticks_labels) +
    labs( x = "Frequency (kHz)", y = "Voxel Count") +
    guides(colour=guide_legend(title=NULL)) + 
    guides(fill=guide_legend(title=NULL)) + 
    theme(legend.position=legend_location,legend.text=element_text(size=text_size)) +
    theme(legend.background = element_rect(colour = 'white', fill = 'white', linetype='solid')) +
    scale_colour_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    geom_vline(xintercept=stim_min_nERB, linetype="dashed", color = "black") +
    geom_vline(xintercept=stim_max_nERB, linetype="dashed", color = "black") +
    geom_vline(xintercept=stim_middle_nERB, linetype="dashed", color = "black")
  
  
  # set sizes
  fhist_plot <- fhist_plot +
    theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))
  
  return(fhist_plot)
  
}
