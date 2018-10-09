plot_hist_freqhist_analysis <- function(ve_dataset_histo,legend_location){
  
  text_size = 9
  
  # set plot labels
  fd_ticks_values <- c(0, 10, 20, 30, 40)
  fd_ticks_labels <- nERB2kHz(fd_ticks_values)
  fd_ticks_labels <- round(fd_ticks_labels,digits=2)
  
  y_values <- c(5000,10000,15000)
  y_labels <- y_values/100
  
  fhist_plot <- ggplot(data = ve_dataset_histo, mapping = aes(x = frequency_nERB, colour = comparision)) + 
    geom_freqpoly(binwidth = 1.5) 
  
  # + 
  #   geom_area(aes(fill = comparision), position = "identity", binwidth = 1.5, stat = "bin", alpha=0.1)
  # 
      # geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SLnorm*20000, colour=NULL),linetype="dashed") 
  
  fhist_plot <- fhist_plot +
    scale_x_continuous(breaks = fd_ticks_values, label = fd_ticks_labels) +
    scale_y_continuous(breaks = y_values, label = y_labels) +
    labs( x = "Frequency (kHz)", y = "Voxel Count (x100)") +
    guides(colour = guide_legend(title = NULL)) +
    guides(fill = guide_legend(title = NULL)) +
    theme(legend.position = legend_location,
          legend.text = element_text(size = text_size)) +
    theme(legend.background = element_rect(
      colour = 'white',
      fill = 'white',
      linetype = 'solid'
    )) +
    scale_colour_manual(name="Condition", 
                      labels = c("NH", 
                                 "sHL", 
                                 "sHL (mod)"), 
                      values = c("NH_pRF"="#e41a1c", 
                                 "sHL_pRF"="#377eb8", 
                                 "sHL_pRF_SL_level"="#4daf4a"))
  
  

  
  # fhist_plot +
  #   theme(strip.background =element_rect(fill="white")) +
  #   scale_colour_brewer(palette = "Set1") +
  #   scale_fill_brewer(palette = "Set1")
  
  # set sizes
  fhist_plot <- fhist_plot +
    theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
          axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),  
          axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))
  
  
  
}