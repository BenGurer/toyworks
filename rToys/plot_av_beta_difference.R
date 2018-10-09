plot_av_beta_difference <- function(d,noise_dataset_2plot,secylabel,legend_location){
  
text_size = 9

# if (test_expression) {
#   statement1
# } else {
#   statement2
# }
  
data_max <- max(d$condition_difference, na.rm = TRUE) 
data_min <- min(d$condition_difference, na.rm = TRUE)
data_range <- data_max - data_min
noise_max <- max(noise_dataset_2plot$noise, na.rm = TRUE)
noise_min <- min(noise_dataset_2plot$noise, na.rm = TRUE)
noise_range <- noise_max - noise_min
scale_plot <- data_range / noise_range

axis_offset <- max(noise_dataset_2plot$noise*scale_plot-scale_plot+data_max, na.rm = TRUE) 
# noise_dataset_2plot %>% 
# mutate(n = NH + sHL) -> n$noise
noise <- (noise_dataset_2plot$noise*scale_plot)-scale_plot+data_max
frequency_nERB <- noise_dataset_2plot$frequency_nERB
n <- data.frame(frequency_nERB, noise)
axis_offset <- max(n$noise, na.rm = TRUE) 
# +noise_max-scale_noise

# dif_plot <- ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference, linetype="Difference")) +
#   geom_ribbon(aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
#   geom_line() +
#   geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=(noise*data_max), linetype="Noise")) + 
#   scale_y_continuous(sec.axis = sec_axis(~./(data_max), name = secylabel))

dif_plot <- ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference, linetype="Difference")) +
  geom_ribbon(aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
  geom_line() +
  geom_line(data=n, aes(x=frequency_nERB, y=noise, linetype="Noise")) +
  scale_y_continuous(sec.axis = sec_axis(~./axis_offset, name = secylabel,breaks = c(-1,0,1), label = c(0,0.5,1)))


# ggplot(data=d, aes(x = beta_freq_NERB, y = condition_difference, linetype="Difference")) +
#   geom_ribbon(aes(ymin=condition_difference-condition_se, ymax=condition_difference+condition_se), alpha=0.1) +
#   geom_line() +
#   geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=(noise*scale_plot)+data_max-scale_plot, linetype="Noise")) +
#   scale_y_continuous(sec.axis = sec_axis(~.*(scale_noise) + scale_noise, name = secylabel))

# add labels
dif_plot <- dif_plot +
  scale_x_continuous(breaks = bw_ticks_values, label = bw_ticks_labels) +
  labs( x = "Frequency (kHz)", y = "Average Beta Weight Difference (arb. units)") +
  guides(linetype = guide_legend(title = NULL)) +
  theme(legend.position = legend_location,
        legend.text = element_text(size = text_size)) +
  theme(legend.background = element_rect(
    colour = NULL,
    fill = NULL,
    linetype = 'solid'
  )) +
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1")

# set sizes
dif_plot <- dif_plot +
  theme(axis.text.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=text_size,angle=0,hjust=0,vjust=0.5,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=text_size,angle=0,hjust=0.5,vjust=0.5,face="plain"),
        axis.title.y = element_text(colour="grey20",size=text_size,angle=90,hjust=0.5,vjust=0.5,face="plain"))

return(dif_plot)

}
# # save plot
# save_plot("cm_av_beta_weights_difference.png", dif_plot, device = "png", base_height = NULL,
#           base_aspect_ratio = 1.6, base_width =  3.34646,dpi = 450)
