plot_av_beta_weights <- function(bw_dataset_2plot, legend_location) {
  text_size = 9
  
  row.has.na <- apply(bw_dataset_2plot, 1, function(x) {
    any(is.na(x))
  })
  sum(row.has.na)
  bw_dataset_2plot <- bw_dataset_2plot[!row.has.na, ]
  
  bw_dataset_2plot_sum <-
    summarySE(
      bw_dataset_2plot,
      measurevar = "beta_weight",
      groupvars = c("beta_freq_NERB", "acquistion")
    )
  
  # set label stuff
  stim_min_nERB_mv = 4.802
  stim_max_nERB_mv = 31.746
  stim_middle_nERB_mv = ((stim_max_nERB_mv - stim_min_nERB_mv) / 2) + stim_min_nERB_mv
  stim_lower_nERB_mv = ((stim_middle_nERB_mv - stim_min_nERB_mv) / 2) + stim_min_nERB_mv
  stim_upper_nERB_mv = stim_middle_nERB_mv + ((stim_middle_nERB_mv - stim_min_nERB_mv) / 2)
  #ticks_values <- c(4.802, 7.9365, 15.873, 23.8095, 31.746)
  bw_ticks_values <-
    c(
      stim_min_nERB_mv,
      stim_lower_nERB_mv,
      stim_middle_nERB_mv,
      stim_upper_nERB_mv,
      stim_max_nERB_mv
    )
  bw_ticks_labels <- nERB2kHz(bw_ticks_values)
  bw_ticks_labels <- round(bw_ticks_labels, digits = 2)
  text_size = 9
  
  # plot data
  bw_plot <-
    ggplot(
      bw_dataset_2plot_sum,
      aes(
        x = beta_freq_NERB,
        y = beta_weight,
        colour = acquistion,
        fill = acquistion
      )
    ) +
    geom_ribbon(
      aes(ymin = beta_weight - se, ymax = beta_weight + se),
      alpha = 0.1,
      linetype = "blank"
    ) +
    geom_line() +
    geom_point()
  
  # add noise
  # bw_plot +
  #   geom_line(data=noise_dataset_2plot, aes(x=frequency_nERB, y=noise_SPLnorm, colour=NULL, fill=NULL),linetype="dashed")
  
  # add labels
  bw_plot <- bw_plot +
    scale_x_continuous(breaks = bw_ticks_values, label = bw_ticks_labels) +
    labs(x = "Frequency (kHz)", y = "Average Beta Weight (arb. units)") +
    guides(colour = guide_legend(title = NULL)) +
    guides(fill = guide_legend(title = NULL)) +
    theme(legend.position = legend_location,
          legend.text = element_text(size = text_size)) +
    theme(legend.background = element_rect(
      colour = 'white',
      fill = 'white',
      linetype = 'solid'
    )) +
    scale_colour_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1")
  
  # set sizes
  bw_plot <- bw_plot +
    theme(
      axis.text.x = element_text(
        colour = "grey20",
        size = text_size,
        angle = 0,
        hjust = 0.5,
        vjust = 0,
        face = "plain"
      ),
      axis.text.y = element_text(
        colour = "grey20",
        size = text_size,
        angle = 0,
        hjust = 0,
        vjust = 0.5,
        face = "plain"
      ),
      axis.title.x = element_text(
        colour = "grey20",
        size = text_size,
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        face = "plain"
      ),
      axis.title.y = element_text(
        colour = "grey20",
        size = text_size,
        angle = 90,
        hjust = 0.5,
        vjust = 0.5,
        face = "plain"
      )
    )
  
  return(bw_plot)
  
}
# # save plot
# save_plot("cm_av_beta_weights.png", bw_plot, device = "png", base_height = NULL,
#           base_aspect_ratio = 1.1, base_width =  3.34646,dpi = 450)