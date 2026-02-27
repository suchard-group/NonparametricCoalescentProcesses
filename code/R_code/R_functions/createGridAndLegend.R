createGridAndLegend <- function(listGrids, title = "", ncol = 4) {
  titleGrid <- ggdraw() + draw_label(title, fontface = 'bold', x = 0, hjust = 0) # can add margin to
  legendGrid <- get_legend(listGrids[[1]] + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))
  nt <- theme(legend.position = 'hidden')
  plotsGrid <- plot_grid(plotlist =  lapply(listGrids, function(p) p + nt), ncol = ncol)
  plot_grid(titleGrid, legendGrid, ncol=1, rel_heights = c(1, .5))|> plot_grid(plotsGrid, ncol = 1, rel_heights = c(.1, 1))
}


g_record <- function(plot, width = 3, height = 2.5) {
  showtext_auto()
  gg_record(device = "pdf", width = width, height = height, units = "in", dpi = 300)   
  print(plot)
  gg_stop_recording()
}