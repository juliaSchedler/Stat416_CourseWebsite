plot_acfs <- function(dataset, main){
  
  all_ts <- process_ts(dataset, ptitle = "", output = T, plots = F)
  
  p <-  forecast::ggAcf(all_ts$x_t)  + ylim(-1,1) + ggtitle(names(all_ts)[1]) +  plot_layout(nrow = 2, ncol = 3)
  for(i in names(all_ts)[!(names(all_ts)%in% c("Time", "x_t"))]){
    
    g <- forecast::ggAcf(all_ts[, i])  + ylim(-1,1) + ggtitle(i) 
    p <- p + g+ plot_layout(nrow = 2, ncol = 3) + plot_annotation(title = paste(main))
  }
  
  p
}