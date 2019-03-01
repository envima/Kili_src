lvlplt <- function(mat, name, font_sz = 0.35, 
                   lbl_x, lbl_y, rnge = seq(0,1,0.1), main = "", maxcat = 5,
                   clr = colorRampPalette(brewer.pal(9, "YlOrRd"))){
  xdim <- dim(mat)[2]
  ydim <- dim(mat)[1]
  rst <- raster(mat, xmn = 0.5, xmx = (xdim+0.5), ymn = 0.5, ymx = ydim+0.5)
  # a <- rst
  # rst <- as.factor(a)
  rst <- ratify(rst)
  rst_rat <- levels(rst)[[1]]
  rst_rat$value <- as.character(unique(rst))
  levels(rst) <- rst_rat
  print(rasterVis::levelplot(x = rst, scales = list(x = list(rot=90, cex = font_sz, at = 1:xdim, labels = lbl_x), 
                                     y = list(at = c(ydim:1), cex = font_sz, labels = lbl_y)), 
                  margin = FALSE, 
                  main = main, 
                  col.regions = c("#FFFFFF", clr(maxcat)[seq(rst@data@min ,rst@data@max)]), 
                  # colorkey = list(space = "bottom"), 
                  at = rnge))
}


#                  breaks = seq(0.5, 5.5, 1), 
