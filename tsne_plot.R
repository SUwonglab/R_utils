plot.util <- function(tsne.coord,
                      color.assignment,
                      title,
                      add.labels,
                      cluster.labels,
                      cex,
                      text.cex) {
    plot(tsne.coord, 
         col=color.assignment,
         pch=19,
         cex=cex,
         main=title,
         ylab='Y',
         xlab='X')
    
    if (add.labels) {
        assertthat::assert_that(!is.null(cluster.labels))
        cluster.labels = as.character(cluster.labels)
        unique.labels = unique(cluster.labels)
        for (i in 1:length(unique.labels)) {
            coord = apply(tsne.obj[which(cluster.labels == unique.labels[i]), ], 2, mean)
            text(coord[1], coord[2], cluster.labels=unique.labels[i], cex=text.cex)
        }
    }
}

#' Plotting function for Rtsne.
#'
#' @param tsne.coord - X and Y coordinates.
#' @param color.assignment - a vector of valid colors assigned to each data point.
#' @param title - title of the plot ('main' in plot).
#' @param add.labels - whether or not to add text labels to each cluster.
#' @param cluster.labels - the corresponding labels of each cluster if add.labels is specified to be 'T'.
#' @param save.plot - whether or not to save the plot.
#' @param save.dir - filename of the resulting plot (e.g. './plots/sc_rna_seq_1.png')
#' plot.tsne()
plot.tsne <- function(tsne.coord,
                      color.assignment,
                      title,
                      add.labels = F,
                      cluster.labels = NULL,
                      cex = 0.5,
                      text.cex = 0.7,
                      save.plot = F,
                      save.dir = NULL) {
    plot.util(tsne.coord,
              color.assignment,
              title,
              add.labels,
              cluster.labels,
              cex,
              text.cex)
    
    if (save.plot) {
        assertthat::assert_that(!is.null(save.dir))
        png(save.dir,
            width=1200, height=1200, res=300)
        plot.util(tsne.coord,
                  color.assignment,
                  title,
                  add.labels,
                  cluster.labels,
                  cex,
                  text.cex)
        dev.off()
    }
}








