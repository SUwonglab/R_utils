library(grDevices)

plot.gene = function(gene,
                     embedding, 
                     data.mat) {
    gene.index = which(tolower(rownames(data.mat)) == gene)
    gene.col = c()
    value = c()
    rbPal = colorRampPalette(c('darkred','lightgrey'))
    for (i in 1:dim(embedding)[1]) {
        if (data.mat[gene.index, i] > 0) {
            gene.col = c(gene.col, 'replace')
            value = c(value, data.mat[gene.index, i])
        } else {
            gene.col = c(gene.col, 'lightgrey')
        }
    }
    gene.col[gene.col=='replace'] = rbPal(length(unique(value)))[as.numeric(cut(value,breaks = length(unique(value))))]
    plot(embedding, 
         col=gene.col,
         pch=19,
         cex=0.5,
         main=paste('Marker gene analysis', gene),
         ylab='Y',
         xlab='X')
}
