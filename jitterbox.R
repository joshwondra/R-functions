jitterbox <- function(dv, f1, f2=NULL, ylab='Value') {
    if(is.factor(f1)==FALSE | (is.factor(f2) | is.null(f2))==FALSE) {
        stop('f1 and f2 must be factors')
    }
    
    plot.data <- data.frame(dv, f1)
    if(is.null(f2)==FALSE) {
        plot.data <- data.frame(plot.data,f2)
    }
    
    library(ggplot2)
    ggplot(plot.data, aes(y=dv, x=f1, fill=f2)) +
        geom_boxplot() +
        geom_jitter() +
        ylab(ylab)
}