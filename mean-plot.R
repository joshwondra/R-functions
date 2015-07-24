mean.plot <- function(dv, f1, f2, ylab='Mean', ylim=c(min(dv, na.rm=TRUE),max(dv, na.rm=TRUE))) {
    if(is.factor(f1)==FALSE | is.factor(f2)==FALSE) {
        stop('f1 and f2 must be factors')
    }
    means <- by(dv, list(f1,f2), mean, na.rm=TRUE)
    ses <- by(dv, list(f1, f2), function(x){sd(x, na.rm=TRUE)/sqrt(length(x))})
    factor1 <- factor(rep(levels(f1), length(unique(f2))), levels=levels(f1))
    factor2 <- factor(rep(levels(f2), each=length(unique(f1))), levels=levels(f2))
    plot.data <- data.frame(means=as.numeric(means), ses=as.numeric(ses), factor1, factor2)
    
    library(ggplot2)
    ggplot(plot.data, aes(y=means, x=factor1, fill=factor2)) +
        geom_bar(stat='identity', position='dodge') +
        geom_errorbar(stat='identity', position='dodge', aes(ymin=means-ses, ymax=means+ses)) + 
        ylim(ylim) +
        ylab(ylab)
}
