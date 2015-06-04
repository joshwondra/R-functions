prop.plot <- function(dv, f1, f2, ylab='Proportion', xlab='f1', leglab='f2') {
    if(is.factor(f1)==FALSE | is.factor(f2)==FALSE) {
        stop('f1 and f2 must be factors')
    }
    props <- by(dv, list(f1,f2), function(x){sum(x)/length(x)})    
    factor1 <- factor(rep(levels(f1), length(unique(f2))), levels=levels(f1))
    factor2 <- factor(rep(levels(f2), each=length(unique(f1))), levels=levels(f2))
    plot.data <- data.frame(props=as.numeric(props), factor1, factor2)
    
    library(ggplot2)
    ggplot(plot.data, aes(y=props, x=factor1, fill=factor2)) +
        geom_bar(stat='identity', position='dodge') +
        ylim(0,1) +
        ylab(ylab) +
        xlab(xlab) +
        guides(fill=guide_legend(title=leglab))
}