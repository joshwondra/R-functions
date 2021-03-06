# Runs a contrast based on same variances and separate variances

t.contrast <- function(dv, groups, contrast) {
  means <- by(dv, groups, mean)
  vars <- by(dv, groups, var)
  Ns <- by(dv, groups, length)
  ihat <- contrast %*% means
  
  #classic
  df.classic <- sum(Ns)-length(Ns)
  mse <- sum(vars*(Ns-1))/df.classic
  se.classic <- sqrt(mse*(contrast^2 %*% (1/Ns)))
  t.classic <- ihat/se.classic
  p.classic <- 2*(1-pt(abs(t.classic), df.classic))
  
  #welch
  df.welch <- (contrast^2 %*% (vars/Ns))^2/(contrast^2 %*% (vars^2/(Ns^2*(Ns-1))))
  se.welch <- sqrt(contrast^2 %*% (vars/Ns))
  t.welch <- ihat/se.welch
  p.welch <- 2*(1-pt(abs(t.welch), df.welch))
  
  #compute confidence intervals
  t.ci.classic <- qt(.025, df = df.classic)      
  t.ci.welch <- qt(.025, df = df.welch)
  
  classic.lb <- ihat-t.ci.classic*se.classic
  classic.ub <- ihat+t.ci.classic*se.classic
  welch.lb <- ihat-t.ci.welch*se.welch
  welch.ub <- ihat+t.ci.welch*se.welch
  
  output <- data.frame(t = c(t.classic, t.welch),
                       df = c(df.classic, df.welch),
                       p = c(p.classic, p.welch),
                       lb.95CI = c(min(classic.lb, classic.ub), min(welch.lb, welch.ub)),
                       ub.95CI = c(max(classic.lb, classic.ub), max(welch.lb, welch.ub)))
  rownames(output) <- c("Equal variances", "Separate variances")
  output <- round(output, digits = 3)
  return(output)
}