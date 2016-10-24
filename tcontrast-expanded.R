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
  classic.lb <- ihat-t.classic*se.classic
  print(ihat)
  print(t.classic)
  print(se.classic)
  print(classic.lb)
  classic.ub <- ihat+t.classic*se.classic
  print(classic.ub)
  
  #welch
  df.welch <- (contrast^2 %*% (vars/Ns))^2/(contrast^2 %*% (vars^2/(Ns^2*(Ns-1))))
  se.welch <- sqrt(contrast^2 %*% (vars/Ns))
  t.welch <- ihat/se.welch
  p.welch <- 2*(1-pt(abs(t.welch), df.welch))
  welch.lb <- ihat-t.welch*se.welch
  welch.ub <- ihat+t.welch*se.welch
  
  output <- data.frame(t = c(t.classic, t.welch),
                       df = c(df.classic, df.welch),
                       p = c(p.classic, p.welch),
                       lb.95CI = c(classic.lb, welch.lb),
                       ub.95CI = c(classic.ub, welch.ub))
  rownames(output) <- c("Equal variances", "Separate variances")
  output <- round(output, digits = 3)
  return(output)
}