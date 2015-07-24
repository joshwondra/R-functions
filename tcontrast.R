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
  
  result <- list(ihat=ihat, est.vars=vars, se.classic=se.classic, t.classic=t.classic, df.classic=df.classic, p.classic=p.classic, se.welch=se.welch, t.welch=t.welch, df.welch=df.welch, p.welch=p.welch)
  return(result)
}