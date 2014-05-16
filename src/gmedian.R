# This Function computes the median of a grouped frequency distribution.
gmedian <- function(grpVar, percentile = 0.5) {
  if(is.factor(grpVar)) grpVar <- as.numeric(grpVar)
  scale_step <- as.numeric(names(table(grpVar))[2]) - as.numeric(names(table(grpVar))[1])
  grp_detect <- ifelse(scale_step==1, 0.5 , 0)
  algeb_sign <- 1
  egs <- quantile(grpVar, percentile, na.rm=T)
  egs_pointer_abs <- length(table(subset(grpVar, grpVar <= egs)))
  cumU <- sum(table(grpVar)[1 : egs_pointer_abs-1])
  intpol <- (length(na.omit(grpVar)) / 2 - cumU) / table(grpVar)[egs_pointer_abs] * scale_step
  ifelse(intpol <= 0, algeb_sign <- -1, algeb_sign <- 1)
  val <- as.numeric(names(table(grpVar))[egs_pointer_abs]) - grp_detect + (intpol * algeb_sign)
  round(val, digits = 3)
}
 
mQ <- function(grpVar) {
  (gmedian(grpVar, 0.75)-gmedian(grpVar, 0.25)) / 2
}
