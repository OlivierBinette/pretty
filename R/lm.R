#' @export
plot.envelopes <- function(out, level) {
  # Specifying evaluation points
  out = lm(out, x=T)
  r = range(out$x[,2]) * c(-1.1, 1.1)
  points = seq(r[1], r[2], length.out = 200)
  x = cbind(1, points)

  # Computing mean and variances
  mean = x %*% c(out$coefficients[[1]], out$coefficients[[2]])
  XtX.inv = solve(crossprod(out$x))
  sig.est = summary(out)$sigma

  # Computing interval lengths
  cs = colSums( t(x) * (XtX.inv %*% t(x)))
  size1 = sig.est * sqrt( 2* qf(.95, 2, out$df) * cs )

  polygon(c(points, rev(points)), c(mean-size1, rev(mean+size1)),
          border=NA, col=adjustcolor(cmap.knitr(0), alpha.f=.15))
}
