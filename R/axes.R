
x_axis <- function(lwd = .5, mark=NULL, mark.label=NULL,
                   cex.axis = .8,
                   family="serif",
                   ...) {
  if (!is.null(mark)) {
    xaxp = par("xaxp")
    pos = seq(xaxp[[1]], xaxp[[2]], length.out=xaxp[[3]]+1)
    i = which.min(abs(mark - pos))
    pos[[i]] = mark
    labels = pos
    labels[[i]] = if (is.null(mark.label)) formatC(mark) else mark.label
    axis(1, at = pos, labels = labels, tcl = -0.15, mgp = c(0,0.25,0),
         las=1, lwd=lwd, family=family, cex.axis=cex.axis, ...)
  }
  else {
    axis(1, tcl = -0.15, mgp = c(0,0.25,0),
         las=1, lwd=lwd, family=family, cex.axis=cex.axis, ...)
  }
}

y_axis <- function(lwd = .5, mark=NULL, mark.label=NULL,
                   cex.axis = .8,
                   family="serif",
                   ...) {
  if (!is.null(mark)) {
    yaxp = par("yaxp")
    pos = seq(yaxp[[1]], yaxp[[2]], length.out=yaxp[[3]]+1)
    i = which.min(abs(mark - pos))
    pos[[i]] = mark
    labels = pos
    labels[[i]] = if (is.null(mark.label)) formatC(mark) else mark.label
    axis(2, at = pos, labels = labels, tcl = -0.15, mgp = c(0,0.5,0),
         las=1, lwd=lwd, family=family, cex.axis=cex.axis, ...)
  }
  else {
    axis(2, tcl = -0.15, mgp = c(0,0.5,0),
         las=1, lwd=lwd, family=family, cex.axis=cex.axis, ...)
  }
}
