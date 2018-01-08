find_color <- function (col, cmap) {
  if (!anyNA(suppressWarnings(as.numeric(col)))
      && all.equal(as.numeric(col), as.integer(col))) {
    return(cmap(col))
  }
  return(col)
}

prettify <- function() {
  par(tcl = -0.15,            # Ticks length.
      mgp = c(2,0.25,0),      # Labels positioning.
      cex.axis = .8
      )
}

x_axis <- function(lwd = .5, ...) {
  axis(1, tcl = -0.15, mgp = c(0,0.25,0),
       las=1, lwd=lwd, ...)
}

y_axis <- function(lwd = .5, ...) {
  axis(2, tcl = -0.15, mgp = c(0,0.5,0),
       las=1, lwd=lwd, ...)
}
