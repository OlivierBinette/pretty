
#' Replacement to hist()
#'
#' @export
hist <-
  function(x, xlab = NULL, ylab = NULL, main = NULL,
           alpha = .6,
           cmap = NULL,
           col = 1,                # Color.
           border = rgb(0,0,0,0),          # Border color.
           freq = F, breaks = NULL,        # Histogram type and number of bins.
           yaxt = "n",                     # y axis type.
           pch = 20,                       # Point style.
           bty = "n",                        # Box type.
           lwd = .75, tcl=-0.15,           # Axis line width and ticks length.
           mgp = c(2,0.25,0),                # Labels positioning.
           cex.axis = .8,                    # Scaling.
           family = "serif",
           axes = T,
           showmean=F,
           xmark = NULL,
           xmark.label = NULL,
           ...) {

    # Adjusting colors.
    if (!missing(cmap)) {
      col <- cmap(col)
    }
    else if(missing(cmap) && !anyNA(suppressWarnings(as.numeric(col)))
            && all.equal(as.numeric(col), as.integer(col))) {
      col <- cmap.seaborn(col)
      col <- adjustcolor(col, alpha.f = alpha)
    }

    # Automatic number of bins.
    if (missing(breaks)) {
      breaks <- 3*floor(length(x)^.33) + 3
    }

    h = graphics::hist(x, xlab=xlab, ylab=ylab, main=main, col=col, border=border,
                       freq=freq, breaks=breaks, yaxt=yaxt, lwd=lwd, tcl=tcl, mgp=mgp,
                       cex.axis=cex.axis, family=family, axes=F, ...)
    if (axes) {
      x_axis(family=family, cex.axis=cex.axis, mark=xmark, mark.label = xmark.label)
    }

    # Bins separation lines. The separation disapears at high bin count.
    nBreak = length(h$breaks)
    if (nBreak < 150) {
      abline(v = h$breaks,
             col = rgb(1,1,1,2*exp(-nBreak/50)/(1+exp(-nBreak/50))),
             lwd=0.5)
    }

    if (showmean) {
      q = quantile(x, probs=c(0.05, .35, .65, 0.95))
      rug(mean(x), lwd=1.5, lend=1)
    }
  }
