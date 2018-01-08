#' @title Pretty plot
#'
#' @description Wrapper to R's \code{\link{graphics::plot}} function. Overrides
#'     default behavior for better plots.
#'
#'
#' @param x
#'     The coordinates of points of the plot. Alternatively, a single
#'     plotting structure, function or any R object with a plot method can
#'     be provided.
#'
#' @param y
#'     The y coordinates of points in the plot, optional if x is an
#'     appropriate structure.
#'
#' @param col
#'     A specification for the default plotting color. By default, a value
#'     of 0 represents black. Other positive integer values are mapped to
#'     colors by the default color map.
#'
#' @param alpha
#'     Adjusts the opacity of the color specified by the parameter \code{col}.
#'
#' @param cmap
#'     Map to be applied to the parameter \code{col}.
#'
#' @param ptscale
#'     Symbol size adjustment in log-scale (relatively to cex). Positive
#'     values represent an increased size, while negative value represent
#'     a decreased size.
#'
plot <-
  function(x, y, xlab = NULL, ylab = NULL,
           alpha = .6,
           cmap = cmap.seaborn,
           ptscale = NULL,
           col = 1,                # Color.
           pch = 20,               # Point style.
           bty = "n",              # Box type.
           tcl = -0.15,            # Ticks length.
           mgp = c(2,0.25,0),      # Labels positioning.
           cex.axis = .8,          # Labels scaling.
           cex = 1,
           axes = T,
           family="serif",
           ...) {

    # Adjusting colors.
    if(!anyNA(suppressWarnings(as.numeric(col))) && all.equal(as.numeric(col), as.integer(col))) {
      col <- find_color(col, cmap)
    }
    col <- adjustcolor(col, alpha.f = alpha)

    # Adjusting sizes
    if (!missing(ptscale)) {
      cex <- cex*exp(ptscale/10)
    }

    # Default box style.
    box_style = list(boxcol = rgb(1,1,1,0),
                     boxfill = rgb(1,1,1,0),
                     medlty = "blank",
                     medpch = 20,
                     medcex = .7,
                     whisklty = c(1, 1),
                     staplelty = "blank",
                     outcex = .33,
                     whisklwd = 1,
                     outpch = 5)

    # Figuring out the labels. In simple cases automatic labelling is used. Otherwise labels are empty by default.
    if (missing(y)) {
      if (missing(xlab) && missing(ylab)) {
        graphics::plot(x, col=col, pch=pch, bty=bty, tcl=tcl, mgp=mgp,
                       cex.axis=cex.axis, cex=cex, axes=F,
                       family=family, ...);
      }
      else {
        xlab <- if(missing(xlab)) "" else xlab
        ylab <- if(missing(ylab)) "" else ylab
        graphics::plot(x, xlab=xlab, ylab=ylab, col=col, pch=pch, bty=bty, tcl=tcl,
             mgp=mgp, cex.axis=cex.axis, cex=cex, axes=F,
             family=family, ...);
      }
    }
    else {
      xlab <- if (missing(xlab)) deparse(substitute(x)) else xlab;
      ylab <- if (missing(ylab)) deparse(substitute(y)) else ylab;
      graphics::plot(x, y, xlab=xlab, ylab=ylab, col=col, pch=pch, bty=bty,
                     tcl=tcl, mgp=mgp, cex.axis=cex.axis, cex=cex, axes=F,
                     family=family, ...);
    }

    # Axes placement.
    if (axes) {
      x_axis(family=family, cex.axis=cex.axis)
      y_axis(family=family, cex.axis=cex.axis)
    }
  }

