#' Pretty replacement to plot()
#'
#' @param x coordinates for the plot.
#' @param y y-axis coordinates for the plot (optional).
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param alpha opacity of the plot's points or lines.
#' @param cmap color map to use.
#' @param col colors.
#' @param pch Point style
#' @param cex.axis Scale of the plot axes.
#' @param cex Scale of the plot features.
#' @param family Font family.
#' @param xmark Optional mark on the x-axis.
#' @param xmark.label Optional mark label on the x-axis.
#' @param ymark Optional mark on the y-axis.
#' @param ymark.label Optional mark label on the y-axis.
#' @param axes whether or not to show axes.
#'
#' @export
plot <- function(x, y=NULL, xlab = NULL, ylab = NULL,
           alpha = .6,
           cmap = cmap.seaborn,
           col = 1,                # Color.
           pch = 20,               # Point style.
           cex.axis = .8,          # Labels scaling.
           cex = 1,
           family="serif",
           xmark = NULL,
           xmark.label = NULL,
           ymark = NULL,
           ymark.label = NULL,
           axes = TRUE,
           ...) {

    # Defaults
    bty = "n"              # Box type.
    tcl = -0.15            # Ticks length.
    mgp = c(2,0.25,0)      # Labels positioning.

    # Adjusting colors.
    if(!anyNA(suppressWarnings(as.numeric(col))) && all.equal(as.numeric(col), as.integer(col))) {
      col <- find_color(col, cmap)
    }
    col <- adjustcolor(col, alpha.f = alpha)

    # Figuring out the labels. In simple cases automatic labelling is used. Otherwise labels are empty by default.
    if (missing(y)) {
      if (missing(xlab) && missing(ylab)) {
        if(is.data.frame(x)){
          graphics::plot(x, col=col, pch=pch, bty=bty, tcl=tcl, mgp=mgp,
                         cex.axis=cex.axis, cex=cex,
                         family=family, axes=FALSE, ...);
        }
        else {
          graphics::plot(x, col=col, pch=pch, bty=bty, tcl=tcl, mgp=mgp,
                         cex.axis=cex.axis, cex=cex,
                         family=family, axes=FALSE, ...);
        }
      }
      else {
        xlab <- if(missing(xlab)) "" else xlab
        ylab <- if(missing(ylab)) "" else ylab
        graphics::plot(x, xlab=xlab, ylab=ylab, col=col, pch=pch, bty=bty, tcl=tcl,
                       mgp=mgp, cex.axis=cex.axis, cex=cex,
                       family=family, axes=FALSE, ...);
      }
    }
    else {
      xlab <- if (missing(xlab)) deparse(substitute(x)) else xlab;
      ylab <- if (missing(ylab)) deparse(substitute(y)) else ylab;
      graphics::plot(x, y, xlab=xlab, ylab=ylab, col=col, pch=pch, bty=bty,
                     tcl=tcl, mgp=mgp, cex.axis=cex.axis, cex=cex,
                     family=family, axes=FALSE, ...);
    }

    if (axes) {
      x_axis(family=family, mark=xmark, mark.label = xmark.label,
             cex.axis=cex.axis)
      y_axis(family=family, mark=ymark, mark.label=ymark.label,
             cex.axis=cex.axis)
    }
  }

find_color <- function (col, cmap) {
  if (!anyNA(suppressWarnings(as.numeric(col)))
      && all.equal(as.numeric(col), as.integer(col))) {
    return(cmap(col))
  }
  return(col)
}
