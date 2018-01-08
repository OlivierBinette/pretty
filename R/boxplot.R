boxplot <-
  function(x, ...,
           xticks = NULL,
           xticks_labels = NULL,
           color = NULL,
           color.mid = NULL,
           color.out = NULL,
           color.line = NULL,
           cmap = cmap.seaborn,
           scale.mid = 0,
           scale.out = 0,
           linewidth = 1,
           pars = NULL,
           axes = T,
           family="serif",
           main = NULL,
           sub = NULL,
           xlab = NULL,
           ylab = NULL,
           cex = 1,
           cex.axis = .8
          ) {

  # Default box style.
  box_style = list(boxcol = rgb(1,1,1,0),
                   boxfill = rgb(1,1,1,0),
                   medlty = "blank",
                   medpch = 20,
                   medcex = .7,
                   whisklty = c(1, 1),
                   staplelty = "blank",
                   outcex = .15,
                   whisklwd = 1,
                   outpch = 5,
                   boxwex = 0.1)

  # Adjusting colors.
  if(!missing(color)) {
      box_style$medcol <- find_color(color, cmap)
      box_style$outcol <- find_color(color, cmap)
      box_style$whiskcol <- find_color(color, cmap)
  }
  if(!missing(color.mid)) {
    box_style$medcol <- find_color(color.mid, cmap)
  }
  if(!missing(color.out)) {
    box_style$outcol <- find_color(color.out, cmap)
  }
  if(!missing(color.line)) {
    box_style$whiskcol <- find_color(color.line, cmap)
  }

  # Adjusting sizes.
  if (!missing(scale.mid)) {
    box_style$medcex <- box_style$medcex*exp(scale.mid/10)
  }
  if (!missing(scale.out)) {
    box_style$outcex <- box_style$outcex * exp(scale.out/10)
  }
  if(!missing(linewidth)) {
    box_style$whisklwd <- linewidth
  }


  if (!missing(pars)) {
    box_style[names(pars)] <- pars
  }

  graphics::boxplot(x, ..., pars=box_style, tcl = -0.15, axes=F)

  # Axes placement.
  if (axes) {
    x_axis(family=family, lwd=-1, at=xticks, labels=xticks_labels, cex.axis=cex.axis)
    y_axis(family=family, cex.axis=cex.axis)
  }

  # Titles placement.
  title(main=main, sub=sub, xlab=xlab, ylab=ylab, family=family, line=2)
}
