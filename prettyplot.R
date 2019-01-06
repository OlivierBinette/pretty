#
# Work in progress.
#

prettyplot.DEFAULT_CMAP = cmap.knitr

plot <-
  function(x, y, xlab = NULL, ylab = NULL,
           alpha = .6,
           cmap = prettyplot.DEFAULT_CMAP,
           ptscale = NULL,
           col = 1,                # Color.
           pch = 20,               # Point style.
           bty = "n",              # Box type.
           tcl = -0.15,            # Ticks length.
           mgp = c(2,0.25,0),      # Labels positioning.
           cex.axis = .8,          # Labels scaling.
           cex = 1,
           family="serif",
           xmark = NULL,
           xmark.label = NULL,
           ymark = NULL,
           ymark.label = NULL,
           axes = T,
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

lines <-
  function(x, y,
           alpha = .6,
           cmap = prettyplot.DEFAULT_CMAP,
           col = 1,                # Color
           ...) {

    # Adjusting colors.
    if(!anyNA(suppressWarnings(as.numeric(col))) && all.equal(as.numeric(col), as.integer(col))) {
      col <- find_color(col, cmap)
    }
    col <- adjustcolor(col, alpha.f = alpha)

    if (missing(y)) graphics::lines(x, col=col, ...)
    else graphics::lines(x, y, col=col, ...)
  }

points <-
  function(x, y,
           alpha = .6,
           cmap = prettyplot.DEFAULT_CMAP,
           col = 1,                # Color
           pch = 20,               # Point style.
           ...) {

    # Adjusting colors.
    if(!anyNA(suppressWarnings(as.numeric(col))) && all.equal(as.numeric(col), as.integer(col))) {
      col <- find_color(col, cmap)
    }
    col <- adjustcolor(col, alpha.f = alpha)

    if (missing(y)) graphics::points(x, col=col, pch=pch, ...)
    else graphics::points(x, y, col=col, pch=pch, ...)
  }


boxplot <-
  function(x, ...,
           xticks = NULL,
           xticks_labels = NULL,
           color = NULL,
           color.mid = NULL,
           color.out = NULL,
           color.line = NULL,
           cmap = prettyplot.DEFAULT_CMAP,
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
           tufte.style=F,
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
      x_axis(family=family, cex.axis=cex.axis)
    }

    # Bins separation lines. The separation disapears at high bin count.
    nBreak = length(h$breaks)
    if (nBreak < 150) {
      abline(v = h$breaks,
             col = rgb(1,1,1,2*exp(-nBreak/50)/(1+exp(-nBreak/50))),
             lwd=0.5)
    }

    if (tufte.style) {
      q = quantile(x, probs=c(0.05, .35, .65, 0.95))
      rug(mean(x), lwd=1.5, lend=1)
      lines(c(q[1],q[2]), c(-.0125, -.0125), col=0, alpha=1, lwd=1.5, lend=1)
      lines(c(q[3],q[4]), c(-.0125, -.0125), col=0, alpha=1, lwd=1.5, lend=1)

    }
  }

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

cor.im <- function(df, order="", size=1) {

  # Ordering variables
  if (order == "hclust") {
    df = df[, order.dendrogram(as.dendrogram(hclust(dist(cor(df)))))]
  }
  else if (order == "magnitude") {
    df = df[, order(rowSums(abs(cor(df))))]
  }

  n = length(df)
  image(x = 1:n, y = 1:n, z = cor(df)[,n:1], zlim = c(-1,1),
        xlab = "", ylab = "",
        col = colorRampPalette(
          colors = c(cmap.knitr(2), "white", cmap.knitr(1))
        )(256),
        axes = F
  )
  m = cor(df); diag(m) = NA; m = m[!is.na(m)]
  text(x = rep(1:n, times=n)[-((0:(n-1))*n+1:n)],
       y = rep(n:1, each=n-1), labels = formatC(m, 2), cex = .7*size,
       col = cmap.knitr(0))
  text(x = 1:n, y=n:1, labels=names(df), col="white", cex = .9*size)
}


mat.im <- function(df, mat, order="", size=1) {

  # Ordering variables
  if (order == "hclust") {
    df = df[, order.dendrogram(as.dendrogram(hclust(dist(cor(df)))))]
  }
  else if (order == "magnitude") {
    df = df[, order(rowSums(abs(cor(df))))]
  }

  n = length(df)
  image(x = 1:n, y = 1:n, z = mat[,n:1], zlim = c(-1,1),
        xlab = "", ylab = "",
        col = colorRampPalette(
          colors = c(cmap.knitr(2), "white", cmap.knitr(1))
        )(256),
        axes = F
  )
  m = cor(df); diag(m) = NA; m = m[!is.na(m)]
  text(x = rep(1:n, times=n)[-((0:(n-1))*n+1:n)],
       y = rep(n:1, each=n-1), labels = formatC(m, 2), cex = .7*size,
       col = cmap.knitr(0))
  text(x = 1:n, y=n:1, labels=names(df), col="white", cex = .9*size)
}

find_color <- function (col, cmap) {
  if (!anyNA(suppressWarnings(as.numeric(col)))
      && all.equal(as.numeric(col), as.integer(col))) {
    return(cmap(col))
  }
  return(col)
}

axelines <- function(x, y, col=1, minx=0, miny=0) {
  col = find_color(col, cmap.knitr)
  points(x, y, col=col, pch=20)
  lines(c(x,x), c(miny,y), col=col, lty=2)
  lines(c(minx,x), c(y,y), col=col, lty=2)
}

x_axis <- function(lwd = .5, mark=NULL, mark.label=NULL, ...) {
  if (!is.null(mark)) {
    xaxp = par("xaxp")
    pos = seq(xaxp[[1]], xaxp[[2]], length.out=xaxp[[3]]+1)
    i = which.min(abs(mark - pos))
    pos[[i]] = mark
    labels = pos
    labels[[i]] = if (is.null(mark.label)) formatC(mark) else mark.label
    axis(1, at = pos, labels = labels, tcl = -0.15, mgp = c(0,0.25,0),
         las=1, lwd=lwd, ...)
  }
  else {
    axis(1, tcl = -0.15, mgp = c(0,0.25,0),
         las=1, lwd=lwd, ...)
  }
}

y_axis <- function(lwd = .5, mark=NULL, mark.label=NULL, ...) {
  if (!is.null(mark)) {
    yaxp = par("yaxp")
    pos = seq(yaxp[[1]], yaxp[[2]], length.out=yaxp[[3]]+1)
    i = which.min(abs(mark - pos))
    pos[[i]] = mark
    labels = pos
    labels[[i]] = if (is.null(mark.label)) formatC(mark) else mark.label
    axis(2, at = pos, labels = labels, tcl = -0.15, mgp = c(0,0.5,0),
         las=1, lwd=lwd, ...)
  }
  else {
    axis(2, tcl = -0.15, mgp = c(0,0.5,0),
         las=1, lwd=lwd, ...)
  }
}


cmap.seaborn <- function(n) {
  colors.seaborn <-
    c(rgb(0.12156862745098039, 0.4666666666666667, 0.7058823529411765),
      rgb(1.0, 0.4980392156862745, 0.054901960784313725),
      rgb(0.17254901960784313, 0.6274509803921569, 0.17254901960784313),
      rgb(0.8392156862745098, 0.15294117647058825, 0.1568627450980392),
      rgb(0.5803921568627451, 0.403921568627451, 0.7411764705882353),
      rgb(0.5490196078431373, 0.33725490196078434, 0.29411764705882354),
      rgb(0.8901960784313725, 0.4666666666666667, 0.7607843137254902),
      rgb(0.4980392156862745, 0.4980392156862745, 0.4980392156862745),
      rgb(0.7372549019607844, 0.7411764705882353, 0.13333333333333333),
      rgb(0.09019607843137255, 0.7450980392156863, 0.8117647058823529))
  
  n <- as.numeric(n); indices <- ((n-1) %% 9) + 1
  colors <- c()
  colors[n > 0] <- colors.seaborn[indices[n > 0]]
  colors[n == 0] <- rgb(0,0,0)
  return(colors)
}

cmap.knitr <- function(n) {
  colors.knitr <- c(rgb(0.161,0.373,0.58),
                    rgb(0.69,0.353,0.396),
                    rgb(0.333,0.667,0.333),
                    rgb(0.686,0.059,0.569),
                    rgb(0.678,0.584,0.686),
                    rgb(0.192,0.494,0.8),
                    rgb(0.333,0.667,0.333),
                    rgb(0.737,0.353,0.396)
  )
  n <- as.numeric(n); indices <- ((n-1) %% 7) + 1
  colors <- c()
  colors[n > 0] <- colors.knitr[indices[n > 0]]
  colors[n == 0] <- rgb(0.345,0.345,0.345)
  return(colors)
}

