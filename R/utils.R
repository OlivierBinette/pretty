#' @export
axelines <- function(x, y, col=1, minx=-2e300, miny=-2e300, alpha=1, cmap=cmap.seaborn) {
  col = find_color(col, cmap)
  col = adjustcolor(col, alpha.f = alpha)
  points(x, y, col=col, pch=20)
  lines(c(x,x), c(miny,y), col=col, lty=2)
  lines(c(minx,x), c(y,y), col=col, lty=2)
}

#' @export
hline <- function(y, col=1, lty=2, alpha=1, cmap=cmap.seaborn) {
  col = find_color(col, cmap)
  col = adjustcolor(col, alpha.f = alpha)
  abline(h=y, col=col, lty=lty)
}

#' @export
vline <- function(x, col=1, lty=2, alpha=1, cmap=cmap.seaborn) {
  col = find_color(col, cmap)
  col = adjustcolor(col, alpha.f = alpha)
  abline(v=x, col=col, lty=lty)
}

#' @export
lines <-
  function(x, y,
           alpha = .6,
           cmap = cmap.seaborn,
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

#' @export
points <-
  function(x, y,
           alpha = .6,
           cmap = cmap.seaborn,
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
