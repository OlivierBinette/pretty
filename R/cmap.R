#' Seaborn color map
#'
#' 10 distinct colors from https://seaborn.pydata.org/.
#'
#' @usage cmap.seaborn(n)
#'
#' @param n integer vector of color indices.
#' @return RGB colors.
#'
#' @export
cmap.seaborn <- function(n) {
  system.file(package="pretty")
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

#' Knitr color map
#'
#' 8 distinct colors compatible with knitr's default colors.
#'
#' @usage cmap.knitr(n)
#'
#' @param n integer vector of color indices.
#' @return RGB colors.
#'
#' @export
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
