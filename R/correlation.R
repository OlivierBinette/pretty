
#' @export
correlation <- function(df, order="", size=1) {

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
  text(x = 1:n, y=n:1, labels=names(df), col="white", cex = .8*size)
}

#' @export
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
