#' Plot Confusion Matrix
#'
#' Draw Confusion Matrix using ComplexHeatmap
#' @param x,y vector with discrete values
#' @param col_title Heatmap title
#'
#' @return A Heatmap-class object.
#'
#' @import ComplexHeatmap
#' @import circlize
#'
#' @export
#'
#' @examples
#' x_ <- sample(1:4,100,replace = T)
#' y_ <- sample(1:5,100,replace = T)
#' plot_confusion_matrix(x_,y_,col_title = "Confusion Matrix")
#'
plot_confusion_matrix <- function(x,y,col_title = ""){
  na_index <- (is.na(x) | is.na(y))
  x <- x[!na_index]
  y <- y[!na_index]
  u_x <- ifelse(is.null(levels(x)), unique(x), levels(x))
  u_y <- ifelse(is.null(levels(y)), unique(y), levels(y))
  hm_mat <- matrix(0, nrow = length(u_x), ncol = length(u_y))
  row.names(hm_mat) <- u_x
  colnames(hm_mat) <- u_y
  for(i in seq_len(length(u_x))){
    for(j in seq_len(length(u_y))){
      hm_mat[i,j] = sum(x==u_x[i] & y ==u_y[j])
    }
  }
  hm_mat <- hm_mat/matrix(rep(rowSums(hm_mat),ncol(hm_mat)),
                          byrow = F,nrow = nrow(hm_mat), ncol = ncol(hm_mat))
  Heatmap(hm_mat,cluster_columns = F, cluster_rows = F,
          col = colorRamp2(seq(0, 1,length.out=5), viridis::viridis(5)),
          rect_gp = gpar(col = "white", lwd = 1),column_title = col_title)
}