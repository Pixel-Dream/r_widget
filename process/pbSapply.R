#' This function is similar to "parSapply", but doesn't preschedule
#' tasks and doesn't support "simplify" and "USE.NAMES" options
#'
#' @param cl 
#' @param X 
#' @param FUN function
#' 
#' @return data frame with columns "Gene", "base_seq" and "ch_seq" (optional)
#'
#' @import tidyr
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' TBD
pbSapply <- function(cl, X, FUN, ...) {
  require(doSNOW)
  registerDoSNOW(cl)
  pb <- txtProgressBar(max=length(X))
  on.exit(close(pb))
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  foreach(i=X, .combine='rbind', .options.snow=opts) %dopar% {
    FUN(i, ...)
  }
}