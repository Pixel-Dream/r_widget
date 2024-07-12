#' This function is used to convert 
#' tasks and doesn't support "simplify" and "USE.NAMES" options
#'
#' @param file_path h5ad file path
#' @param export_name tsv file name
#' @param layer_name expression layer in h5ad file to be export
#' @param save_path tsv file path
#' 
#' @return NULL
#'
#' @import tidyr
#' @import dplyr
#' @import Matrix
#' @import hdf5r
#' 
#' @export
#'
#' @examples
#' TBD
h5ad_export_expr <- function(file_path, export_name, layer_name = "log2_norm1e4", save_path){
    file.h5 <- H5File$new(file_path, mode="r")

    expr_mat <- sparseMatrix(i = file.h5[[paste0("layers/",layer_name,"/indices")]][] + 1,  
                             p = file.h5[[paste0("layers/",layer_name,"/indptr")]][],
                             x = file.h5[[paste0("layers/",layer_name,"/data")]][],
                             dims = c(file.h5[["X"]]$dims[1], file.h5[["X"]]$dims[2]))
    if("cell_barcode" %in% list.datasets(file.h5[["obs"]])){
        dimnames(expr_mat) <- list(file.h5[["var/_index"]][],paste0("cell_",file.h5[["obs/cell_barcode"]][]))
    }else if("_index" %in% list.datasets(file.h5[["obs"]])){
        dimnames(expr_mat) <- list(file.h5[["var/_index"]][],paste0("cell_",file.h5[["obs/_index"]][]))
    }else stop("No vaild cell index")
    
    expr_mat <- as(expr_mat, "TsparseMatrix")
    if (!dir.exists(save_path)) {dir.create(save_path)}
    # export gene and cell index
    write.table(data.frame(ind=1:length(expr_mat@Dimnames[[1]])-1,gene=expr_mat@Dimnames[[1]]),
                file.path(save_path,paste0(export_name,"_gene")), row.names = F, quote = FALSE, sep = "\t")
    write.table(data.frame(ind=1:length(expr_mat@Dimnames[[2]])-1,cell_id=expr_mat@Dimnames[[2]]),
                file.path(save_path,paste0(export_name,"_cell")), row.names = F, quote = FALSE, sep = "\t")
    # export main matrix
    write.table(data.frame(ind=1:length(expr_mat@i)-1, i=expr_mat@i,j=expr_mat@j,x=expr_mat@x),
                file.path(save_path,export_name), row.names = F, quote = FALSE, sep = "\t")
}