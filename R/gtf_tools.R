library(devtools)
#' Reads GTFs
#'
#' Does what is says.
#' @param filename Path to gtf file.
#' @keywords GTF
#' @export
gtf_reader <- function(filename) {
	dplyr::tbl_df(data.table::fread(filename, data.table = F))
}

#' Writes GTFs
#'
#' Does what is says.
#' @param x GTF object.
#' @param filename new gtf filename.
#' @keywords GTF
#' @export
gtf_writer <- function(x, filename) {
	write.table(x, file = filename, row.names = F, col.names = F, quote = F, sep = "\t")
}

#' Removes unstranded from gtf read into R.
#'
#' Assumes stranded info is in column 7.
#' @param x GTF object.
#' @keywords GTF
#' @export
#' @examples
#' remove_unstranded(mtcars)

remove_unstranded <- function(x) {
	col7 <- colnames(x)[7]
	dplyr::filter(x, col7 != ".")
}

#' Update id in vector of IDs, eg. plink fam.
#'
#' Update Ids in dataframe column based on two column data frame
#' @param x Character vector of IDs to update.
#' @param List two column dataframe with pattern and replacement
#' @keywords plink
#' @export
#' @examples
#'
#' replace_id_str(x)

replace_id_str <- function(x, list){
	assertthat::assert_that(ncol(list) == 2)
	names(list) <- c("V1", "V2")
	pattern = paste("\\b", list$V1, "\\b", sep = "") # only 1606 and not eg. 01606 is replaced.
	replacement = list$V2
		stringi::stri_replace_all(
		str = x, replacement = replacement, regex = pattern, vectorize_all = F)
}
