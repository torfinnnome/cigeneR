#' Reads GTFs
#'
#' Does what is says.
#' @param filename Path to gtf file.
#' @keywords GTF
#' @export
#' @examples
#' gtf_reader()
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
#' @examples gtf_writer()
gtf_writer <- function(x, filename) {
	write.table(x, file = filename, row.names = F, col.names = F, quote = F, sep = "\t")
}

#' Removes unstranded from gtf
#'
#' Does what is says.
#' @param x GTF object.
#' @keywords GTF
#' @export
#' @examples
#' gtf_reader()

remove_unstranded <- function(x) {
	dplyr::filter(x, V7 != ".")
}

#' Update id in plink ped
#'
#' Update Ids in plink ped file based on two column data frame
#' @param x Character vector of lines from ped
#' @param List two column text file with pattern and replacement
#' @keywords plink
#' @export
#' @examples
#' replace_id_str()

replace_id_str <- function(x, list){
	assertthat::assert_that(ncol(id_changes_fattyacid) == 2)
	names(list) <- c("V1", "V2")
	pattern = paste("\\b", list$V1, "\\b", sep = "") # only 1606 and not eg. 01606 is replaced.
	replacement = list$V2
	stringi::stri_replace_all(
		str = x, replacement = replacement, regex = pattern, vectorize_all = F)
}
