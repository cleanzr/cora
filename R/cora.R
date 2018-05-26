#' CORA data set
#'
#' This provides a record linkage data set with information about different CORA research papers.
#'
#' @format A data frame with 16 variables: \code{id}, \code{title}, \code{book_title}, \code{authors}, \code{address}, \code{date}, \code{year}, \code{editor},\code{journal}, \code{volume}, \code{pages}, \code{publisher}, \code{institution}, \code{type}, \code{tech}, \code{note}.
#' 
#' This data set includes 1879 CORA research papers.
#' It is appropriate for performing various types of record linkage and can be assessed by standard record linkage methods. 
#'
#'@examples
#'head(cora)
#'dim(cora)
#'
"cora"

#' Cora Gold
#'
#' This data set includes the matched record pairs based on ID.
#'
#' @format A data frame with 2 variables: \code{id1}, \code{id2}
#' 
#' This data set includes the matched record pairs based on ID from the CORA data set.
#' This data set can be used to evaluate the performance of record linkage methods performed on the CORA data set. 
#'
#'@examples
#'head(cora_gold)
#'dim(cora_gold)
#'
"cora_gold"

#' Cora Gold Update
#'
#' This data set includes the matched record pairs based on ID.
#'
#' @format A data frame with 2 variables: \code{cora_id}, \code{unique_id}
#' 
#' This data set includes the matched record pairs based on ID from the CORA data set.
#' This data set can be used to evaluate the performance of record linkage methods performed on the CORA data set. 
#'
#'@examples
#'head(cora_gold_update)
#'dim(cora_gold_update)
#'
"cora_gold_update"
