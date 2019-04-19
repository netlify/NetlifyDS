#' Cluster analysis
#' @description Does cluster analysis and report an index of cluster
#' @param ans data matrix
#' @param  k number of clusters
#' @param distm  Dissimilarity index, partial match to "\code{manhattan}", "\code{euclidean}"(default), "\code{canberra}", "\code{bray}", "\code{kulczynski}", "\code{jaccard}", "\code{gower}", "\code{altGower}", "\code{morisita}", "\code{horn}", "\code{mountford}", "\code{raup}" , "\code{binomial}", "\code{chao}", "\code{cao}" or "\code{mahalanobis}".
#' @param hclustm the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "\code{ward.D}"(default), "\code{ward.D2}", "\code{single}", "\code{complete}", "\code{average}" (= UPGMA), "\code{mcquitty}" (= WPGMA), "\code{median}" (= WPGMC) or "\code{centroid}" (= UPGMC).
#' @param na.rm	Pairwise deletion of missing observations when computing dissimilarities.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' data("SegData")
#' library(dplyr)
#' library(vegan)
#' cind <- clust_ana(dat, k=4)
#' }

#' @export
clust_ana <- function(ans, k, distm = "euclidean", hclustm = "ward.D",
                      na.rm = FALSE) {
  library(vegan)
  dist_t <- vegdist(ans, method = distm, na.rm = na.rm)
  clust <- hclust(dist_t, method = hclustm)
  plot(clust, main = "Cluster Dendrogram of Segment Survey")
  return(cutree(clust, k = k))
}

