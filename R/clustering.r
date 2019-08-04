##' @title
##' Clustering Metrics Parameters
##'
##' @description
##' Documentation for shared parameters of functions that compute clustering
##' metrics.
##'
##' @param actual \code{[numeric]} The ground truth numeric vector.
##' @param predicted \code{[numeric]} The predicted numeric vector, where each
##'     element in the vector is a prediction of the corresponding elements in
##'     \code{actual}.
##' @name clustering_params
##' @include helper-functions.r
NULL


##' @title
##' Adjusted Mutual Information Score / Mututal Information Score
##'
##'
##' @description
##'
##' \code{mtr_mutual_info_score} measures the similarity, or mutual dependence 
##' between two variable. The worst possible score is 0, higher values are 
##' better.
##' 
##' 
##' @inheritParams clustering_params
##' @importFrom stats var
##' @seealso \code{\link{mtr_adjusted_rand_score}}
##' @return A numeric scalar output
##' @author Phuc Nguyen
##' @examples
##'
##' act <- sample(1:10, 100, replace = T)
##' pred <- sample(1:10, 100, replace = T)
##' mtr_mutual_info_score(act, pred)
##'
##' act <- rep(c('a', 'b', 'c'), times = 4)
##' pred <- rep(c('a', 'b', 'c'), each = 4)
##' mtr_mutual_info_score(act, pred)
##'
##' @export
mtr_mutual_info_score <- function(actual, predicted) {
    chec_empty_vec(actual)
    check_equal_length(actual, predicted)
    entropy(actual) + entropy(predicted) - joint_entropy(vec_1 = actual, 
                                                         vec_2 = predicted)
}

mtr_normalized_mutual_info_score <- function(actual, predicted) {
    mtr_mutual_info_score(actual = actual, predicted = predicted) / 
        mean(c(entropy(vec = actual), entropy(vec = predicted)))
}

mtr_adjusted_mutual_info_score <- function(actual, predicted) {
    (mtr_mutual_info_score(actual, predicted) - expected_mutual_info(actual, predicted)) / 
        (mean(c(entropy(actual), entropy(predicted))) - expected_mutual_info(actual, predicted))
}

##' @title
##' Adjusted Rand Score
##'
##'
##' @description
##'
##' \code{mtr_adjusted_rand_score} measures the similarity, or mutual dependence 
##' between two variable. Perfect score is 1. Score between total random vectors
##' is close to 0. Score can be negative.
##' 
##' 
##' @inheritParams clustering_params
##' @importFrom base choose
##' @seealso \code{\link{mtr_mutual_info_score}}
##' @return A numeric scalar output
##' @author Phuc Nguyen
##' @examples
##'
##' act <- sample(1:10, 100, replace = T)
##' pred <- sample(1:10, 100, replace = T)
##' mtr_adjusted_rand_score(act, pred)
##'
##' act <- rep(c('a', 'b', 'c'), times = 4)
##' pred <- rep(c('a', 'b', 'c'), each = 4)
##' mtr_adjusted_rand_score(act, pred)
##'
##' @export

mtr_adjusted_rand_score <- function(actual, predicted) {
    check_equal_length(actual, predicted)
    N = length(actual)
    a = sum(choose(table(actual, predicted), 2))
    b = sum(choose(table(actual), 2)) * sum(choose(table(predicted), 2)) / choose(N, 2)
    c = 1/2 * (sum(choose(table(actual), 2)) + sum(choose(table(predicted), 2)))
    (a - b) / (c - b)
}

##' @title
##' Homogeneity, Completeness, V-measure
##'
##'
##' @description
##'
##' \code{mtr_homogeneity} and \code{mtr_completeness} measures an aspect of the 
##' quality of clustering algorithm, as the former measures how similar the 
##' elements within each cluster to each other, and the later measures 
##' the degree a cluster has cover elements of same labels. 
##' Both scores are in range of 0 to 1, as worst to best.
##' \code{mtr_v_measure} is harmonic mean of homogeneity score and completeness
##' score.
##' 
##' @inheritParams clustering_params
##' @return A numeric scalar output
##' @author Phuc Nguyen
##' @examples
##'
##' act <- sample(1:10, 100, replace = T)
##' pred <- sample(1:10, 100, replace = T)
##' mtr_homogeneity(act, pred)
##'
##' act <- sample(1:10, 100, replace = T)
##' pred <- sample(1:10, 100, replace = T)
##' mtr_completeness(act, pred)
##'
##' act <- sample(1:10, 100, replace = T)
##' pred <- sample(1:10, 100, replace = T)
##' mtr_v_measure(act, pred)
##'
##' @export

mtr_homogeneity <- function(actual, predicted) {
    1 - conditional_entropy(actual, predicted) / entropy(actual)
}

mtr_completeness <- function(actual, predicted) {
    1 - conditional_entropy(predicted, actual) / entropy(predicted)
}

mtr_v_measure <- function(actual, predicted) {
    h = mtr_homogeneity(actual, predicted)
    c = mtr_completeness(actual, predicted)
    2 * h * c / (h + c)
}

##' @title
##' Calinski-Harabasz Score (Variance Ratio Criterion)
##'
##'
##' @description
##'
##' \code{mtr_calinski_harabasz} measure the 'goodness' of clustering model 
##' output, in case ground truth is unknown. Higher score mean cluster are dense
##' and well separated.
##' 
##' @inheritParams clustering_params
##' @seealso \code{\link{mtr_davies_boulding_score}}
##' @return A numeric scalar output
##' @author Phuc Nguyen
##' @examples
##' dt <- iris[,-5]
##' pred <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
##' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
##' 1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
##' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
##' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2,
##' 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2,
##' 2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0)
##' mtr_calinski_harabasz(dt, pred)
##'
##' @export

mtr_calinski_harabasz <- function(matrix_feature, predicted) {
    dt_center = apply(matrix_feature, 2, FUN = mean)
    N = length(predicted)
    num_cluster = length(unique(predicted))
    
    check_number_of_labels(n_labels = num_cluster, n_samples = N)
    
    dispersion_between_cluster = 0
    dispersion_within_cluster = 0
    
    for (cluster_val in unique(predicted)) {
        size_cluster = length(which(predicted == cluster_val))
        dt_cluster = matrix_feature[which(predicted == cluster_val),]
        cluster_center = apply(dt_cluster, 2, FUN = mean)
        
        dispersion_between_cluster = dispersion_between_cluster + size_cluster * sum((t(cluster_center) - dt_center) ^ 2)
        dispersion_within_cluster = dispersion_within_cluster + sum((t(dt_cluster) - cluster_center) ^ 2)
    }
    
    (dispersion_between_cluster / dispersion_within_cluster) * ((N - num_cluster) / (num_cluster - 1))
}

##' @title
##' Davies-Bouldin Score
##'
##'
##' @description
##'
##' \code{mtr_davies_boulding_score} measure the 'goodness' of clustering model 
##' output, in case ground truth is unknown. Lowest and best score is 0,
##' and lower score mean clusters are dense and separated
##' 
##' @inheritParams clustering_params
##' @seealso \code{\link{mtr_calinski_harabasz}}
##' @return A numeric scalar output
##' @author Phuc Nguyen
##' @examples
##' dt <- iris[,-5]
##' pred <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
##' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
##' 1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
##' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
##' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2,
##' 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2,
##' 2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0)
##' mtr_calinski_harabasz(dt, pred)
##'
##' @export

mtr_davies_boulding_score <- function(matrix_feature, predicted) {
    check_equal_cluster_length(matrix_feature, predicted)
    similarity_sum = 0
    for (cluster_val in unique(predicted)) {
        dt_cluster = matrix_feature[which(predicted == cluster_val),]
        cluster_center = apply(dt_cluster, 2, FUN = mean)
        cluster_diameter = mean_distance(dt_cluster, cluster_center)
        max_similarity = 0
        
        for(other_cluster_val in unique(predicted)[unique(predicted) != cluster_val]) {
            dt_other_cluster = matrix_feature[which(predicted == other_cluster_val),]
            other_cluster_center = apply(dt_other_cluster, 2, FUN = mean)
            other_cluster_diameter = mean_distance(dt_other_cluster, other_cluster_center)
            between_distance = pairwise_distance(cluster_center, other_cluster_center)
            similarity = (cluster_diameter + other_cluster_diameter) / between_distance
            max_similarity = max(max_similarity, similarity)
        }
        similarity_sum = similarity_sum + max_similarity
    }
    
    similarity_sum/length(unique(predicted))
}

##' @title
##' Sihouette Coefficient
##'
##'
##' @description
##'
##' \code{mtr_sihouette_coef} measure the 'goodness' of clustering model 
##' output for each sample in the clustering model output, in case ground 
##' truth is unknown. Lowest and worst score is -1, while highest and best 
##' score is 1, with higher score means denser and better
##' separated clusters. 
##' 
##' The Silhouette Coefficient is defined for each sample and is composed of 
##' two scores:
##' a: The mean distance between a sample and all other points in the same class.
##' b: The mean distance between a sample and all other points in the next 
##' nearest cluster.
##' The Silhouette Coefficient s for a single sample is then given as:
##' \code{(b-a)/max(a,b)}
##'
##' @inheritParams clustering_params
##' @seealso \code{\link{mtr_davies_boulding_score}} 
##' @return A numeric vector output, length is number of all predictions.
##' @author Phuc Nguyen
##' @examples
##' dt <- iris[,-5]
##' pred <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
##' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
##' 1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
##' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
##' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2,
##' 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2,
##' 2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0)
##' mtr_sihouette_coef(dt, pred)
##'
##' @export

mtr_sihouette_coef <- function(matrix_feature, predicted) {
    # matrix_feature <- iris[,-5]
    # predicted <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    # 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    # 1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    # 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    # 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2,
    # 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2,
    # 2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0)
    
    check_equal_cluster_length(matrix_feature, predicted)
    li = c()
    
    for (i in seq_along(predicted)) {
        cluster_val = predicted[[i]]
        dt_sample = matrix_feature[i,]
        dt_cluster_exclude = matrix_feature[setdiff(which(predicted == cluster_val), i) ,]
        inner_dist = mean_distance(set = dt_cluster_exclude, pt = dt_sample)
        outer_dist_li = c()
        for(other_cluster_val in unique(predicted)[unique(predicted) != cluster_val]) {
            dt_other_cluster = matrix_feature[which(predicted == other_cluster_val),]
            outer_dist = mean_distance(set = dt_other_cluster, pt = dt_sample)
            outer_dist_li = c(outer_dist_li, outer_dist)
        }
        # this method can be further optimized by calculating distance matrix 
        # beforehand then using pointer to select out
        
        min_outer_dist = min(outer_dist_li)
        
        sample_coef = (min_outer_dist - inner_dist) / max(inner_dist, min_outer_dist)
        li = c(li, sample_coef)
    }
    li
}

##' @title
##' Mean Sihouette Coefficient
##'
##'@description
##'
##' \code{mtr_mean_sihouette_coef} is an average of Sihouette coefficient of all 
##' samples, representing general level of 'goodness' of model in one single score.
##' 
##' @inheritParams clustering_params
##' @seealso \code{\link{mtr_sihouette_coef}} 
##' @return A numeric scalar output
##' @author Phuc Nguyen
##' @examples
##' dt <- iris[,-5]
##' pred <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
##' 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
##' 1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
##' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
##' 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 2, 2, 2, 0, 2, 2, 2,
##' 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 2, 0, 2, 0, 2, 2, 0, 0, 2, 2, 2, 2,
##' 2, 0, 2, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 2, 0, 2, 2, 0)
##' mtr_mean_sihouette_coef(dt, pred)
##'
##' @export

mtr_mean_sihouette_coef <- function(matrix_feature, predicted) {
    check_equal_cluster_length(matrix_feature, predicted)
    mean(mtr_sihouette_coef(matrix_feature, predicted))
}

