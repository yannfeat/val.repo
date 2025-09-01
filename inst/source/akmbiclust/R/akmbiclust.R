#' Alternating k-means biclustering
#'
#' This function uses the alternating k-means biclustering algorithm to
#' extract the k biclusters in the matrix X. See the paper
#' "Biclustering with Alternating K-Means" for more details.
#'
#'
#' @param X Data matrix.
#' @param k The number of biclusters.
#' @param lambda Regularization parameter. Default is 0.
#' @param nstart The number of random initializations. Default is 1.
#'
#' @return A list containing three objects:
#' \item{row_labels}{The bicluster labels of every row.}
#' \item{col_labels}{The bicluster labels of every column.}
#' \item{loss}{The loss of the produced biclusters.}
#'
#' @export
#'
#' @author Nicolas Fraiman and Zichao Li
#'
#' @references N. Fraiman and Z. Li (2020). Biclustering with Alternating K-Means.
#' arXiv preprint arXiv:2009.04550.
#'
#' @examples
#' # we create a 100 by 100 matrix X which has an underlying 2 by 2 block structure.
#' # The entries in the two 50 by 50 blocks on the top left and bottom right follow
#' # i.i.d. normal with mean 0 and variance 4. The entries in the two 50 by 50 blocks
#' # on the top right and bottom left follow i.i.d. normal with mean 0 and variance 1.
#'
#' X <- matrix(rnorm(10000, 0, 1), 100, 100)
#' X[1:50, 1:50] <- matrix(rnorm(2500, 0, 2), 50, 50)
#' X[51:100, 51:100] <- matrix(rnorm(2500, 0, 2), 50, 50)
#'
#' # Alternating k-means biclustering
#' # Result: perfect
#' result <- akmbiclust(X, 2, lambda = 0, nstart = 100)
#' result$row_labels
#' result$col_labels
#'
#' # Separate k-means clustering on the rows and columns
#' # Result: random
#' kmeans(X, 2)$cluster
#' kmeans(t(X), 2)$cluster

akmbiclust <- function(X, k, lambda = 0, nstart = 1) {
    n <- nrow(X)
    m <- ncol(X)
    best_row_labels <- rep(0, n)
    best_col_labels <- rep(0, m)
    min_loss <- Inf
    X0 <- X

    for (t in 1:nstart) {
        flag <- 0
        row_perm <- sample.int(n)
        col_perm <- sample.int(m)
        row_perm_reverse <- sort(row_perm, index.return = TRUE)$ix
        col_perm_reverse <- sort(col_perm, index.return = TRUE)$ix
        X <- X0[row_perm, col_perm]

        row_labels <- stats::kmeans(X, k, iter.max = 20)$cluster
        col_labels <- stats::kmeans(t(X), k, iter.max = 20)$cluster
        row_centers = vector(mode = "list", length = k)
        col_centers = vector(mode = "list", length = k)
        for (i in 1:k) {
            row_centers[[i]] <- rowMeans(X[row_labels == i, col_labels == i, drop = FALSE])
            col_centers[[i]] <- colMeans(X[row_labels == i, col_labels == i, drop = FALSE])
        }

        loss <- compute_loss(X, row_labels, col_labels, lambda)$loss
        if (loss < min_loss) {
            min_loss <- loss
            best_row_labels <- row_labels[row_perm_reverse]
            best_col_labels <- col_labels[col_perm_reverse]
        }

        total_iter <- 0
        while (total_iter < 50 && flag != 1) {
            total_iter <- total_iter + 1

            col_iter <- 0
            col_delta <- 1
            while (col_iter < 50 && col_delta > 0.001) {
                col_iter <- col_iter + 1
                col_centers_vec <- centers2vec(col_centers, col_labels)
                distance_matrix <- Inf * matrix(1, n, k)
                X_mean <- matrix(rep(col_centers_vec, n), nrow = n, byrow = TRUE)
                X_temp <- (X - X_mean)^2
                for (i in 1:k) {
                    distance_matrix[, i] <- rowSums(X_temp[, col_labels == i, drop = FALSE])/
                        sum(col_labels == i)
                }
                row_labels <- max.col(-distance_matrix)
                for (i in 1:k) {
                    if (!is.element(i, row_labels)) {
                        flag <- 1
                    }
                }
                if (flag == 1) {
                    break
                }
                new_col_centers <- vector(mode = "list", length = k)
                for (i in 1:k) {
                    X_sub <- X[row_labels == i, col_labels == i, drop = FALSE]
                    new_col_centers[[i]] <- colMeans(X_sub)
                }
                col_delta <- dist_centers(col_centers, new_col_centers)/
                    norm_centers(col_centers)
                col_centers <- new_col_centers
            }
            if (flag == 1) {
                break
            }
            for (i in 1:k) {
                row_centers[[i]] = rowMeans(X[row_labels == i, col_labels == i, drop = FALSE])
            }

            X_tran <- t(X)
            row_iter <- 0
            row_delta <- 1
            while (row_iter < 50 && row_delta > 0.001) {
                row_iter <- row_iter + 1
                row_centers_vec <- centers2vec(row_centers, row_labels)
                distance_matrix <- Inf * matrix(1, m, k)
                X_mean <- matrix(rep(row_centers_vec, m), nrow = m, byrow = TRUE)
                X_temp <- (X_tran - X_mean)^2
                for (i in 1:k) {
                    distance_matrix[, i] <- rowSums(X_temp[, row_labels == i, drop = FALSE])/
                        sum(row_labels == i)
                }
                col_labels <- max.col(-distance_matrix)
                for (i in 1:k) {
                    if (!is.element(i, col_labels)) {
                        flag <- 1
                    }
                }
                if (flag == 1) {
                    break
                }
                new_row_centers <- vector(mode = "list", length = k)
                for (i in 1:k) {
                    X_sub <- X_tran[col_labels == i, row_labels == i, drop = FALSE]
                    new_row_centers[[i]] <- colMeans(X_sub)
                }
                row_delta <- dist_centers(row_centers, new_row_centers)/
                    norm_centers(row_centers)
                row_centers <- new_row_centers
            }
            if (flag == 1) {
                break
            }
            for (i in 1:k) {
                col_centers[[i]] = colMeans(X[row_labels == i, col_labels == i, drop = FALSE])
            }

            new_loss <- compute_loss(X, row_labels, col_labels, lambda)$loss
            loss_delta <- abs(loss - new_loss)/loss
            loss <- new_loss
            if (loss_delta < 0.01) {
                flag <- 1
            }
        }

        if (loss < min_loss) {
            min_loss <- loss
            best_row_labels <- row_labels[row_perm_reverse]
            best_col_labels <- col_labels[col_perm_reverse]
        }
    }
    list(row_labels = best_row_labels, col_labels = best_col_labels, loss = min_loss)
}


