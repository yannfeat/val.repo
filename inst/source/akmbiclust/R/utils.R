compute_loss <- function(X, row_labels, col_labels, lambda = 0) {
    loss <- 0
    k <- max(row_labels)
    n <- nrow(X)
    c <- norm(X, type = "F")^2
    changes <- rep(Inf, k)

    for (i in 1:k) {
        X_sub <- X[row_labels == i, col_labels == i, drop = FALSE]
        nrows <- sum(row_labels == i)
        col_mean_sub <- matrix(rep(colMeans(X_sub), nrows), nrow = nrows, byrow = TRUE)
        row_loss <- norm(X_sub - col_mean_sub, "F")^2/sum(col_labels == i)
        penalization <- lambda * c/(norm(X_sub, "F")^2 + 1)
        loss <- loss + 1/n * row_loss + penalization
        changes[i] <- -penalization
    }

    min_change <- min(changes)
    min_loss <- loss + min_change
    index <- which.min(changes)
    list(loss = min_loss, index = index)
}

centers2vec <- function(centers, labels) {
    n <- length(labels)
    k <- length(centers)
    vec <- rep(0, n)
    for (i in 1:k) {
        vec[labels == i] <- centers[[i]]
    }
    vec
}

norm_centers <- function(centers) {
    norm_of_centers <- 0
    k <- length(centers)
    for (i in 1:k) {
        norm_of_centers <- norm_of_centers + sum(centers[[i]]^2)
    }
    norm_of_centers <- sqrt(norm_of_centers)
}

dist_centers <- function(centers1, centers2) {
    distance <- 0
    k <- length(centers1)
    for (i in 1:k) {
        distance <- distance + sum((centers1[[i]] - centers2[[i]])^2)
    }
    distance <- sqrt(distance)
}
