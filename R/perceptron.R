#' @title Perceptron
#' @description Perceptron
#' @export
#' @importFrom R6 R6Class
#' @examples
#' #"===============Example One===============\n")
#' X <- iris[1:100,1:2]
#' y <- iris$Species[1:100]
#' m <- Perceptron$new(max_iteration = 10000)
#' m$fit(X, y)
#' m$plot()
#' #"===============Example Two ===============\n")
#' X <- matrix(c(3, 3, 4, 3, 1, 1), ncol = 2, byrow = TRUE)
#' y <- c(1, 1, -1)
#' fit <- Perceptron$new()
#' fit$fit(X, y, w = c(0,0), b = 0)
#' fit$plot()

Perceptron <- R6Class(
        "Perceptron",
        private = list(
                .lr = NA_real_,
                .max_iter = NA_integer_,
                .verbose = logical(1),
                .predict = function(value) {
                        crossprod(self$w, value)[1] + self$b
                }
        ),
        public = list(
                #' @field X is the depandent variables matrix
                #' @field y is the response variable vector
                #' @field f_n number of features
                #' @field s_n number of samples
                #' @field w weight vector
                #' @field b intercept
                #' @field model the final model
                #' @field labels -1 and 1 stand for
                X = NULL,
                y = NULL,
                f_n = NULL,
                s_n = NULL,
                w = NULL,
                b = NULL,
                model = NULL,
                labels = NULL,
                #' @param learn_ratio learn ratio, the default value is 0.1
                #' @param max_iteration the max iteration
                #' @param verbose show more messages
                initialize = function(learn_ratio = 1e-1, max_iteration = 2e+3,
                                      verbose = TRUE) {
                        private$.lr <- learn_ratio
                        private$.max_iter <- max_iteration
                        private$.verbose <- verbose
                },
                #' @description fit the data
                #' @param X matrix contains dependent variable
                #' @param y vector the response variable
                #' @param seed used to `set.seed` for get random initial weights and intercept
                #' @param w initial weights, `fit` will random initial a weights vector if `w` is NULL
                #' @param b initial intercept, `fit` will random initial a value if `b` is NULL
                fit = function(X, y, seed = 123, w = NULL, b = NULL) {
                        X <- as.matrix(X)
                        self$X <- X
                        self$y <- ifelse(factor(y, labels = c(-1, 1)) == -1,
                                         -1, 1)
                        self$labels <- structure(unique(y), names = c(-1, 1))
                        self$f_n <- ncol(X)
                        self$s_n <- nrow(X)
                        set.seed(seed)
                        self$w <- ifelse(is.null(w),list(w=rnorm(self$f_n)),
                                         list(w = w))
                        self$w <- self$w[[1]]
                        self$b <- ifelse(is.null(b),rnorm(1), b)
                        stopifnot("w or b dimention wrong!" = length(self$w) == self$f_n & length(self$b) == 1 )
                        cat("Initial weights is: ", self$w, "\n",
                            "Initial intercept is: ", self$b, "\n")
                        ok <- FALSE
                        n <- 0
                        while(n <= private$.max_iter) {
                                pre_tmp <- vapply(seq_len(self$s_n), \(x) {
                                        sign(private$.predict(self$X[x,]))
                                }, numeric(1))
                                ok <- all(pre_tmp == self$y)
                                if (ok) {
                                        cat("epoch: ", n, " \n\tw: ", self$w,
                                            "\tb: ", self$b, "\n")
                                        break
                                }
                                x_f <- which(pre_tmp != self$y)[1]
                                self$w <- self$w + private$.lr * self$X[x_f,] * self$y[x_f]
                                self$b <- self$b + private$.lr * self$y[x_f]
                                n <- n + 1
                        }
                        self$model <- substitute(
                                w1 * x[1] + w2 * x[2] + b == 0,
                                list(
                                        w1 = round(self$w[1], 2),
                                        w2 = round(self$w[2], 2),
                                        b = round(self$b, 2)
                                ))
                },
                #' @description plot
                plot = function() {
                        X <- self$X
                        y <- self$y
                        plot(X, col = 'blue', xlab = "", ylab = "")
                        mtext(as.expression(self$model))
                        points(X[y == 1,], col = "red")
                        abline(-(self$b/self$w[2]), -(self$w[1]/self$w[2]))
                        title(
                              "Perceptron Model",
                              xlab = ifelse(is.null(colnames(X)),
                                            expression(x[1]), colnames(X)[1]),
                              ylab = ifelse(is.null(colnames(X)),
                                            expression(x[2]), colnames(X)[2])
                              )
                        legend("bottomright", legend = self$labels,
                               col = c("blue", "red"), pch = 1)
                }
        )
)
