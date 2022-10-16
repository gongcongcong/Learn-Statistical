#' @title kd.node
#' @examples
#' root <- kd.node$new(1, 2, 3, 4)
#' note <- root$add_child(kd.node$new(2, 2, 5, 6))
#' note
#' root
kd.node <- R6Class(
        classname = "kd.node",
        private = list(
                #' @description correct the location for each node
                #' @param node `kd.note`
                .update = function(node = self) {
                        left <- node$left
                        right <- node$right
                        if (!is.numeric(left)) {
                                left$set_location()
                                private$.update(left)
                        }
                        if (!is.numeric(right)) {
                                right$set_location()
                                private$.update(right)
                        }
                }
        ),
        #' @field item node item
        #' @field axis dimentsion to split
        #' @field left left node
        #' @field right right node
        #' @field parent parent node
        #' @field location location of node in the kd tree
        public = list(
                item = NULL,
                axis = NULL,
                left = NULL,
                right = NULL,
                parent = NULL,
                location = NULL,
                #' @description the type of node, 0 represents root node; 1 represents left node; 2 represents right node
                node_type = function() {
                        if (is.null(self$parent)) {
                                0 # root
                        } else if (self$item < self$parent$item) {
                                1 # left
                        } else {
                                2 # right
                        }
                },
                #' @description get and set the location of node by its item and parent's item
                set_location = function() {
                        type <- self$node_type()
                        if (type == 0) {
                                self$location <- c(1, 1)
                        } else if (type == 1) {
                                l <- self$parent$location
                                self$location <- c(l[1] + 1, l[2]*2 -1)
                        } else {
                                l <- self$parent$location
                                self$location <- c(l[1] + 1, l[2]*2)
                        }
                },
                #' @param item node item
                #' @param axis dimension to split
                #' @param left left node
                #' @param right right node
                #' @param parent parent node
                #' @param location location of node in the kd tree
                initialize = function(item = NULL, axis = NULL, left = NULL,
                                      right = NULL, parent = NULL) {
                        self$item <- item
                        self$axis <- axis
                        self$left <- left
                        self$right <- right
                        self$parent <- parent
                        self$set_location()
                },
                #' @description add child node
                #' @param child `kd.note` to add
                add_child = function(child) {
                        child$parent <- self
                        type <- child$node_type()
                        child$set_location()
                        if (type == 1) {
                                self$left <-  child
                        }
                        if (type == 2) {
                                self$right <-  child
                        }
                        invisible(child)
                }
        )
)

#' @title kd.tree
#' @export
#' @param X matrix object, each row is point, each column is the dimentsion
#' @examples
#' x <- c(2, 5, 9, 4, 8, 7)
#' y <- c(3, 4, 6, 7, 1, 2)
#' tree <- kd.tree$new(matrix(c(x, y ), ncol = 2))
#' tree$create()
#' tree
#' @importFrom R6 R6Class
#' @importFrom purrr walk
kd.tree <- R6Class(
        classname = "kd.tree",
        private = list(
                .X = NULL,
                #' @description transfer numeric vector to `kd.note`
                #' @param axis dimension to split
                #' @param ps which rows need to split in input matrix, it should be a integer vector
                #' @param parent parent node
                .split_idx = function(axis, ps = seq_len(nrow(private$.X)), parent = NULL) {
                        if (is.null(ps)) return(
                                return(kd.node$new(parent = parent))
                        )
                        x <- private$.X[ps, axis]
                        if (length(ps) == 1) return(
                                return(kd.node$new(axis = axis, item = x, parent = parent))
                        )
                        idx <- order(x)
                        p <- ceiling(length(x)/2)
                        left <- ifelse(p==1, list(0), list(ps[idx[1:(p-1)]]))[[1]]
                        return(kd.node$new(item = ps[idx[p]],
                                       axis = axis,
                                       left = left,
                                       right = ps[idx[(p+1):length(x)]],
                                       parent = parent
                                       )
                               )
                        },
                #' @description get node type, 0 represents this node has no child nodes; 1 represents it only has left child; 2 represents it only has right child node; 3 represents has left and right children
                .node_type = function(node) {
                        left <- is.null(node$left)
                        right <- is.null(node$right)
                        if (left & right) {
                                return(0) #no child nodes
                        } else if (left & !right) {
                                return(1) #only has left child
                        } else if (!left & right) {
                                return(2) #only has right child
                        } else {
                                return(3) #has left and right children
                        }
                        },
                #' @description transfer each children nodes to `kd.note`
                .update = function(node = self$nodes) {
                        if (private$.node_type(node) == 0 | node$axis >= ncol(private$.X)) {
                                cat("DONE\t!")
                        } else {
                                # browser()
                                left <- ifelse(inherits(node$left, "kd.node") ,
                                               list(node$left),
                                               list(private$.split_idx(node$axis+1,
                                                                       node$left, parent = node))
                                )[[1]]
                                right <- ifelse(inherits(node$right, "kd.node"),
                                               list(node$right),
                                               list(private$.split_idx(node$axis+1, node$right,
                                                                       parent = node))
                                )[[1]]
                                node$add_child(child = left)

                                node$add_child(child = right)
                                private$.update(left)
                                private$.update(right)
                        }
                },
                #' @description get the nodes position in the tree
                #' @param node get the `kd.note` location
                .to_df = function(node = self$nodes) {
                        left <- node$left
                        right <- node$right
                        l <- node$location
                        self$df[l[1], l[2]] <- node$item
                        if (is.numeric(left)) {
                                self$df[1+l[1], 2*l[2]-1] <- left
                        } else {
                                private$.to_df(left)
                        }
                        if (is.numeric(right)) {
                                self$df[1+l[1], 2*l[2]] <- right
                        } else {
                                private$.to_df(right)
                        }
                }
        ),
        public = list(
                #' @field df contain position of each node in `kd.tree`
                #' @field nodes store the `kd.note` information
                df = data.frame(),
                nodes = NULL,
                #' @param X matrix object, each row is point, each column is the dimentsion
                initialize = function(X) {
                        private$.X <- X
                },
                #' @description create the `kd.note` to store in `nodes` and get the location of each node to store in `df`
                create = function() {
                        root <- private$.split_idx(1)
                        private$.update(root)
                        self$nodes <- root
                        private$.to_df()
                },
                #' @description print the nodes location in `kd.tree`
                print = function() {
                        df <- self$df
                        walk(seq_len(nrow(df)), function(x){
                                value <- df[x,] |> unlist()
                                value <- value[!is.na(value)]
                                value <- sapply(value, \(v) ifelse(v == 0, "(NULL)",
                                                paste0("(", paste(private$.X[v, ],
                                                                  collapse = ","), ")"))
                                                )
                                cat(paste(value, collapse = ""),"\n")
                        })
                }
        )
)
