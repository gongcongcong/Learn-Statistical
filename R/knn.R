#' @title queue
#' @export
queue <- R6Class(classname = "queue",
                 public = list(
                         #' @field items content of queue
                         items = list(),
                         #' @description pop
                         pop = function(){
                                 if (self$is_empty()) {
                                         message("Queue is empty!")
                                         return()
                                 }
                                 ret <- self$items[1][[1]]
                                 self$items <- self$items[-1]
                                 return(ret)
                         },
                         #' @description whether the queue empty
                         is_empty = function() {
                                 length(self$items) == 0
                         },
                         #' @description add value to queue
                         #' @param value value to add
                         append = function(value) {
                                 self$items <- c(self$items, list(value))
                         },
                         #' @description print information
                         print = function() {
                                 print(self$items)
                         }
                 )
                 )


#' @title kd.node
#' @examples
#' root <- kd.node$new(item = c(3,4,5),
#'                     axis = 2
#'                     )
#' left <- kd.node$new(item = c(4,2,3), axis = 3)
#' left$parent
#' right <- kd.node$new(item = c(1,4,5), axis = 2)
#' root
#' root$add_child(left)
#' root$left
#' root$add_child(right)
#' root$right
#' left$parent
#'
#' left$sibling()
#' @export
kd.node <- R6Class(
        classname = "kd.node",
        private = list(
                #' @description information of `kd.node`
                .info = function() {
                        message("Item: ", paste0("(", paste0(self$item, collapse = ","), ")"),
                                ", Split: ", self$axis)
                }
        ),
        #' @field item node item
        #' @field axis dimentsion to split
        #' @field left left node
        #' @field right right node
        #' @field parent parent node
        public = list(
                item = NULL,
                axis = NULL,
                left = NULL,
                right = NULL,
                parent = NULL,
                #' @param item node item
                #' @param axis dimension to split
                #' @param left left node
                #' @param right right node
                #' @param parent parent node
                initialize = function(item, axis, left = NULL, right = NULL, parent = NULL) {
                        self$item <- item
                        self$axis <- axis
                        self$left <- left
                        self$right <- right
                        self$parent <- parent
                },
                #' @description get the node type
                #' @param X raw data to constructe `kd.tree`
                node_type = function(X) {
                        if (is.null(self$parent)) {
                                0 # root
                        } else {
                                a <- X[self$item, self$parent$axis]
                                b <- X[self$parent$item, self$parent$axis]
                                if (a < b) {
                                        1 # left
                                } else {
                                        2 # right
                                }
                        }
                },
                #' @description print `kd.node` information
                print = function(){
                        private$.info()
                },
                #' @description get the sibling node
                sibling = function(){
                        node_type <- self$node_type()
                        if (node_type == 0) {
                                warning("This node is root node")
                                return(NULL)
                        } else if (node_type == 1) {
                                return(self$parent$right)
                        } else {
                                return(self$parent$left)
                        }
                },
                #' @description add child node to the current node and the child node will be auto add to the left or right basing their `item`
                #' @param child child node
                #' @return child node
                add_child_auto = function(child) {
                        a <- self$item[self$axis]
                        b <- child$item[self$axis]
                        child$parent <- self
                        if (a > b) {
                                self$left <- child
                        } else {
                                self$right <- child
                        }
                        return(invisible(child))
                },
                #' @description add child node
                #' @param child child node
                #' @param type what type child node is, left or right?
                add_child = function(child, type) {
                        child$parent <- self
                        if (type == 'left') {
                                self$left <- child
                        } else {
                                self$right <- child
                        }
                        return(invisible(child))
                }
        )
)

#' @title kd.tree
#' @export
#' @param X matrix object, each row is point, each column is the dimentsion
#' @examples
#' ======================Example One====================
#' x <- c(2, 5, 9, 4, 8, 7)
#' y <- c(3, 4, 6, 7, 1, 2)
#' X <- matrix(c(x, y ), ncol = 2)
#' tree <- kd.tree$new(X)
#' tree
#' tree$plot()
#' tree$plot(T)
#' ======================Example Two====================
#' X <- iris[1:10, 1:4]
#' tree <- kd.tree$new(X)
#' tree
#' tree$plot()
#' @importFrom R6 R6Class
#' @importFrom igraph plot.igraph graph_from_data_frame layout.auto V V<-
kd.tree <- R6Class(
        classname = "kd.tree",
        private = list(
                .node_n = NULL,
                .df = list(
                        nodes = NULL,
                        links = data.frame()
                ),
                .split_idx = function(ps, axis, X) {
                        if (is.null(axis)) {
                                return(list(left = NULL, right = NULL,
                                            m_idx = ps))
                        }
                        x <- X[ps, axis]
                        x_n <- length(x)
                        x_idx <- order(x)
                        ps_m <- ceiling(x_n/2)
                        if (x_n == 0) {
                                return()
                        } else if (x_n == 2 | x_n == 1) {
                                m_idx <- ps[which.min(x)]
                                left <- NULL
                        } else {
                                m_idx <- ps[x_idx[ps_m]]
                                left <- ps[x_idx[1:(ps_m-1)]]
                        }
                        right <- ifelse(x_n == 1, list(NULL),
                                        list(ps[x_idx[(ps_m + 1):x_n]]))[[1]]
                        list(left = left, right = right, m_idx = m_idx)
                },
                .get_axis = function(ps, X) {
                        if (length(ps) == 1) {
                                NULL
                        } else {
                                which.max(apply(X[ps, ], 2, sd))
                        }
                },
                .add_link = function(node) {
                        nt <- node$node_type(self$df)
                        if (nt == 0) {
                                return()
                        } else {
                                nr <- nrow(private$.df$links)
                                from <- node$item
                                to <- node$parent$item
                                private$.df$links[nr+1, 1:2] <- c(from, to)
                        }
                },
                .add_nt = function(node) {
                        idx <- which(private$.df$nodes$name == node$item)
                        private$.df$nodes[idx, "color"] <- node$node_type(self$df)
                },
                .quote = function(x) {
                        paste0("(", paste0(x, collapse=","),")")
                }
        ),
        public = list(
                #' @field df the raw data to constructe `kd.tree`
                #' @field root the root node
                df = NULL,
                root = NULL,
                #' @description create the `kd.tree`
                #' @param X matrix-like data to construct `kd.tree`
                initialize = function(X) {
                        self$df <- X
                        que <- queue$new()
                        private$.node_n <- nrow(X)
                        private$.df$nodes <- data.frame(
                                                        name = seq_len(private$.node_n),
                                                        label = apply(X, 1,
                                                                     \(x) private$.quote(x) ),
                                                        color = NA
                                                        )
                        self$root <- kd.node$new(item = NULL, axis = NULL)
                        que$append(c(self$root,
                                     ps = list(seq_len(private$.node_n))))
                        while (!que$is_empty()) {
                                tmp <- que$pop()
                                node <- tmp[[1]]
                                ps <- tmp[[2]]
                                axis <- private$.get_axis(ps, X)
                                split_idx <- private$.split_idx(ps, axis, X)
                                if (is.null(split_idx)) {
                                        next
                                }
                                item <- split_idx[["m_idx"]]
                                node$axis <- axis
                                node$item <- item
                                left <- split_idx[["left"]]
                                right <- split_idx[["right"]]
                                private$.add_link(node)
                                private$.add_nt(node)
                                if (!is.null(left)) {
                                         left_node <- kd.node$new(NULL, NULL)
                                         left_node <- node$add_child(left_node, "left")
                                         que$append(c(left_node, list(left)))
                                }
                                if (!is.null(right)) {
                                        right_node <- kd.node$new(NULL, NULL)
                                        right_node <- node$add_child(right_node, "right")
                                        que$append(c(right_node, list(right)))
                                }
                        }
                        names(private$.df$links) <- c("from", "to")
                        private$.df$nodes$label.color <- private$.df$nodes$color
                        private$.df <- graph_from_data_frame(private$.df$links,
                                                             vertices = private$.df$nodes
                        )
                },
                #' @description return the igraph of the `kd.tree`
                get_graph = function() {
                        return(private$.df)
                },
                #' @description plot the tree
                #' @param show_id using the row number or feature content of raw data `X` as node label
                plot = function(show_id = FALSE) {
                        colors <- c("black", "orange", "darkgreen")
                        names(colors) <- 0:2 |> as.character()
                        G <- private$.df
                        if (show_id) V(G)$label <- V(G)$name
                        V(G)$color <- colors[as.character(V(G)$color)]
                        V(G)$label.color <- V(G)$color
                        plot.igraph(G, layout = layout.auto(G),
                                    vertex.label.dist = 2,
                                    edge.arrow.size = 0.4)
                       legend(1, 1.5, legend = c("root", "left", "right"),
                               col = colors, pch = 16, bty = 'n')
                },
                #' @description print the igraph
                print = function() {
                        print(private$.df)
                }
        )
)
