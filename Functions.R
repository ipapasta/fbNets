closeness.score <-
    function(A, weight=.5, nms = NULL, ...)
{
    ## ---------------------------------------------------
    ## DESCRIPTION:
    ## ---------------------------------------------------
    ## function closeness score returns the
    ## closeness centrality of all players in a team
    ## (Section 3.1 in Pena and Touchette)
    ## ---------------------------------------------------
    ## INPUTS:
    ## ---------------------------------------------------
    ## A: p by p adjacency matrix where A_{ij} equals  
    ## the number of passes given by player i to player j
    ## nms: names of players. Defaulted at NULL returning
    ## a vector of numeric values.
    L       <- 1/A
    diag(L) <- 0
    n.L     <- L %>% nrow
    graph.L <- graph.adjacency(L, weighted=TRUE)
    sh.paths.L <- shortest.paths(graph.L,
                                 algorithm = "dijkstra",
                                 mode="out")
    C <- weight*(sh.paths.L %>% apply(1, sum)) +
        (1-weight)*(sh.paths.L %>% apply(2, sum))
    if(!is.null(nms))
    {
        (n.L-1)/(C %>% as.numeric %>% names(nms)) %>% return
    }
    (n.L-1)/(C %>% as.numeric) %>% return
} 


betweeness.score <-
    function(A, nms = NULL, ...)        
{
    ## ‘shortest_paths’ calculates one
    ##  shortest path (the path itself,
    ## and not just its length) from or
    ##  to the given vertex.
    n.A     <- A %>% nrow
    L       <- 1/A    
    diag(L) <- 0
    graph.L <- graph.adjacency(L, weighted=TRUE)
    if(!is.null(nms))
    {
        V(graph.L)$name <- nms %>% as.character
        betweenness(graph.L) %>% return     # import from igraph
    }
    V(graph.L)$name <- 1:n.A %>% as.character
    ((betweenness(graph.L))/((n.A-1)*(n.A-2))) %>% return     # import from igraph
}


## --------------------------------------------------------------
## TODO: pagerank
## inputs:
## A: 11 by 11 adjacency matrix
## --------------------------------------------------------------

pagerank <-
    function(A, nms = NULL, ...)        
{
    ## ----------------------------------------------------
    ## TODO: damping factor 
    ## 
    ##
    ## ----------------------------------------------------
    A       <- t(A / rowSums(A))
    n.A     <- A %>% nrow
    ## L       <- 1/A
    ## diag(L) <- 0
    graph.A <- graph.adjacency(A,
                               weighted=TRUE)
    V(graph.A)$name <- 1:n.A %>% as.character
    if(!is.null(nms))
    {
        V(graph.A)$name <- nms %>% as.character
    }
    ## ----------------------------------------------------
    ## ----------------------------------------------------    
    ## U <- rep(1/n.A, n.A^2) %>%
    ##     matrix(nrow=n.A, ncol=n.A)
    ## beta <- 0.85
    ## A    <- beta*M+(1-beta)*U
    ## e    <- eigen(A)
    ## v    <- e$vec[,1]
    ## v    <- as.numeric(v) / sum(as.numeric(v))
    page.rank(graph.A)$vector %>%
                         as.numeric %>%
                         return
}



clustering.coefficient <-
    function(A, nms = NULL, ...)        
{
    ## ----------------------------------------------------
    ## TODO: 
    ## 
    ##
    ## ----------------------------------------------------
    A       <- t(A / rowSums(A))
    n.A     <- A %>% nrow
    E       <- A>0
    U       <- E %>% rowSums
    ## L       <- 1/A
    ## diag(L) <- 0
    graph.A <- graph.adjacency(A,
                               weighted=TRUE)
    V(graph.A)$name <- 1:n.A %>% as.character
    if(!is.null(nms))
    {
        V(graph.A)$name <- nms %>% as.character
    }
    ## ----------------------------------------------------
    page.rank(graph.A)$vector %>%
                         as.numeric %>%
                         return
}



in.box <- function(x, y)
{
    in.x <- sapply(good.passes.coo.team1$to.x,
                   function(x) x > 83 & x < 100 )
    in.y <- sapply(good.passes.coo.team1$to.y,
                   function(y) y > 21.1 & y < 78.9 )
    (in.x & in.y) %>% sum
}
