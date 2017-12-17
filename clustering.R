B1 <- cbind(matrix(closeness1 %>% unlist, nrow=64, ncol=11),
      matrix(betweeness1 %>% unlist, nrow=64, ncol=11),
      matrix((lapply(pagerank1, function(z) z$vector) %>% 
              unlist),  nrow=64, ncol=11))

B2 <- cbind(matrix(closeness2 %>% unlist, nrow=64, ncol=11),
            matrix(betweeness2 %>% unlist, nrow=64, ncol=11),
            matrix((lapply(pagerank2, function(z) z$vector) %>%
                    unlist),  nrow=64, ncol=11))



temp1 <- agg.positions1 %>% lapply(function(z) z[-1])
temp2 <- agg.positions2 %>% lapply(function(z) z[-1]) 


X <- t(as.data.frame(agg.positions1))
X.mean1 <- X[grep("x_mean", rownames(X)),]
Y.mean1 <- X[grep("y_mean", rownames(X)),]

X <- t(as.data.frame(agg.positions2))
X.mean2 <- X[grep("x_mean", rownames(X)),]
Y.mean2 <- X[grep("y_mean", rownames(X)),]

pos.vec1 <- cbind(X.mean1, Y.mean1)
rownames(pos.vec1) <- 1:64

pos.vec2 <- cbind(X.mean2, Y.mean2)
rownames(pos.vec2) <- 65:128

pos <- as.data.frame(rbind(pos.vec1, pos.vec2))

cbind(matrix.to.cluster)

matrix.to.cluster <- cbind(rbind(B1, B2), pos)


x=matrix.to.cluster
## 12:22
## 34:55

ind.features <- c(1:11, 34:55)
matrix.to.cluster.reduced <- matrix.to.cluster[,ind.features]





