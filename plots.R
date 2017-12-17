full.names <- rbind(c(575, "AU", "Australia", NA),
                    read_csv("full_names.csv"))

full.names <- full.names %>% select(`575`, AU, Australia)

cmdscale(dist(x), k=3) %>%
    scatterplot3d(pch=16, angle=45, color=grs+1)

Z <- cmdscale(dist(x), k=2)
df <- data.frame(grs, cmdscale(dist(x), k=2))

## 1:64 home
## 65:128 away
ident <- NULL

for(i in 1:128)
{
    ident[i] <- which(full.names$`575` == as.character(c(team.1,team.2)[i]))           
}

plot(h)
grs <- cutree(h, h = 76.5)

U <- rnorm(128,sd=3)

Z <- cmdscale(dist(x), k=2)

(U + Z) %>% plot(pch=16,
                 cex=0,
                 xaxt="n",
                 yaxt="n",
                 xlab="",
                 ylab="",axes=FALSE,
                 axis.lab=2)

df2 <- data.frame(grs,Z, full.names$AU[ident])
df3 <- success.metric %>% as.data.frame %>% t
score <- c(df3[,1], df3[,2])
df2 <- cbind(df2, score)


cbind(df2, score, grs) %>% group_by(grs) %>%
    summarize(Mean=mean(Score))

names(df2) <- c("Groups", "Component1", "Component2", "Country",
                "Score")

pdf(width=12,height=8,file="~/Dropbox/org/Research/Software/R/calcio_hackathon/mds_cluster.pdf")

ggplot(data=df2) +
    geom_text(aes(x=Component1,Component2,
                  label=(paste(Country)),
                  color = factor(grs)),
              fontface="bold" , size=7) +
    theme(legend.position = "none") +
    theme(axis.title.x= element_blank(),
            axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y= element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())

dev.off()


ggplot(data=df2) +
    geom_text(aes(x=Component1,Component2,
                  label=(paste(Country)),
                  color = factor(grs)),
              fontface="bold" , size=7) +
    theme(legend.position = "none") +


text(U + Z , full.names$AU[ident], cex=2, col=grs+1)

cbind(which(full.names$AU[ident]=="IT"),
which(full.names$AU[ident]=="NL")



114 (England)    119 (Italy)
119 (Italy)      838 (Costa Rica)
119 (Uruguay)    837 (Italy)

(U + Z) %>% scatterplot3d(pch=16, color=grs+1)
(U + Z) %>% scatterplot3d(pch=16, color=grs+1,
                          cex=2,
                          xaxt="n",
                          yaxt="n",
                          xlab="",
                          ylab="")

text(U + Z , full.names$AU[ident], cex=2)



Radialsvm <- svm(factor(grs) ~ .,data=df,
                 kernel="radial",
                 cost=5,
                 scale=FALSE)

xgrid=expand.grid(X1=Z[,1],X2=Z[,2]) #generating grid points

ygrid=predict(Radialsvm, newdata = xgrid) #ygird consisting of predicted Response values

plot(xgrid,col=as.numeric(ygrid),pch=20,cex=0.3)
points(Z, pch=19, cex=1.5, col=grs+1)


