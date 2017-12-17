## --------------------------------------------------------------
source("~/Dropbox/org/Research/Software/R/calcio_hackathon/Yiannis/Functions.R")
## ----------------------------------------
player <- 1:11

tmp.distribute <- A1 %>% lapply(as.matrix)%>%
    lapply(closeness.score, weight=1)

tmp.receive <- A1 %>% lapply(as.matrix)%>%
    lapply(closeness.score, weight=0)

closeness1 <- A1 %>% lapply(as.matrix)%>%
    lapply(closeness.score, weight=.5)



for(i in 1:length(closeness1))
{
    Sys.sleep(10)
    plot(tmp.distribute[[i]], type="b", pch=4, col=2,
         ylim=c(0,10))
    lines(closeness1[[i]], type="b", pch=10, col=1)
    lines(tmp.receive[[i]], type="b", pch=10, col=3)
}

## ----------------------------------------
closeness1 %>% lapply(mean)
closeness1 %>% lapply(sd)

## ----------------------------------------
closeness2 <- A2 %>% lapply(as.matrix)%>%    
    lapply(closeness.score)
## ----------------------------------------
closeness2 %>% lapply(mean)
closeness2 %>% lapply(sd)

setwd("~/Dropbox/org/Research/Software/R/calcio_hackathon/Statistics/closeness/")

save(closeness1, file="closeness_team1.RData")
save(closeness2, file="closeness_team2.RData")

## ----------------------------------------
betweeness1 <- A1 %>% lapply(as.matrix)%>%
    lapply(betweeness.score)
## ----------------------------------------
betweeness1 %>% lapply(mean)
betweeness1 %>% lapply(sd)

## ----------------------------------------
betweeness2 <- A2 %>% lapply(as.matrix)%>%
    lapply(betweeness.score)
## ----------------------------------------
betweeness2 %>% lapply(mean)
betweeness2 %>% lapply(sd)


setwd("~/Dropbox/org/Research/Software/R/calcio_hackathon/Statistics/betweeness/")

save(betweeness1, file="betweeness_team1.RData")
save(betweeness2, file="betweeness_team2.RData")


## ----------------------------------------
## Page Rank
## ----------------------------------------
pagerank1 <- g1 %>% lapply(as.matrix)%>%
    lapply(page.rank)
pagerank2 <- g2 %>% lapply(as.matrix)%>%
    lapply(page.rank)

setwd("~/Dropbox/org/Research/Software/R/calcio_hackathon/Statistics/pagerank/")

save(pagerank1, file="pagerank_team1.RData")
save(pagerank2, file="pagerank_team2.RData")






