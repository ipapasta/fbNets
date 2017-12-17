library(igraph)
library(reshape)
library(plyr)
library(tidyverse)

path <- "/home/ipapasta/Dropbox/org/Research/Data/Hackathon - Sfida Match Analysis/2014 World Cup/"

setwd(path)
all.data       <- list.files()
booleanf24     <- sapply(all.data, function(x) grepl("f24", x))
all.data       <- all.data[booleanf24]
n.data         <- length(all.data)
files.to.save1 <- NULL
files.to.save2 <- NULL
event.types <- as.data.frame(
    read.csv("~/Dropbox/org/Research/Software/R/calcio_hackathon/event_types.csv")
) %>%
    select(type_id=id, event_name = name)  %>%
    mutate(type_id = as.numeric(type_id))

## files.to.save <- sapply(all.data,
##                         function(x) gsub(".xml",".csv", x))
for(i in 1:n.data)
    {
        files.to.save1[i] <- gsub(".xml",
                                  "_adj1.csv",
                                  all.data[i])
        files.to.save2[i] <- gsub(".xml",
                                  "_adj2.csv",
                                  all.data[i])
    }



team.1 <- NULL
team.2 <- NULL 

## --------------------------------
g1 <- vector('list', length=n.data)
g2 <- vector('list', length=n.data)
## --------------------------------
A1 <- vector('list', length=n.data)
A2 <- vector('list', length=n.data)
## --------------------------------------------
agg.positions1 <- vector('list', length=n.data)
agg.positions2 <- vector('list', length=n.data)


setwd("~/Dropbox/org/Research/Software/R/calcio_hackathon/zygalakis_attempts/Adjacencies/")


library(igraph)
library(reshape)
library(plyr)
library(tidyverse)


for(i in 1:n.data)
{ 
    fnm <- paste(path,
                 all.data[i],
                 sep="")   
    ##utility function
    grabAll <- function(XML.parsed, field){
        parse.field <- xpathSApply(XML.parsed, paste("//", field, "[@*]", sep="")) #what does this regular expression stand for?
        results <- t(sapply(parse.field, function(x) xmlAttrs(x)))
        if(typeof(results)=="list"){
            do.call(rbind.fill, lapply(lapply(results, t), data.frame, stringsAsFactors=F))
        } else {
            as.data.frame(results, stringsAsFactors=F)
        }
    }
    ##XML Parsing
    pbpParse <- xmlInternalTreeParse(fnm)
    eventInfo <- grabAll(pbpParse, "Event")
    eventParse <- xpathSApply(pbpParse, "//Event")
    NInfo <- sapply(eventParse, function(x) sum(names(xmlChildren(x)) == "Q"))
    QInfo <- grabAll(pbpParse, "Q")
    EventsExpanded <- as.data.frame(lapply(eventInfo[,1:2], function(x) rep(x, NInfo)), stringsAsFactors=F)
    QInfo <- cbind(EventsExpanded, QInfo)
    names(QInfo)[c(1,3)] <- c("Eid", "Qid")
    QInfo$value <- ifelse(is.na(QInfo$value),-1, QInfo$value)
    Qual <- cast(QInfo, Eid ~ qualifier_id)
    ## 
    ##Game data set & team details
    game.details <- as.data.frame(xpathApply(pbpParse,
                                             "//Game",
                                             xmlAttrs),
                                  stringsAsFactors = FALSE)
    team <- data.frame(ids = c(game.details["home_team_id",],
                               game.details["away_team_id",]),
                       names=c(game.details["home_team_name",],
                               game.details["away_team_name",]),
                       home=c(1,0))
    ## 
    ## 
    ##Event data set
    events <- merge(eventInfo, Qual, by.x="id", by.y="Eid", all.x=T)
    events <- events %>% mutate(period_id =
                                    as.numeric(period_id),
                                min=as.numeric(min), 
                                sec=as.numeric(sec), 
                                x = as.numeric(x), 
                                y=as.numeric(y), 
                                outcome = as.numeric(outcome),
                                player_id =
                                    as.numeric(player_id))
    ## 
    ##Load event_type dataset
    ## 
    ## Join to get event names
    events <- join(events, event.types, by=c("type_id"))
    ## 
    ##Sort to get right chronological order (note the order by team_id!!)
    events <- events %>% arrange(team_id, period_id, min, sec, timestamp)
    ## 
    ##Reduce data a bit
    events.firsthalf <- events %>% filter(period_id==1)
    events.secondhalf <- events %>% filter(period_id==2)
    ## 
    ## 
    ##Lineup data -- qualifiers 30, 131, 194, 227, 44, 59
    ## 30: Player id's in lineup
    ## 130: Team player formation
    ## 194 : Captian
    ## 44 : Player position
    ## 59: Jersey Number.
    ## 227 : ??
    ## 
    getTeamLineup <- function(events, team_id){
        lineup.data <- events %>%
            filter(event_id==1)
        team1.lineup <- lineup.data %>%
            filter(team_id==team_id)
        team1 <- data.frame(
            id =  as.numeric(unlist(strsplit(as.character(team1.lineup[["30"]]),','))),
            positions = as.numeric(unlist(strsplit(as.character(team1.lineup[["131"]]),','))))    
        return(team1)
    }
    ## 
    lineups <- c(list(getTeamLineup(events, team$ids[1])),list(getTeamLineup(events, team$ids[2])))
    ## 
    PASS_ID <- "1"
    FORMATION_CHANGE_ID <- "40"
    PLAYER_ON <- "19"    
    lineups <- list(getTeamLineup(events, team$ids[1]),getTeamLineup(events, team$ids[2]))
    doFormationChangeEvent = function(formation_change_event) {
        team1 <- data.frame(
            id =  as.numeric(unlist(strsplit(as.character(formation_change_event[["30"]]),','))),
            positions = as.numeric(unlist(strsplit(as.character(formation_change_event[["131"]]),','))))
        return(team1)
    }
    doPlayerOnEvent = function(event, lineup) {
        ## print(lineup)
        player <- event$player_id
        new.position <- event[["145"]]
        ## print(c("New position = ",new.position, " : ", player))
        lineup$positions[lineup$id==player] <- new.position
        ## print(lineup)
        return(lineup)
    }
    ##Now extract passing data
                                        #Now extract passing data
    extractPassingSequences <- function(events, team_lineup, teamID) {
  
  motifs <- list()
  passes <- matrix(NA, 0, 4)
  colnames(passes) = c("to.player","from.player", "x", "y")
  
  p.start <- 1
  p.end <- 1
  
  while (p.end<= length(events$id)){
    while (((events$type_id[p.start] != PASS_ID) | (events$outcome[p.start]==0))   && (p.start<= length(events$id))){
      
      if (events$type_id[p.start]==FORMATION_CHANGE_ID) {
        team_lineup<- doFormationChangeEvent(events[p.start,])
      } else  if (events$type_id[p.start] ==PLAYER_ON) {
        team_lineup<- doPlayerOnEvent(events[p.start,], team_lineup)
      }
      p.start = p.start + 1
    }
    
    p.end <- p.start
    repeat{
      p.end <- p.end + 1
      
      if (is.na(events$player_id[p.end])) {
        break;
      }
      
      pos1 <- team_lineup$positions[team_lineup$id == events$player_id[p.end-1]][1]
      pos2<- team_lineup$positions[team_lineup$id == events$player_id[p.end]][1]
      
      # print(c("Pass from:",events$player_id[p.end-1], events$plRayer_id[p.end], pos1, pos2))
      if (is.na(pos1)){
        break;
      }
      
      passes = rbind(passes, c(pos2, pos1, events$x[p.end-1], events$y[p.end-1]))
      
      
      cndn <- ((events$type_id[p.end] == PASS_ID) && (events$outcome[p.end]==1))
      if (!cndn) {
        break;
      }
    }
    
    if (p.start < p.end) {
      idl<-rep(NA, p.end-p.start)
      
      for (k in p.start:p.end){
        position <- team_lineup$positions[team_lineup$id == events$player_id[k]][[1]]
        if (!is.na(position)){
          idl[(k - p.start) + 1] <- team_lineup$positions[team_lineup$id == events$player_id[k]];
        }
      }
      
      #    print(c("Save:", p.start, p.end))
      # idl <- events$player_id[p.start:(p.end-1)]
      if (!any(is.na(idl))){
        motifs <- append(motifs, list(rle(unlist(idl))$values))
      }
    }
    
    p.start <- p.end 
  }
  
  return(list(motifs=motifs, pass.data = as.data.frame(passes)))
}
    
    ## ----------------------------------------------------------
    events.team1  <- events %>%
        filter((period_id %in% c(1,2)) &
               (team_id== team$ids[1]))
    ##

    events.team2  <- events %>%
        filter((period_id %in% c(1,2)) &
               (team_id== team$ids[2]))
    team.1[i] <- team$ids[1] %>% as.character %>% as.numeric
    team.2[i] <- team$ids[2] %>% as.character %>% as.numeric
    ## 
    passes.team1  <-  extractPassingSequences(events.team1,
                                              lineups[[1]],
                                              team$ids[1])
    passes.team2  <-  extractPassingSequences(events.team2,
                                              lineups[[2]],
                                              team$ids[2])    
    pass.positions  <- passes.team1$pass.data %>%
        select(player = from.player, x, y)
    agg.positions1[[i]]  <- pass.positions %>%
        group_by(player) %>%
        summarise(x_mean = median(x), y_mean = median(y),
                  x_var = var(x), y_var = var(y),
                  xy_cov = cov(x,y))
    pass.positions <- passes.team2$pass.data %>%
        select(player = from.player, x, y)
    agg.positions2[[i]]  <- pass.positions %>%
        group_by(player) %>%
        summarise(x_mean = median(x), y_mean = median(y),
                  x_var = var(x), y_var = var(y),
                  xy_cov = cov(x,y))
    ## 
    df1 <- passes.team1$pass.data
    df2 <- passes.team2$pass.data
    vertices1 <- data.frame(names=
                                df1 %>%
                                select(from.player,
                                       to.player) %>%
                                as.matrix %>%
                                as.vector %>% unique)
    edges1    <- data.frame(from = unlist(df1$from.player),
                            to = unlist(df1$to.player))
    vertices2 <- data.frame(names=
                                df2 %>%
                                select(from.player,
                                       to.player) %>%
                                as.matrix %>%
                                as.vector %>% unique)
    edges2    <- data.frame(from = unlist(df2$from.player),
                            to = unlist(df2$to.player))
    g1[[i]] <- graph_from_data_frame(edges1,
                                directed=TRUE,
                                vertices=vertices1)
    g2[[i]] <- graph_from_data_frame(edges2,
                                     directed=TRUE,
                                     vertices=vertices2)
    ## --------------------------------------------------------
    ## second team
    ## --------------------------------------------------------
    g1[[i]]$weighted<-TRUE
    E(g1[[i]])$weight <- 1
    g1[[i]]  <- igraph::simplify(g1[[i]],
                                 edge.attr.comb=
                                     list(weight="sum"))
    iso <- V(g1[[i]])[degree(g1[[i]])==0]
    g1[[i]]   <- delete.vertices(g1[[i]], iso)
    ## A1[[i]] <- as_adjacency_matrix (g1[[i]], attr="weight") %>%
    ##     as.matrix
    A1[[i]] <- as_adjacency_matrix (g1[[i]], attr="weight")
    ## --------------------------------------------------------
    ## second team
    ## --------------------------------------------------------
    g2[[i]]$weighted<-TRUE
    E(g2[[i]])$weight <- 1
    g2[[i]]  <- igraph::simplify(g2[[i]],
                            edge.attr.comb=list(weight="sum"))
    iso <- V(g2[[i]])[degree(g2[[i]])==0]
    g2[[i]]   <- delete.vertices(g2[[i]], iso)
    ## A2 <- as_adjacency_matrix (g2[[i]], attr="weight") %>%
    ## as.matrix
    A2[[i]] <- as_adjacency_matrix (g2[[i]], attr="weight")     
    ## write.csv(A1[[i]]%>% as.matrix, file=files.to.save1[i] )
    ## write.csv(A2[[i]]%>% as.matrix, file=files.to.save2[i] )  
}




## --------------------------------------------------------------



#####!!!Grandissimo cornuto
pass.positions <- passes.team1$pass.data %>% select(player = from.player, x, y)
agg.positions <- pass.positions %>% group_by(player) %>% summarise(x_mean = mean(x), y_mean = mean(y),x_var = var(x), y_var = var(y), xy_cov = cov(x,y))

## ----------------------------------------------------

rnms <- function(A)
{
    A %>%
        as.matrix %>%
        as.data.frame %>%
        row.names %>%
        as.numeric %>%
        order
}

for(j in 1:length(A1))
{
    A1[[j]] <- (A1[[j]] %>% as.matrix)[order(A1[[j]] %>% as.matrix %>% as.data.frame %>% names %>% as.numeric),order(A1[[j]] %>% as.matrix %>% as.data.frame %>% names %>% as.numeric)]
    A2[[j]] <- (A2[[j]] %>% as.matrix)[order(A2[[j]] %>% as.matrix %>% as.data.frame %>% names %>% as.numeric),order(A2[[j]] %>% as.matrix %>% as.data.frame %>% names %>% as.numeric)]
}

    (A1[[1]] %>% as.matrix)[order(A1[[1]] %>% as.matrix %>% as.data.frame %>% names %>% as.numeric),order(A1[[1]] %>% as.matrix %>% as.data.frame %>% names %>% as.numeric)]


##Ancora piu grande cornuto
