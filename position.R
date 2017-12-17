path <- "/home/ipapasta/Dropbox/org/Research/Data/Hackathon - Sfida Match Analysis/2014 World Cup/"

setwd(path)
all.data       <- list.files()
booleanf24     <- sapply(all.data, function(x) grepl("f24", x))
all.data       <- all.data[booleanf24]
n.data         <- length(all.data)
files.to.save1 <- NULL
files.to.save2 <- NULL
## ------------------------------------------------------------
goals.team1 <- NULL
goals.team2 <- NULL


tag <- c(1, 0, 0, 0, 0, 2, 2, 2, 2, 2)
quals  <-   c("1", "7", "8", "44", "49",
              "13", "14", "15", "16", "60")
nms    <-   c("pass", "tackle",
              "interception", "aerial",
              "ball recovery",
              "miss", "post", "attempt saved","goal",
              "chance missed")

event.list     <- vector('list', length(quals))
success.metric <- vector('list', length=n.data)



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



## ## --------------------------------
## g1 <- vector('list', length=n.data)
## g2 <- vector('list', length=n.data)
## ## --------------------------------
## A1 <- vector('list', length=n.data)
## A2 <- vector('list', length=n.data)

## setwd("~/Dropbox/org/Research/Software/R/calcio_hackathon/Statistics/centrality/")

for(i in 1:n.data)
{ 
    fnm <- paste(path,
                 all.data[i],
                 sep="")   
    ##utility function
    grabAll <- function(XML.parsed, field){
        parse.field <- xpathSApply(XML.parsed,
                                   paste("//", field,
                                         "[@*]", sep=""))
        results <- t(sapply(parse.field,
                            function(x) xmlAttrs(x)))
        if(typeof(results)=="list"){
            do.call(rbind.fill,
                    lapply(lapply(results, t),
                           data.frame, stringsAsFactors=F))
        } else {
            as.data.frame(results, stringsAsFactors=F)
        }
    }
    ##XML Parsing
    pbpParse   <- xmlInternalTreeParse(fnm)
    eventInfo  <- grabAll(pbpParse, "Event")
    eventParse <- xpathSApply(pbpParse, "//Event")
    NInfo      <- sapply(eventParse,
                         function(x)
                             sum(names(xmlChildren(x)) == "Q"))
    QInfo <- grabAll(pbpParse, "Q")
    EventsExpanded <- as.data.frame(lapply(eventInfo[,1:2],
                                           function(x)
                                               rep(x, NInfo)),
                                    stringsAsFactors=F)
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
##     ## 
##     ## -----------------------------------------------
## ####Now extract passing data
##     ## -----------------------------------------------
##     extractPassingSequences <- function(events) {
##         motifs <- list()
##         pass.data <- matrix(NA, 0, 6)
##         colnames(pass.data) = c("to.player",
##                                 "from.player",
##                                 "from.x",
##                                 "to.x",
##                                 "from.y",
##                                 "to.y")  
##         p.start <- 1
##         p.end <- 1    
##         while (p.end<= length(events$id)){
##             while ((events$type_id[p.start] != PASS_ID)  && (p.start<= length(events$id) )){
##                 p.start = p.start + 1
##             }        
##             p.end <- p.start + 1
##             while ((events$type_id[p.end] == PASS_ID) &&
##                    (p.end<= length(events$id) )) {
##                        pass.data = rbind(pass.data,
##                                          c(events$player_id[p.end-1],
##                                            events$player_id[p.end],
##                                            events$x[p.end-1], 0,
##                                            events$y[p.end-1], 0))
##                        p.end <- p.end + 1
##                    }
##             idl <- events$player_id[p.start:(p.end-1)]
##             motifs <- append(motifs, list(rle(unlist(idl))$values))    
##             p.start <- p.end
##         }  
##         return(list(motifs=motifs,
##                     pass.data = as.data.frame(pass.data)))
##     }
    ## !!!!!
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
    extractPassingSequences <- function(events, team_lineup,
                                        teamID) {        
        motifs <- list()
        pass.data <- matrix(NA, 0, 4)
        colnames(pass.data) = c("to.player","from.player",
                                "x", "y")        
        p.start <- 1
        p.end <- 1        
        while (p.end<= length(events$id)){
            while ((events$type_id[p.start] != PASS_ID)  &&
                   (p.start<= length(events$id) )){
                       
                       if (events$type_id[p.start]==FORMATION_CHANGE_ID) {
                           team_lineup<- doFormationChangeEvent(events[p.start,])
                       } else  if (events$type_id[p.start] ==PLAYER_ON) {
                           team_lineup<- doPlayerOnEvent(events[p.start,], team_lineup)
                       }
                       p.start = p.start + 1
                   }
            ## 
            p.end <- p.start + 1
            while ((events$type_id[p.end] == PASS_ID) &&
                   (p.end<= length(events$id) )) {
                       pos1 <- team_lineup$positions[team_lineup$id == events$player_id[p.end-1]]
                       pos2 <- team_lineup$positions[team_lineup$id == events$player_id[p.end]]
                       ## print(c(pos1, pos2))
                       pass.data = rbind(pass.data, c(pos1, pos2, events$x[p.end-1], events$y[p.end-1] ))
                       p.end <- p.end + 1
                   }
            idl <- team_lineup$positions[team_lineup$id %in% events$player_id[p.start:(p.end-1)]]
                                        # idl <- events$player_id[p.start:(p.end-1)]
            motifs <- append(motifs,
                             list(rle(unlist(idl))$values))
            ## 
            p.start <- p.end
        }        
        return(list(motifs=motifs,
                    pass.data = as.data.frame(pass.data)))
    }
    for(j in 1:2)
    {
        count <- NULL
        for(k in 1:length(quals))
        {
            event.list[[k]] <- events %>%
                filter((period_id %in% c(1,2)) &
                       (team_id== team$ids[j]) &
                       type_id == quals[k])
            event.list[[i]] <- event.list[[k]] %>%
                select(to.x="140", to.y="141", from.x=x, from.y=y)
            if(tag[k]==0){
                count[k] <- in.box(event.list[[k]]$to.x,
                                   event.list[[k]]$to.y)   
            } 
            if(tag[k]==1){
                count[k] <- in.box(event.list[[k]]$from.x,
                                   event.list[[k]]$from.y)   
            }
            if(tag[k]==2){
                count[k] <- event.list[[k]] %>% nrow
            }
        }   
        success.metric[[i]][j] <- sum(count)
        
    }
    ## ---------------------------------------------------------
    ## Add id_pass==1? 
    ## events.team1  <- events %>%
    ##     filter((period_id %in% c(1,2)) &
    ##            (team_id== team$ids[1]) & outcome==1 &
    ##            type_id == "1")
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ## good.passes.team1 <- events %>%
    ##     filter((period_id %in% c(1,2)) &
    ##            (team_id== team$ids[1]) & type_id==1 &
    ##            outcome == 1)
    ## good.passes.team2 <- events %>%
    ##     filter((period_id %in% c(1,2)) &
    ##            (team_id== team$ids[2]) & type_id==1 &
    ##            outcome == 1)
    ## ## ---------------------------------------------
    ## good.passes.coo.team1 <- good.passes.team1 %>%
    ##     select(id, from.x=x, from.y=y,
    ##            to.x="140", to.y="141")    
    ## good.passes.coo.team2 <- good.passes.team2 %>%
    ##     select(id, from.x=x, from.y=y,
    ##            x="140", y="141")
    ## ## ---------------------------------------------
    ## good.passes.coo.team1 <- good.passes.coo.team1 %>%
    ##     apply(2,as.character) %>%
    ##     apply(2,as.numeric) %>% as.data.frame
    ## ## names(good.passes.coo.team1) <- c("id","x","y")
    ## good.passes.coo.team2 <- good.passes.coo.team2 %>%
    ##     apply(2,as.character) %>%
    ##     apply(2,as.numeric) %>% as.data.frame
    ## ## names(good.passes.coo.team2) <- c("id","x","y")
    ## ## TO:
    ## T1.x <- sapply(good.passes.coo.team1$to.x,
    ##                function(x) x > 83 & x < 100 )
    ## T1.y <- sapply(good.passes.coo.team1$to.y,
    ##                function(y) y > 21.1 & y < 78.9 )
    ## T2.x <- sapply(good.passes.coo.team2$to.x,
    ##                function(x) x > 83 & x < 100 )
    ## T2.y <- sapply(good.passes.coo.team2$to.y,
    ##                function(y) y > 21.1 & y < 78.9 )
    ## ## FROM:
    ## F1.x <- sapply(good.passes.coo.team1$from.x,
    ##                function(x) x < 83  )
    ## F1.y <- sapply(good.passes.coo.team1$from.y,
    ##                function(y) y < 21.1 | y > 78.9 )
    ## F2.x <- sapply(good.passes.coo.team2$from.x,
    ##                function(x) x < 83)
    ## F2.y <- sapply(good.passes.coo.team2$from.y,
    ##                function(y) y < 21.1 | y > 78.9 )
    ## success.metric1.1[i] <- (T1.x & T1.y) %>% sum
    ## success.metric1.1[i] <- (T1.x & T1.y) %>% sum
    ## success.metric1.2[i] <- (T2.x & T2.y) %>% sum
    ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ## ---------------------------------------------
    ## number of goals
    ## ---------------------------------------------
    game.details <-
        as.data.frame(xpathApply(pbpParse, "//Game", xmlAttrs),
                      stringsAsFactors = FALSE)
    team <- data.frame(ids = c(game.details["home_team_id",],
                               game.details["away_team_id",]),
                       names=c(game.details["home_team_name",],
                               game.details["away_team_name",]),
                       home=c(1,0))
    goals.team1[i] <- game.details[which(rownames(game.details) == "home_score"),] %>% as.numeric
    goals.team2[i] <- game.details[which(rownames(game.details) == "away_score"),] %>% as.numeric
}



events.goal <- events %>%
    filter((period_id %in% c(1,2)) &
           (team_id== team$ids[1]) & type_id=="16") %>% nrow

good.passes.team1 <- events %>%
    filter((period_id %in% c(1,2)) &
           (team_id== team$ids[1]) & type_id==1 &
           outcome == 1)

good.passes.coo.team2 <- good.passes.team2 %>%
    select(id, from.x=x, from.y=y,
           x="140", y="141", "Post")


tackles.inbox.team1 <- events %>%
    filter(type_id == "49")

ball.recovery.team1 <- events %>%
    filter(type_id == "49")

ball.recovery.team1 %>% select(from.x=x, from.y=y, "140")

tackles.inbox.team1%>%select(from.x=x, from.y=y)

miss.sogoal.team1 <- events %>%
    filter(type_id == "13")

## miss.sogoal.team1 %>% nrow              



## nrow: 2
## TO: 1
## FROM:0

for(j in 1:2)
{
    count <- NULL
    for(i in 1:length(quals))
    {
        event.list[[i]] <- events %>%
            filter((period_id %in% c(1,2)) &
                   (team_id== team$ids[j]) &
                   type_id == quals[i])
        event.list[[i]] <- event.list[[i]] %>%
            select(to.x="140", to.y="141", from.x=x, from.y=y)
        if(tag[i]==0){
            count[i] <- in.box(event.list[[i]]$to.x,
                               event.list[[i]]$to.y)   
        } 
        if(tag[i]==1){
            count[i] <- in.box(event.list[[i]]$from.x,
                               event.list[[i]]$from.y)   
        }
        if(tag[i]==2){
            count[i] <- event.list[[i]] %>% nrow
        }
    }   
    success.metric[j] <- sum(count)
}

