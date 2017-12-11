
score_p <- function(vector){
  vector <- as.numeric(vector)
  # points, rebounds, assists, steals, blocks
  (vector[1] + vector[2] + vector[3] * 1.5 + vector[4] * 2 + vector[5] * 2)
}

score_n <- function(vector){
  vector <- as.numeric(vector)
  #turnovers, fgattempted, fgmade, ftattempted, ftmade, personalfouls
  vector[1] * 2 + (vector[2] - vector[3]) * 0.5 + (vector[4] - vector[5]) * 0.5  + vector[6] * 0.5
}

score <- function(score_p, score_n){
  score_p - score_n
}

library(XML)
library(lubridate)
library(httr)
#one player
#appURL <- "http://www.basketball-reference.com/players/w/westbru01/gamelog/2016/"
#doc <- htmlParse(appURL)
#appTables <- doc['//table/tbody']
#myHeaders <- unlist(doc["//thead/tr[2]/th", fun = xmlValue])
#myTables <- lapply(appTables, readHTMLTable, header = myHeaders)
#westbrook <- data.frame(myTables)
#colnames <- c("Rk", "G", "Date", "Age", "Tm", "?", "Opp", "?", "GS", "MP", "FG", "FGA", "FG", "3P", "3PA", "3P", "FT", "FTA", "FT", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "+/-")
#colnames(westbrook) <- colnames
#all players
appURL <- "http://www.basketball-reference.com/leagues/NBA_2018_per_game.html"
lines <- readLines(appURL)
docplayers <- lines[grep("data-append-csv=", lines)]
#string zerlegen

links <- c()
for (i in 1:length(docplayers)){
  links[i] <- paste0("http://www.basketball-reference.com/", substr(docplayers[i], regexpr("players", docplayers[i]), (regexpr("html", docplayers[i])-2)), "/gamelog/2018/")
}

pl <- htmlParse(rawToChar(GET(appURL)$content))
plTable <- pl['//table/tbody']
plHeaders <- unlist(pl["//thead/tr[2]/th", fun = xmlValue])
players <- lapply(plTable, readHTMLTable, header = plHeaders)
players <- data.frame(players)
colnames(players) <- c("Rk", "Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FGp", "3P", "3PA", "3Pp", "2P", "2PA", "2Pp", "eFGp", "FT", "FTA", "FTp", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PSperG")
players <- players[players$Rk!="Rk",]
players <- cbind(players, links)

bb <- as.list(c(rep(0, length(players$Player))))
names(bb) <- players$Player

for (i in 1:length(bb)){
  appURL <- as.character(players$links[i])
  doc <- htmlParse(rawToChar(GET(appURL)$content))
  appTables <- doc['//table/tbody']
  myHeaders <- unlist(doc["//thead/tr[2]/th", fun = xmlValue])
  myTables <- lapply(appTables, readHTMLTable, header = myHeaders, stringsAsFactors = FALSE)
  person <- data.frame(myTables)
  if(ncol(person) == 30){
    colnames(person) <- c("Rk", "G", "Date", "Age", "Tm", "-", "Opp", "Res", "GS", "MP", "FG", "FGA", "FGp", "3P", "3PA", "3Pp", "FT", "FTA", "FTp", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "plusminus")
  }
  bb[[i]] <- person
}

bbstat<-bb


for (n in 1:length(bbstat)){
  bbstat[[n]] <- bbstat[[n]][(bbstat[[n]]$Rk!="Rk"),]
  if(ncol(bbstat[[n]]) == 30){
    pos_mp <- apply(bbstat[[n]][,c("PTS", "TRB", "AST", "STL", "BLK")], 1, score_p)
    neg_mp <- apply(bbstat[[n]][,c("TOV", "FGA", "FG", "FTA", "FT", "PF")], 1, score_n)
    tot_mp <- pos_mp-neg_mp
    bbstat[[n]] <- cbind(bbstat[[n]], pos_mp, neg_mp, tot_mp)
  }
}

#summary((bbstat$`Stephen Curry`)[,c("tot_mp")])

stat <- data.frame(matrix(NA, nrow = 1, ncol = 15))
colnames(stat) <- c(names(summary(1)), "games", "sd", "min last 5", "min else", "p value mins", "mean last 5", "mean else", "p value means","points total")
stat <- cbind(paste0(names(bbstat)), stat)
colnames(stat)[1] <- "Name"

for (n in 1:length(bbstat)){
  if(!is.null(bbstat[[n]]$'tot_mp')){
    points <- bbstat[[n]]$'tot_mp'
    mins <- paste0((bbstat[[n]])[,c("MP")])
    mins[mins == "NA"] <- NA
    stat[n,8] <- as.numeric(bbstat[[n]][nrow(bbstat[[n]]),c("G")])
    stat[n,2:7] <- as.numeric(summary(points))[1:6]
    stat[n,9] <- sd(points, na.rm = TRUE)
    stat[n,16] <- sum(points, na.rm = TRUE)
    clean <- na.omit(points)
    cmins <- na.omit(as.numeric(as.period(ms(mins), unit = "sec"))) / 60
    if(length(clean) > 10){
      stat[n,10] <- mean(cmins[(length(cmins)-5):length(cmins)])
      stat[n,11] <- mean(cmins[1:(length(cmins)-6)])    
      stat[n,12] <- t.test(cmins[(length(cmins)-5):length(cmins)], mu = stat[n,11], alternative = "greater")$p.value
      stat[n,13] <- mean(clean[(length(clean)-5):length(clean)])
      stat[n,14] <- mean(clean[1:(length(clean)-6)])
      stat[n,15] <- t.test(clean[(length(clean)-5):length(clean)], mu = stat[n,14], alternative = "greater")$p.value
    }
  }
  
}


##not every player matched
overwrite <- function(name){
  name <- as.character(name)
  ind <- as.numeric(regexpr(' ', name))
  
  out <- paste0(substr(name, (ind+1), nchar(name)), ", ", substr(name, 1, (ind-1)))
  
}

setwd("/run/media/atoeroek/Data/basketball.de_data")
bb_raw <- read.delim("20171210_players.csv", stringsAsFactors = FALSE)
stat$Name <- sapply(stat$Name, overwrite)

indn <- as.numeric(sapply(stat$Name, grep, bb_raw$Spieler))
#cbind(as.character(bb_raw$Spieler[na.omit(indn)]), as.character(stat$`names(bbstat)`[!is.na(indn)]))

stat$pos <- rep(NA, nrow(stat))
stat$gehalt <- rep(NA, nrow(stat))

stat$pos[!is.na(indn)] <- as.character(bb_raw$Pos.[na.omit(indn)])
stat$gehalt[!is.na(indn)] <- as.numeric(as.character(bb_raw$Gehalt[na.omit(indn)]))

stat$ppg <- stat$Mean / stat$gehalt
stat$ppgl <- stat$`mean last 5` / stat$gehalt

statmod <- stat[stat$games > 6 & stat$gehalt > 0.5,]
model <- lm(gehalt ~ Mean, statmod)

stat$value <- sapply(stat$Mean, function(x){model$coefficients[1] + model$coefficients[2] * x})
stat$valuation <- stat$value - stat$gehalt

colnames(stat)[1] <- "Name"
stat <- unique(stat)
stat <- stat[order(stat$valuation, decreasing = TRUE),]

setwd("/run/media/atoeroek/Data/dominateR")
save.image(file = "dominateR2018.RData")

#all players
appURL <- "http://www.basketball-reference.com/leagues/NBA_2017_per_game.html"
lines <- readLines(appURL)
docplayers <- lines[grep("data-append-csv=", lines)]
#string zerlegen

links <- c()
for (i in 1:length(docplayers)){
  links[i] <- paste0("http://www.basketball-reference.com/", substr(docplayers[i], regexpr("players", docplayers[i]), (regexpr("html", docplayers[i])-2)), "/gamelog/2017/")
}

pl <- htmlParse(rawToChar(GET(appURL)$content))
plTable <- pl['//table/tbody']
plHeaders <- unlist(pl["//thead/tr[2]/th", fun = xmlValue])
players <- lapply(plTable, readHTMLTable, header = plHeaders)
players <- data.frame(players)
colnames(players) <- c("Rk", "Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FGp", "3P", "3PA", "3Pp", "2P", "2PA", "2Pp", "eFGp", "FT", "FTA", "FTp", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PSperG")
players <- players[players$Rk!="Rk",]
players <- cbind(players, links)

bb <- as.list(c(rep(0, length(players$Player))))
names(bb) <- players$Player

for (i in 1:length(bb)){
  appURL <- as.character(players$links[i])
  doc <- htmlParse(rawToChar(GET(appURL)$content))
  appTables <- doc['//table/tbody']
  myHeaders <- unlist(doc["//thead/tr[2]/th", fun = xmlValue])
  myTables <- lapply(appTables, readHTMLTable, header = myHeaders)
  person <- data.frame(myTables)
  colnames(person) <- c("Rk", "G", "Date", "Age", "Tm", "-", "Opp", "Res", "GS", "MP", "FG", "FGA", "FGp", "3P", "3PA", "3Pp", "FT", "FTA", "FTp", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc")
  bb[[i]] <- person
}

bbstat<-bb


for (n in 1:length(bbstat)){
  bbstat[[n]] <- bbstat[[n]][(bbstat[[n]]$Rk!="Rk"),]
  pos_mp <- apply(bbstat[[n]][,c("PTS", "TRB", "AST", "STL", "BLK")], 1, score_p)
  neg_mp <- apply(bbstat[[n]][,c("TOV", "FGA", "FG", "FTA", "FT", "PF")], 1, score_n)
  tot_mp <- pos_mp-neg_mp
  bbstat[[n]] <- cbind(bbstat[[n]], pos_mp, neg_mp, tot_mp)
}

#summary((bbstat$`Stephen Curry`)[,c("tot_mp")])

stat <- data.frame(matrix(NA, nrow = 1, ncol =16))
colnames(stat) <- c(names(summary(1)), "games", "sd", "min last 5", "min else", "p value mins", "mean last 5", "mean else", "p value means")
stat <- cbind(names(bbstat), stat)

for (n in 1:length(bbstat)){
  points <- (bbstat[[n]])[,c("tot_mp")]
  mins <- paste0((bbstat[[n]])[,c("MP")])
  mins[mins == "NA"] <- NA
  stat[n,8] <- bbstat[[n]][nrow(bbstat[[n]]),c("G")]
  stat[n,2:7] <- as.numeric(summary(points))[1:6]
  stat[n,9] <- sd(points, na.rm = TRUE)
  clean <- na.omit(points)
  cmins <- na.omit(as.numeric(as.period(ms(mins), unit = "sec"))) / 60
  if(length(clean) > 10){
    stat[n,10] <- mean(cmins[(length(cmins)-5):length(cmins)])
    stat[n,11] <- mean(cmins[1:(length(cmins)-6)])    
    stat[n,12] <- t.test(cmins[(length(cmins)-5):length(cmins)], mu = stat[n,10], alternative = "greater")$p.value
    stat[n,13] <- mean(clean[(length(clean)-5):length(clean)])
    stat[n,14] <- mean(clean[1:(length(clean)-6)])
    stat[n,15] <- t.test(clean[(length(clean)-5):length(clean)], mu = stat[n,13], alternative = "greater")$p.value
  }
}


##not every player matched
overwrite <- function(name){
  name <- as.character(name)
  ind <- as.numeric(regexpr(' ', name))
  
  out <- paste0(substr(name, (ind+1), nchar(name)), ", ", substr(name, 1, (ind-1)))
  
}

setwd("/run/media/atoeroek/Data/basketball.de_data")
bb_raw <- read.delim2("2017end_players_Tab.csv")
#stat$`names(bbstat)` <- sapply(stat$`names(bbstat)`, overwrite)

indn <- as.numeric(sapply(stat$`names(bbstat)`, grep, bb_raw$Spieler))
#cbind(as.character(bb_raw$Spieler[na.omit(indn)]), as.character(stat$`names(bbstat)`[!is.na(indn)]))

stat$pos <- rep(NA, nrow(stat))
stat$gehalt <- rep(NA, nrow(stat))

stat$pos[!is.na(indn)] <- as.character(bb_raw$Pos.[na.omit(indn)])
stat$gehalt[!is.na(indn)] <- as.numeric(as.character(bb_raw$Gehalt[na.omit(indn)]))

stat$ppg <- stat$Mean / stat$gehalt
stat$ppgl <- stat$`mean last 5` / stat$gehalt

statmod <- stat[stat$games > 5 & stat$gehalt > 0.5,]
model <- lm(gehalt ~ Mean, statmod)

stat$value <- sapply(stat$Mean, function(x){model$coefficients[1] + model$coefficients[2] * x})
stat$valuation <- stat$value - stat$gehalt

colnames(stat)[1] <- "Name"
stat <- unique(stat)
stat <- stat[order(stat$ppg, decreasing = TRUE),]

setwd("/run/media/atoeroek/Data/dominateR")
save.image(file = "dominateR2017.RData")


##all players 2016
appURL <- "http://www.basketball-reference.com/leagues/NBA_2016_per_game.html"
lines <- readLines(appURL)
docplayers <- lines[grep("data-append-csv=", lines)]
#string zerlegen

links <- c()
for (i in 1:length(docplayers)){
  links[i] <- paste0("http://www.basketball-reference.com/", substr(docplayers[i], regexpr("players", docplayers[i]), (regexpr("html", docplayers[i])-2)), "/gamelog/2016/")
}
links <- unique(links)


pl <- htmlParse(appURL)
plTable <- pl['//table/tbody']
plHeaders <- unlist(pl["//thead/tr[2]/th", fun = xmlValue])
players <- lapply(plTable, readHTMLTable, header = plHeaders)
players <- data.frame(players)
colnames(players) <- c("Rk", "Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FGp", "3P", "3PA", "3Pp", "2P", "2PA", "2Pp", "eFGp", "FT", "FTA", "FTp", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PSperG")
players <- players[players$Rk!="Rk",]
players1 <- cbind(unique(paste(players$Player)), links)

bb2016 <- as.list(c(rep(0, length(players1[,1]))))
names(bb2016) <- players1[,1]

for (i in 1:length(bb2016)){
  appURL <- players1[i, 2]
  doc <- htmlParse(appURL)
  appTables <- doc['//table/tbody']
  myHeaders <- unlist(doc["//thead/tr[2]/th", fun = xmlValue])
  myTables <- lapply(appTables, readHTMLTable, header = myHeaders)
  person <- data.frame(myTables)
  colnames(person) <- c("Rk", "G", "Date", "Age", "Tm", "-", "Opp", "Res", "GS", "MP", "FG", "FGA", "FGp", "3P", "3PA", "3Pp", "FT", "FTA", "FTp", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "GmSc", "+/-")
  bb2016[[i]] <- person
}


#calculations testing
bb2016test<-bb2016


for (n in 1:length(bb2016test)){
  bb2016test[[n]] <- bb2016test[[n]][(bb2016test[[n]]$Rk!="Rk"),]
  pos_mp <- apply(data.matrix((bb2016test[[n]])[,c("PTS", "TRB", "AST", "STL", "BLK")]), 1, score_p)
  neg_mp <- apply(data.matrix((bb2016test[[n]])[,c("TOV", "FGA", "FG", "FTA", "FT", "PF")]), 1, score_n)
  tot_mp <- pos_mp-neg_mp
  bb2016test[[n]] <- cbind(bb2016test[[n]], pos_mp, neg_mp, tot_mp)
}

#summary((bb2016test$`Stephen Curry`)[,c("tot_mp")])

stat16 <- data.frame(matrix(NA, nrow = 1, ncol =10))
colnames(stat16) <- c(names(summary(1)), "sd", "mean last 5", "mean else", "p value")
stat16 <-cbind(names(bb2016test), stat16)

for (n in 1:length(bb2016test)){
  points <- (bb2016test[[n]])[,c("tot_mp")]
  stat16[n,2:7] <- as.numeric(summary(points))[1:6]
  stat16[n,8] <- sd(points, na.rm = TRUE)
  clean <- na.omit(points)
  if(length(clean)>10){
    stat16[n,9] <- mean(clean[(length(clean)-5):length(clean)])
    stat16[n,10] <- mean(clean[1:(length(clean)-6)])
    stat16[n,11] <- t.test(clean[(length(clean)-5):length(clean)], mu = stat[n,10], alternative = "greater")$p.value
  }
}


team <- function(vec, stat, on){
  
}
#string zerlegen
# 
# usm <- readHTMLTable("http://basketball.de/app/usmanager/top-spieler")
# n.rows <- unlist(lapply(usm, function(t) dim(t)[1]))
# grep("<table>", usm)
# grep("</table>", usm)
# 
# regexpr("<table>",usm)
# regexpr("</table>",usm)
# 
# usmTable <- usm['//table/tbody']
# usmHeaders <- unlist(usm["//thead/tr[2]/th", fun = xmlValue])
# usmplayers <- lapply(usmTable, readHTMLTable, header = usmHeaders)
# usmplayers <- data.frame(usmplayers)
# #colnames(players) <- c("Rk", "Player", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FGp", "3P", "3PA", "3Pp", "2P", "2PA", "2Pp", "eFGp", "FT", "FTA", "FTp", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PSperG")
# #players <- players[players$Rk!="Rk",]
# #players1 <- cbind(unique(paste(players$Player)), links)
# 
# library(rvest)
# theurl <- "http://basketball.de/app/usmanager/top-spieler"
# theurl <- "http://basketball.de/app/usmanager/trades#"
# theurl <- "http://www.basketball-reference.com/leagues/NBA_2016_per_game.html"
# file <- read_html(theurl)
# tables <- html_nodes(file, "table")
# # table1 <- html_table(tables[4], fill = TRUE)