
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
appURL <- "http://www.basketball-reference.com/leagues/NBA_2017_per_game.html"
lines <- readLines(appURL)
docplayers <- lines[grep("data-append-csv=", lines)]
#string zerlegen

links <- c()
for (i in 1:length(docplayers)){
  links[i] <- paste0("http://www.basketball-reference.com/", substr(docplayers[i], regexpr("players", docplayers[i]), (regexpr("html", docplayers[i])-2)), "/gamelog/2017/")
}

pl <- htmlParse(appURL)
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
  appURL <- players$links[i]
  doc <- htmlParse(appURL)
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

summary((bbstat$`Stephen Curry`)[,c("tot_mp")])

stat <- data.frame(matrix(NA, nrow = 1, ncol =10))
colnames(stat) <- c(names(summary(1)), "sd", "mean last 5", "mean else", "p value")
stat<-cbind(names(bbstat), stat)

for (n in 1:length(bbstat)){
  points <- (bbstat[[n]])[,c("tot_mp")]
  stat[n,2:7] <- as.numeric(summary(points))[1:6]
  stat[n,8] <- sd(points, na.rm = TRUE)
  clean <- na.omit(points)
  if(length(clean)>10){
    stat[n,9] <- mean(clean[(length(clean)-5):length(clean)])
    stat[n,10] <- mean(clean[1:(length(clean)-6)])
    stat[n,11] <- t.test(clean[(length(clean)-5):length(clean)], mu = stat[n,10], alternative = "greater")$p.value
  }
}

stat <- stat[unique(stat$`names(bbstat)`),]

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

save.image(file = "dominateR.RData")

bbURL <- "http://basketball.de/app/usmanager/top-spieler"
bblines <- readLines(bbURL)
docplayers <- bblines[grep("data-append-csv=", lines)]

overwrite <- function(name){
  name <- as.character(name)
  ind <- as.numeric(regexpr(' ', name))
  
  out <- paste0(substr(name, (ind+1), nchar(name)), ", ", substr(name, 1, (ind-1)))
  
}

stat$`names(bbstat)` <- sapply(stat$`names(bbstat)`, overwrite)
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
library(rvest)
theurl <- "http://basketball.de/app/usmanager/top-spieler"
theurl <- "http://basketball.de/app/usmanager/trades#"
theurl <- "http://www.basketball-reference.com/leagues/NBA_2016_per_game.html"
file <- read_html(theurl)
tables <- html_nodes(file, "table")
# table1 <- html_table(tables[4], fill = TRUE)