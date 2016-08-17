### NBA player's performance data

library(XML)
library(rvest)
library(stringr)
library(stringi)
load(file = 'playerList.Rda')

## extracting playerList, cleaning
data.player <- read.csv(file = 'Draft2006-2015.csv', stringsAsFactors = F)
data.player = data.player[-c(1,32),]
playerList = c()
for (i in 1:10) {
      playerList = c(playerList, data.player[,i])
}
playerList = gsub('\\.', '', playerList)
playerList = gsub("'", '', playerList)
playerList = gsub(' ', '-', playerList)

## creating empty dataframe
data.pergame <- matrix(data = NA, nrow = 400, ncol = 28)
data.pergame = as.data.frame(data.pergame)
colnames(data.pergame) <- c('Name','Height','Weight', 'HighSchool',
                            'Season', 'School', 'Conf', 'G', 'MP', 'FG', 'FGA', 'FG%',
                            '2P', '2PA', '2P%', '3P', '3PA', '3P%', 'FT', 'FTA',' FT%',
                            'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS')
data.advanced <- matrix(data = NA, nrow = 400, ncol = 29)
data.advanced = as.data.frame(data.advanced)
colnames(data.advanced) <- c('Name', 'Season', 'School', 'Conf', 'G', 'MP', 'PER', 'TS%', 'eFG%',
                             '3PAr', 'FTr', 'PProd', 'ORB%', 'DRB%', 'TRB%', 'AST%', 'STL%',
                             'BLK%', 'TOV%', 'USG%', 'empty1', 'OWS', 'DWS', 'WS',' WS/40',
                             'empty2', 'OBPM', 'DBPM', 'BPM')

## getting data
countnum = 0
for (player in playerList) {
      
      countnum = countnum + 1
      
      nameInsert = tolower(player)
      fileUrl <- paste0('http://www.sports-reference.com/cbb/players/',nameInsert,'-1.html')
      web <- try(read_html(fileUrl), silent = T)
      if (identical(class(web), "try-error")){
            next()
      }
      
      item.current <- html_nodes(web, '#players_per_game .stat_total td') %>% html_text()
      if (length(item.current) != 0) {
            prof <- html_nodes(web, '.margin0:nth-child(7)') %>% html_text()
            height = str_extract(prof, '\\d\\-\\d')
            if (length(height) == 0) { height = NA }
            weight = str_extract(prof, '\\d{3,3}')
            if (length(weight) == 0) { weight = NA }
            highSchool <- html_nodes(web, '.margin0~ .margin0+ .margin0') %>% html_text()
            highSchool = gsub('High School: ', '', highSchool)
            if (length(highSchool) == 0) { highSchool = NA }
            item.current = c(player, height, weight, highSchool, item.current)
            data.pergame[countnum,] = item.current
      }
      else {
            next()
      }

      item.current2 <- html_nodes(web, '#players_advanced .stat_total:nth-child(1) td') %>% html_text()
      if (length(item.current2) != 0) {
            item.current2 = c(player, item.current2)
            data.advanced[countnum,] = item.current2
      }
      else {
            next()
      }
      
      print(countnum)
      
      if (countnum %% 50 == 0){
            Sys.sleep(20)
      }
}

sum(is.na(data.advanced$Name))
sum(is.na(data.pergame$Name))

## clean empty columns
data.advanced$empty1 = NULL
data.advanced$empty2 = NULL
playerList = c()
for (i in 1:10) {
      playerList = c(playerList, data.player[,i])
}
data.advanced$Name = playerList
data.pergame$Name = playerList


# save(data.pergame, file = 'data.pergame.Rda')
# save(data.advanced, file = 'data.advanced.Rda')
# save(playerList, file ='playerList.Rda')

