library(rvest)
library(tidyverse)
base_url <- "http://www.spotrac.com/epl/"
read.base <- read_html(base_url)
team.URL <- read.base %>% html_nodes(".team-name") %>% html_attr('href')
team.URL

# Clean up the URLs to get the team names by themselves.
team.names <- gsub(base_url, "", team.URL)
team.names <- gsub("-f.c", " FC", team.names)
team.names <- gsub("afc", "AFC", team.names)
team.names <- gsub("a.f.c", "AFC", team.names)
# Dashes and slashes need to  removed.
team.names <- gsub("-", " ", team.names)
team.names <- gsub("/", "", team.names)
# Fix FC and AFC for Bournemouth
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}
# Capitalise and trim white space
team.names <- sapply(team.names, simpleCap)
#team.names <- sapply(team.names, trimws)
names(team.names) <- NULL
# Now I have a vector of 20 names.
short.names <- gsub(" FC","", team.names)
short.names <- gsub(" AFC","", short.names)
EPL.names <- data.frame(team.names,short.names,team.URL)
EPL.names

team_links <- paste0(team.URL,"payroll/",sep="")

data.creator <- function(link) {
  read_html(link) %>% html_nodes("table") %>% html_table(header=TRUE, fill=TRUE)
}

EPL.salary <- sapply(team_links, function(x) {data.creator(x)})
names(EPL.salary) <- EPL.names$short.names
team.len <- sapply(seq(1,20), function(x) { dim(EPL.salary[[x]][[1]])[[1]]})
Team <- rep(EPL.names$short.names, team.len)
Team <- gsub(".*epl","",Team)
Players <- sapply(seq(1,20), function(x) { str_split(EPL.salary[[x]][[1]][,1], "\t", simplify=TRUE)[,31]})
Position <- sapply(seq(1,20), function(x) { EPL.salary[[x]][[1]][,2]})
Base.Salary <- sapply(seq(1,20), function(x) { Res <- gsub("£", "", EPL.salary[[x]][[1]][,5]); gsub(",","",Res)})
EPL.Result <- data.frame(Players=unlist(Players), Team=Team, Position=unlist(Position), Base.Salary=unlist(Base.Salary))
EPL.Result$Base.Salary <- str_replace(EPL.Result$Base.Salary, "-", NA_character_)
EPL.Result$Base.Num <- as.numeric(EPL.Result$Base.Salary)
EPL.Result$Base.Num <- (EPL.Result$Base.Num)/1000000
EPL.Result %>% group_by(Position) %>% summarise(Mean.Base.Salary=mean(Base.Num, na.rm=TRUE),sdBS=sd(Base.Num, na.rm = TRUE))

EPL.Result %>% group_by(Position,Team) %>% summarise(Mean.Base.Salary=mean(Base.Num, na.rm=TRUE),sdBS=sd(Base.Num, na.rm = TRUE))

EPL.Result %>% group_by(Team) %>% summarise(Mean.Base.Salary=mean(Base.Num, na.rm=TRUE),sdBS=sd(Base.Num, na.rm = TRUE))

fplot <- ggplot(EPL.Result, aes(Base.Num,Team))
gpl <- fplot + geom_jitter(height=0.25, width=0) + facet_wrap(~Position) + labs(x="2020-2021 Base Salary (in £ millions)")
gpl


head(EPL.Result,20)
EPL.Result$Year <- "2020"
EPL.Result <- EPL.Result[c("Year", "Team", "Players","Position","Base.Salary","Base.Num")]
write.csv(EPL.Result, file = "c:\\Users\\Owner\\Documents\\Northwestern\\SportsClass\\Week4\\Assignment1\\2020_2021Payroll.csv", row.names = FALSE)