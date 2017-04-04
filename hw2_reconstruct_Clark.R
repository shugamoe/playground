# read in Armchair data
FGXP <- read.csv("nfl_00_16/FGXP.csv")
PBP <- read.csv("nfl_00_16/PBP.csv")
GAME <- read.csv("nfl_00_16/GAME.csv")
PLAYER <- read.csv("nfl_00_16/PLAYER.csv")

# read in Clark data
# Clark <- read.csv("NFL FIeld Goals 2000-2011.csv")

# merge Armchair csv files to create field goal dataframe
df <- merge(FGXP, PBP[,c("pid", "gid")], by="pid")
df <- merge(df, GAME, by="gid")
df <- df[df$fgxp == "FG",]
PLAYER$NAME <- paste(PLAYER$fname, PLAYER$lname)
names(df)[names(df) == 'fkicker'] <- 'player'
df <- merge(df, PLAYER[,c('player', 'NAME')], by="player")
df <- df[with(df, order(pid)),]
names(df)[names(df) == 'player'] <- 'fkicker'
df11 <- df[df$seas <= 2011,]

# create dataframe for field goal attempts for which Armchair has complete temp, wspd, cond data.
# don't need to worry about humd...not in Clark regression!
# rows to keep
keep.a <- which((df$cond == "Closed Roof" | df$cond == "Covered Roof" | df$cond == "Dome"))
keep.b <- which(df$cond != "" & !is.na(df$temp) & !is.na(df$wspd))
keep <- unique(c(keep.a,keep.b))
df2 <- df[keep,]


# initialize dataframe to replicate armchair data analysis
data.colnames <- c('GID', 'PID', 'SEASON', 'POSTSEASON', 'PRECIP', 'COLD49', 'WINDY', 'GRASS', 
                   'ALTITUDE', 'DIST', 'MAKE', 'FKICKER', 'NAME')
data <- data.frame(matrix(ncol = length(data.colnames), nrow = nrow(df2)))
colnames(data) <- data.colnames

# Specify rows for which games are played in closed conditions.
keep2.a <- which((df2$cond == "Closed Roof" | df2$cond == "Covered Roof" | df2$cond == "Dome"))
# Specify all other rows of data set
keep2.nota <- which(!(df2$cond == "Closed Roof" | df2$cond == "Covered Roof" | df2$cond == "Dome"))

# reconstruct Clark data
data$GID <- df2$gid
data$PID <- df2$pid
data$SEASON <- df2$seas
data$POSTSEASON <- ifelse(df2$wk > 17, 1, 0)
#data$AWAY <- skip (not in Clark regression)
#data$PRESSURE <- skip
#data$TO.BEFORE <- skip
# Determine PRECIP
data$PRECIP[keep2.a] <- 0
data$PRECIP[keep2.nota] <- ifelse(df2$cond[keep2.nota] %in% c("Flurries","Light Rain", "Light Showers", "Light Snow", "Rain", "Snow", "Thunderstorms"), 1, 0)
# Determine COLD49
data$COLD49[keep2.a] <- 0
data$COLD49[keep2.nota] <- ifelse(df2$temp[keep2.nota] < 50, 1, 0)
#data$HUMID <- skip
# Determine WINDY
data$WINDY[keep2.a] <- 0
data$WINDY[keep2.nota] <- ifelse(df2$wspd[keep2.nota] >= 10, 1, 0)
data$GRASS <- ifelse(df2$surf == "Grass", 1, 0)
data$ALTITUDE <- ifelse(df2$stad %in% c("Invesco Field at Mile High", "Mile High Stadium", "Sports Authority Field at Mile High"), 1, 0)
data$DIST <- df2$dist
data$MAKE <- df2$good
#data$BLOCKED <- skip
data$FKICKER <- df2$fkicker
data$NAME <- df2$NAME
#data$FG.OF.CAREER <- skip
#data$SEASON.OF.CAREER <- skip

# constrict the reconstructed data to seasons 2000-2011
data11 <- data[data$SEASON <= 2011,]
print(paste("The reconstructed Clark data set contains", nrow(data11), "attempts."))

# write the whole data to a csv file
# write the reconstructed data to a csv file
write.csv(data11, "NFL Field Goals 2000-2011.csv", row.names = FALSE)
write.csv(data, "NFL Field Goals 2000-2016.csv", row.names = FALSE)



# estimate Clark logistic regression on Clark data
# logit.clark<-glm(MAKE ~ DIST+GRASS+COLD49+WINDY+ALTITUDE+PRECIP, family = "binomial",data=Clark)
# summary.glm(logit.clark)

# estimate Clark logistic regression on reconstructed data from 2000-2011
logit.reconstruct <- glm(MAKE ~ DIST+GRASS+COLD49+WINDY+ALTITUDE+PRECIP, family = "binomial",data=data11)
summary.glm(logit.reconstruct)

# save regression output to csv files
#library(broom)
#write.csv(tidy(logit.reconstruct), "reconstructed Clark estimates.csv")
#write.csv(tidy(logit.clark), "original Clark estimates.csv")

