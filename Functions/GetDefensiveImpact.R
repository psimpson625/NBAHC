
getdefimpactshooting <- function(){

year <- c("2019", "2018", "2017", "2016", "2015", "2014")
yrseason <- c("2018-19", "2017-18", "2016-17", "2015-16", "2014-15", "2013-14")

for(i in 1:length(year)){
defense <- teams_players_stats(seasons = as.integer(year[i]), types = c("player"),
                               modes = c("Totals"),
                               tables = c("shots"),
                               closest_defender_ranges = c("0-2 Feet - Very Tight", "2-4 Feet - Tight", "4-6 Feet - Open", "6%2B Feet - Wide Open"),
                               dribble_ranges = c("0 Dribbles", "1 Dribble", "2 Dribbles", "3-6 Dribbles", "7%2B Dribbles"),
                               assign_to_environment = FALSE)
# Placing each in individual df
vt0 <- defense[[7]][[1]]
vt1 <- defense[[7]][[2]]
vt2 <- defense[[7]][[3]]
vt36 <- defense[[7]][[4]]
vt7 <- defense[[7]][[5]]
t0 <- defense[[7]][[6]]
t1 <- defense[[7]][[7]]
t2 <- defense[[7]][[8]]
t36 <- defense[[7]][[9]]
t7 <- defense[[7]][[10]]
o0 <- defense[[7]][[11]]
o1 <- defense[[7]][[12]]
o2 <- defense[[7]][[13]]
o36 <- defense[[7]][[14]]
o7 <- defense[[7]][[15]]
wo0 <- defense[[7]][[16]]
wo1 <- defense[[7]][[17]]
wo2 <- defense[[7]][[18]]
wo36 <- defense[[7]][[19]]
wo7 <- defense[[7]][[20]]
remove(defense)
# Adding how open the shots were
vt0$CloseDefDistRange <- as.character(vt0$CloseDefDistRange)
vt0$CloseDefDistRange <- replace(vt0$CloseDefDistRange, vt0$CloseDefDistRange == "FALSE", "0-2 Feet - Very Tight")
vt1$CloseDefDistRange <- as.character(vt1$CloseDefDistRange)
vt1$CloseDefDistRange <- replace(vt1$CloseDefDistRange, vt1$CloseDefDistRange == "FALSE", "0-2 Feet - Very Tight")
vt2$CloseDefDistRange <- as.character(vt2$CloseDefDistRange)
vt2$CloseDefDistRange <- replace(vt2$CloseDefDistRange, vt2$CloseDefDistRange == "FALSE", "0-2 Feet - Very Tight")
vt36$CloseDefDistRange <- as.character(vt36$CloseDefDistRange)
vt36$CloseDefDistRange <- replace(vt36$CloseDefDistRange, vt36$CloseDefDistRange == "FALSE", "0-2 Feet - Very Tight")
vt7$CloseDefDistRange <- as.character(vt7$CloseDefDistRange)
vt7$CloseDefDistRange <- replace(vt7$CloseDefDistRange, vt7$CloseDefDistRange == "FALSE", "0-2 Feet - Very Tight")
t0$CloseDefDistRange <- as.character(t0$CloseDefDistRange)
t0$CloseDefDistRange <- replace(t0$CloseDefDistRange, t0$CloseDefDistRange == "FALSE", "2-4 Feet - Tight")
t1$CloseDefDistRange <- as.character(t1$CloseDefDistRange)
t1$CloseDefDistRange <- replace(t1$CloseDefDistRange, t1$CloseDefDistRange == "FALSE", "2-4 Feet - Tight")
t2$CloseDefDistRange <- as.character(t2$CloseDefDistRange)
t2$CloseDefDistRange <- replace(t2$CloseDefDistRange, t2$CloseDefDistRange == "FALSE", "2-4 Feet - Tight")
t36$CloseDefDistRange <- as.character(t36$CloseDefDistRange)
t36$CloseDefDistRange <- replace(t36$CloseDefDistRange, t36$CloseDefDistRange == "FALSE", "2-4 Feet - Tight")
t7$CloseDefDistRange <- as.character(t7$CloseDefDistRange)
t7$CloseDefDistRange <- replace(t7$CloseDefDistRange, t7$CloseDefDistRange == "FALSE", "2-4 Feet - Tight")
o0$CloseDefDistRange <- as.character(o0$CloseDefDistRange)
o0$CloseDefDistRange <- replace(o0$CloseDefDistRange, o0$CloseDefDistRange == "FALSE", "4-6 Feet - Open")
o1$CloseDefDistRange <- as.character(o1$CloseDefDistRange)
o1$CloseDefDistRange <- replace(o1$CloseDefDistRange, o1$CloseDefDistRange == "FALSE", "4-6 Feet - Open")
o2$CloseDefDistRange <- as.character(o2$CloseDefDistRange)
o2$CloseDefDistRange <- replace(o2$CloseDefDistRange, o2$CloseDefDistRange == "FALSE", "4-6 Feet - Open")
o36$CloseDefDistRange <- as.character(o36$CloseDefDistRange)
o36$CloseDefDistRange <- replace(o36$CloseDefDistRange, o36$CloseDefDistRange == "FALSE", "4-6 Feet - Open")
o7$CloseDefDistRange <- as.character(o7$CloseDefDistRange)
o7$CloseDefDistRange <- replace(o7$CloseDefDistRange, o7$CloseDefDistRange == "FALSE", "4-6 Feet - Open")
wo0$CloseDefDistRange <- as.character(wo0$CloseDefDistRange)
wo0$CloseDefDistRange <- replace(wo0$CloseDefDistRange, wo0$CloseDefDistRange == "FALSE", "6+ Feet - Wide Open")
wo1$CloseDefDistRange <- as.character(wo1$CloseDefDistRange)
wo1$CloseDefDistRange <- replace(wo1$CloseDefDistRange, wo1$CloseDefDistRange == "FALSE", "6+ Feet - Wide Open")
wo2$CloseDefDistRange <- as.character(wo2$CloseDefDistRange)
wo2$CloseDefDistRange <- replace(wo2$CloseDefDistRange, wo2$CloseDefDistRange == "FALSE", "6+ Feet - Wide Open")
wo36$CloseDefDistRange <- as.character(wo36$CloseDefDistRange)
wo36$CloseDefDistRange <- replace(wo36$CloseDefDistRange, wo36$CloseDefDistRange == "FALSE", "6+ Feet - Wide Open")
wo7$CloseDefDistRange <- as.character(wo7$CloseDefDistRange)
wo7$CloseDefDistRange <- replace(wo7$CloseDefDistRange, wo7$CloseDefDistRange == "FALSE", "6+ Feet - Wide Open")

# Group dfs by how open the shots were
vt <- rbind(vt0, vt1, vt2, vt36, vt7)
t <- rbind(t0, t1, t2, t36, t7)
o <- rbind(o0, o1, o2, o36, o7)
wo <- rbind(wo0, wo1, wo2, wo36, wo7)
remove(vt0, vt1, vt2, vt36, vt7, t0, t1, t2, t36, t7, o0, o1, o2, o36, o7, wo0, wo1, wo2, wo36, wo7)

# Creating master df for year:
masteryr <- rbind(vt, t, o , wo)
masteryr <- mutate(masteryr, Season = yrseason[i])
remove(vt, t, o, wo)

# Depending on year::
if(i == 1){
  defenseeffect <- masteryr
} else{
  helper <- masteryr
  defenseeffect <- rbind(defenseeffect, helper)
}
remove(masteryr, helper)
i = i + 1
}

write.csv(defenseeffect, "/Users/patricksimpson/Desktop/NBAclustering/dribbling-defense.csv")
}
