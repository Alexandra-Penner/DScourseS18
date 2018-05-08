# stolen code
# from paulgp


a <- pbp2016 %>% group_by(GameID, posteam) %>% summarize(top = sum(PlayTimeDiff), end_score = max(PosTeamScore, na.rm=TRUE), numplays = n()) %>% filter(posteam != "NA") %>% group_by(GameID) %>% mutate(teamNum = row_number()) %>% select(GameID, top, numplays, teamNum, end_score)
score <- a %>% select(GameID, teamNum, end_score) %>% spread(teamNum, end_score, sep="_")
numplays <- a %>% select(GameID, teamNum, numplays) %>% spread(teamNum, numplays, sep="_")
top <- a %>% select(GameID, teamNum, top) %>% spread(teamNum, top, sep="_")
colnames(top) <- c("GameID", "top_1", "top_2")
colnames(score) <- c("GameID", "score_1", "score_2")
colnames(numplays) <- c("GameID", "numplays_1", "numplays_2")
final <- inner_join(numplays,score, by="GameID") %>% inner_join(top, by="GameID")  %>% mutate(winner_1 = as.integer(score_1 > score_2))

library(binscattr)
binscatter(data=final %>% mutate(top_diff = (top_1 - top_2)/60), y = winner_1, x = top_diff)
binscatter(data=final %>% mutate(play_diff = numplays_1 - numplays_2), y = winner_1, x = play_diff)


b <- pbp_2016 %>% group_by(GameID, posteam, qtr) %>% summarize(top = sum(PlayTimeDiff), end_score = max(PosTeamScore, na.rm=TRUE), numplays = n()) %>% filter(qtr == 1) %>% filter(posteam != "NA") %>% group_by(GameID) %>% mutate(teamNum = row_number()) %>% select(GameID, top, numplays, teamNum, end_score)
numplays <- b %>% select(GameID, teamNum, numplays) %>% spread(teamNum, numplays, sep="_")
top <- b %>% select(GameID, teamNum, top) %>% spread(teamNum, top, sep="_")
colnames(top) <- c("GameID", "top_1", "top_2")
colnames(numplays) <- c("GameID", "numplays_1", "numplays_2")


binscatter(data=final %>% mutate(play_diff = numplays_1 - numplays_2), y = winner_1, x = play_diff)
binscatter(data=final %>% mutate(top_diff = (top_1 - top_2)/60), y = winner_1, x = top_diff)


