library(r2r)

# constants
K <- 20
REVERT <- 1 / 3.0

MOV_PARAM_1 <- 0.001
MOV_PARAM_2 <- 2.2

HF_ADJ_1 <- 65.0 # 48
HF_ADJ_2 <- 4 / 1000

REVERSIONS <- hashmap()
REVERSIONS[c('CBD1925', 'RAC1926', 'LOU1926', 'CIB1927', 'MNN1929', 'BFF1929', 'LAR1944', 'PHI1944', 'ARI1945', 'PIT1945', 'CLE1999')] = c(1502.032, 1403.384, 1307.201, 1362.919, 1306.702, 1331.943, 1373.977, 1497.988, 1353.939, 1353.939, 1300.0)

#elo_odds <- function(team_elos, teamA, teamB) {
#    base_elo_diff <- as.double(team_elos[teamB][2]) -
#                     as.double(team_elos[teamA][2])
#
#    elo_diff <- base_elo_diff
#    odds <- 1 / (10^(elo_diff / 400) + 1)
#    return(odds)
#}


forecast <- function(games) {
    # test stuff
    #games["my_prob1"] <- games["elo_prob1"]

    team_elos <- read.csv(
        "nfl-elo-game/data/initial_elos.csv",
        colClasses = c("character", "double")
    )
    team_elos["season"] <- rep.int(1920, nrow(team_elos))

    for (i in 1:nrow(games)) {
        for (team_name in list(games$team1[i], games$team2[i])) {
            #print(team)
            team_index <- team_elos$team == team_name
            if (games$season[i] != team_elos[team_index, ]$season) {
                #print(team_elos[team_elos$team == games$team1[i], ]$elo)
                k <- paste(team_name, games[i, 'season'], sep = "")
                if (!is.null(REVERSIONS[k][[1]])) {
                    #print(REVERSIONS[k1][[1]])
                    team_elos[team_index, ]$elo <- REVERSIONS[k][[1]]
                }
                else {
                    #print(team_elos[team_elos$team == team_name, ]$elo)
                    team_elos[team_index, ]$elo <- 1505.0 * REVERT + team_elos[team_elos$team == team_name, ]$elo * (1-REVERT)
                    #print(team_elos[team_elos$team == team_name, ]$elo)
                }
                team_elos[team_index, ]$season = games[i, "season"]
            }
        }

        #game <- games[i, ]

        #odds <- elo_odds(
        #    team_elos,
        #    team_elos["team"] == games[i, "team1"],
        #    team_elos["team"] == games[i, "team2"]
        #)
        teamA <- team_elos["team"] == games[i, "team1"]
        teamB <- team_elos["team"] == games[i, "team2"]
        
        base_elo_diff <- as.double(team_elos[teamB][2]) -
                         as.double(team_elos[teamA][2])

        home_field_adjustment <- if (games[i, "neutral"] == 1) 0 else -1 * HF_ADJ_1 



        elo_diff <- base_elo_diff + home_field_adjustment

        odds <- 1 / (10^(elo_diff / 400) + 1)

        games[i, "my_prob1"] <- odds

        winner_elo_diff <- games$result1[i] - odds

        mov <- log(max(abs(games$score1[i] - games$score2[i]), 1) + 1.0) * MOV_PARAM_2 / (winner_elo_diff * MOV_PARAM_1 + MOV_PARAM_2)

        shift <- K * winner_elo_diff * mov

        team_elos[team_elos$team == games$team1[i], ]$elo <- team_elos[
            team_elos$team == games$team1[i],
        ]$elo + shift
        team_elos[team_elos$team == games$team2[i], ]$elo <- team_elos[
            team_elos$team == games$team2[i],
        ]$elo - shift
        #print(team_elos[team_elos$team == games$team1[i], ])
        #quit()
    }

    return(games)
}