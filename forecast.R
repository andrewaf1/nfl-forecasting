# constants
K <- 20
REVERT <- 1 / 3.0

elo_odds <- function(team_elos, teamA, teamB) {
    base_elo_diff <- as.double(team_elos[teamB][2]) -
                     as.double(team_elos[teamA][2])

    elo_diff <- base_elo_diff
    odds <- 1 / (10^(elo_diff / 400) + 1)
    return(odds)
}


forecast <- function(games) {
    # test stuff
    #games["my_prob1"] <- games["elo_prob1"]

    team_elos <- read.csv(
        "nfl-elo-game/data/initial_elos.csv",
        colClasses = c("character", "double")
    )

    season <- 1920
    for (i in 1:nrow(games)) {
        if (games$season[i] > season) {
            print(team_elos[team_elos$team == games$team1[i], ]$elo)
            team_elos[team_elos$team == games$team1[i], ]$elo <- 1505.0 * REVERT + team_elos[team_elos$team == games$team1[i], ]$elo * (1-REVERT)
            season <- games$season[i]
            print(team_elos[team_elos$team == games$team1[i], ]$elo)
        }

        odds <- elo_odds(
            team_elos,
            team_elos["team"] == games[i, "team1"],
            team_elos["team"] == games[i, "team2"]
        )
        games[i, "my_prob1"] <- odds

        #print(team_elos)
        #print(team_elos[team_elos$team == games$team1[i], ])
        #score_diff <- abs(games$score1[i] - games$score2[i])
        #score_odds <- log(max(score_diff, 1) + 1.0) / 2.2

        #print(odds)
        #print(score_odds)
        #quit()

        shift <- K * (games$result1[i] - odds)# * if (games$result1[i]) 1 else -1

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