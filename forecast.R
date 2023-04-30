library(r2r)
library(parallel)

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

forecast_saved_params <- function(games) {
    accepted <- read.csv("accept.csv")
    plot_dists(accepted)
    #print(nrow(accepted))
    accepted <- accepted[accepted$losses < 0.458, ]
    #print(nrow(accepted))
    #return(forecast(games, mean(accepted$k), mean(accepted$revert), mean(accepted$mov_param_1), mean(accepted$mov_param_2), mean(accepted$hf_adj_1)))

    accept["combined"] <- apply(as.list(as.data.frame(t(accept[["k", "revert", "mov_param_1", "mov_param_2", "hf_adj_1"]]))), mean)

}

forecast_default_params <- function(games) {
    # constants
    K <- 20
    REVERT <- 1 / 3.0

    MOV_PARAM_1 <- 0.001
    MOV_PARAM_2 <- 2.2

    HF_ADJ_1 <- 65.0 # 48
    #HF_ADJ_2 <- 4 / 1000

    return(forecast(games, K, REVERT , MOV_PARAM_1, MOV_PARAM_2, HF_ADJ_1))
}

plot_dists <- function(accept) {
    if (nrow(accept) >= 1) {
        write.csv(accept, "accept.csv")

        jpeg(file="imgs/accept_k.jpeg")
        hist(accept$k, prob=TRUE)
        dev.off()

        jpeg(file="imgs/accept_revert.jpeg")
        hist(accept$revert, prob=TRUE)
        dev.off()

        jpeg(file="imgs/accept_mov_param_1.jpeg")
        hist(accept$mov_param_1, prob=TRUE)
        dev.off()

        jpeg(file="imgs/accept_mov_param_2.jpeg")
        hist(accept$mov_param_2, prob=TRUE)
        dev.off()

        jpeg(file="imgs/accept_hf_adj_1.jpeg")
        hist(accept$hf_adj_1, prob=TRUE)
        dev.off()
    }
}


forecast_abc_parallel <- function(games) {
    ACCEPT_MARGIN <- 0.46
    N_RUNS <- 10000
    # constants
    K <- 20
    REVERT <- 1 / 3.0
    MOV_PARAM_1 <- 0.001
    MOV_PARAM_2 <- 2.2
    HF_ADJ_1 <- 65.0 # 48

    k <- pmax(1e-5, rnorm(N_RUNS, K, K))
    revert <- pmax(1e-5, rnorm(N_RUNS, REVERT, REVERT))
    mov_param_1 <- pmax(1e-5, rnorm(N_RUNS, MOV_PARAM_1, MOV_PARAM_1))
    mov_param_2 <- pmax(1e-5, rnorm(N_RUNS, MOV_PARAM_2, MOV_PARAM_2))
    hf_adj_1 <- pmax(1e-5, rnorm(N_RUNS, HF_ADJ_1, HF_ADJ_1 / 2))
    #revert <- rep(REVERT, N_RUNS)
    #mov_param_1 = rep(MOV_PARAM_1, N_RUNS)
    #mov_param_2 = rep(MOV_PARAM_2, N_RUNS)
    #hf_adj_1 = rep(HF_ADJ_1, N_RUNS)

    #k <- runif(N_RUNS, 0, 2 * K)
    #revert <- runif(N_RUNS, 0, 2 * REVERT)
    #mov_param_1 <- runif(N_RUNS, 0, 2 * MOV_PARAM_1)
    #mov_param_2 <- runif(N_RUNS, 0, 2 * MOV_PARAM_2)
    #hf_adj_1 <- runif(N_RUNS, 0, 2 * HF_ADJ_1)

    #jpeg(file="imgs/prior.jpeg")
    #hist(hf_adj_1, prob=TRUE)
    #dev.off()
    #quit()

    #params <- as.list(as.data.frame(t(matrix(c(k, revert, mov_param_1, mov_param_2, hf_adj_1), ncol=5))))
    #func <- (function(params) (function(result) sqrt(mean((result$my_prob1 - result$result1)^2)))(forecast(games, params[1], params[2], params[3], params[4], params[5])))
    #losses <- mclapply(params, func)

    # beating my PR for most cursed line of code written
    losses <- mclapply(as.list(as.data.frame(t(matrix(c(k, revert, mov_param_1, mov_param_2, hf_adj_1), ncol=5)))), (function(params) (function(result) sqrt(mean((result$my_prob1 - result$result1)^2)))(forecast(games, params[1], params[2], params[3], params[4], params[5]))))

    #print(losses)
    #quit()

    accept_i <- !is.na(losses) & losses < ACCEPT_MARGIN
    accept <- data.frame(losses=unlist(losses[accept_i]), k=k[accept_i], revert = revert[accept_i], mov_param_1 = mov_param_1[accept_i], mov_param_2 = mov_param_2[accept_i], hf_adj_1 = hf_adj_1[accept_i])

    print(paste(N_RUNS, "iterations completed with accept rate", sum(accept_i) / length(accept_i)))

    plot_dists(accept)

    return(forecast(games, mean(accept$k), mean(accept$revert), mean(accept$mov_param_1), mean(accept$mov_param_2), mean(accept$hf_adj_1)))

}


forecast_abc <- function(games) {
    ACCEPT_MARGIN <- 0.4
    N_RUNS <- 10
    # constants
    K <- 20
    REVERT <- 1 / 3.0
    MOV_PARAM_1 <- 0.001
    MOV_PARAM_2 <- 2.2
    HF_ADJ_1 <- 65.0 # 48

    accept <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(accept) <- c("k", "revert", "mov_param_1", "mov_param_2", "hf_adj_1")

    for (i in 1:N_RUNS) {
        k <- max(1e-5, rnorm(1, K, K))
        #revert <- max(1e-5, rnorm(1, REVERT, REVERT))
        #mov_param_1 <- max(1e-5, rnorm(1, MOV_PARAM_1, MOV_PARAM_1))
        #mov_param_2 <- max(1e-5, rnorm(1, MOV_PARAM_2, MOV_PARAM_2))
        #hf_adj_1 <- max(1e-5, rnorm(1, HF_ADJ_1, HF_ADJ_1))

        #k <- runif(1, 0, 2*K)
        #revert <- runif(1, 0, 2*REVERT)
        #mov_param_1 <- runif(1, 0, 2*MOV_PARAM_1)
        #mov_param_2 <- runif(1, 0, 2*MOV_PARAM_2)
        #hf_adj_1 <- runif(1, 0, 2*HF_ADJ_1)

        #k <- rep(K, 1)
        revert <- rep(REVERT, 1)
        mov_param_1 = rep(MOV_PARAM_1, 1)
        mov_param_2 = rep(MOV_PARAM_2, 1)
        hf_adj_1 = rep(HF_ADJ_1, 1)

        #print(c(k, revert, mov_param_1, mov_param_2, hf_adj_1))

        games_with_preds <- forecast(games, k, revert , mov_param_1, mov_param_2, hf_adj_1)
        #print(games_with_preds$my_prob1)
        #print(is.nan(games_with_preds$my_prob1))
        #print(nrow(games_with_preds))
        games_with_preds <- games_with_preds[!is.nan(games_with_preds$my_prob1) & !is.nan(games_with_preds$result1), ]
        #print(nrow(games_with_preds))
        #loss = 1 - sum(lapply(
        #    games_with_preds$my_prob1,
        #    (function(x) if (!is.nan(x) & x >= 0.5) 1 else 0)
        #) == games_with_preds$result1) / nrow(games_with_preds)
        loss = mean(abs(games_with_preds$my_prob1 - games_with_preds$result1))
        #print(loss)

        if (!is.na(loss) & loss < ACCEPT_MARGIN) {
            accept[nrow(accept) + 1,] <- c(k, revert, mov_param_1, mov_param_2, hf_adj_1)
        }

        if (i %% 1 == 0) {
            print(paste("Iteration", i, "out of", N_RUNS, "-", nrow(accept), "Accepted"))
            plot_dists(accept)
        }
    }
    print(accept)

    plot_dists(accept)

    return(forecast(games, mean(accept$k), mean(accept$revert), mean(accept$mov_param_1), mean(accept$mov_param_2), mean(accept$hf_adj_1)))
}


forecast_abc_smc <- function(games) {
    T_MAX <- 2
    N_MAX <- 2
    K <- 20
    REVERT <- 1 / 3.0
    MOV_PARAM_1 <- 0.001
    MOV_PARAM_2 <- 2.2
    HF_ADJ_1 <- 65.0 # 48

    SIGMA <- 0.1

    #accept <- data.frame(matrix(ncol = 5, nrow = 0))
    #colnames(accept) <- c(
    #    "k", "revert", "mov_param_1", "mov_param_2", "hf_adj_1"
    #)

    accept_margin <- seq(0.45, 0.40, -0.1 / T_MAX)
    #accept_margin <- rep(0.45, T_MAX)

    params <- list()
    weights <- list()

    for (t in 1:T_MAX) {
        params <- append(
            params,
            list(
                data.frame(
                    k = double(),
                    revert = double(),
                    mov_param_1 = double(),
                    mov_param_2 = double(),
                    hf_adj_1 = double()
                )
            )
        )
        print(colnames(params[[t]]))
        i <- 1
        while (i <= N_MAX) {
            if (t == 1) {
                new_params <- c(
                    k = runif(1, 0, 2 * K),
                    revert = runif(1, 0, 2 * REVERT),
                    mov_param_1 = runif(1, 0, 2 * MOV_PARAM_1),
                    mov_param_2 = runif(1, 0, 2 * MOV_PARAM_2),
                    hf_adj_1 = runif(1, 0, 2 * HF_ADJ_1)
                )
            } else {
                new_params <- c(
                    k = sample(params[[t - 1]]$k, 1)
                        + SIGMA * runif(1, -1, 1),
                    revert = sample(params[[t - 1]]$revert, 1)
                        + SIGMA * runif(1, -1, 1),
                    mov_param_1 = sample(params[[t - 1]]$mov_param_1, 1)
                        + SIGMA * runif(1, -1, 1),
                    mov_param_2 = sample(params[[t - 1]]$mov_param_2, 1)
                        + SIGMA * runif(1, -1, 1),
                    hf_adj_1 = sample(params[[t - 1]]$hf_adj_1, 1)
                        + SIGMA * runif(1, -1, 1)
                )
            }

            print(params)
            #print(params[[t]])

            games_with_preds <- forecast(
                games,
                new_params["k"],
                new_params["revert"],
                new_params["mov_param_1"],
                new_params["mov_param_2"],
                new_params["hf_adj_1"]
            )
            games_with_preds <- games_with_preds[
                !is.nan(games_with_preds$my_prob1) &
                !is.nan(games_with_preds$result1),
            ]

            loss <- mean(abs(
                games_with_preds$my_prob1 - games_with_preds$result1
            ))
            #print(c(loss, accept_margin[[t]]))
            #print(loss < accept_margin[[t]])
            #quit()
            if (loss < accept_margin[[t]]) {
                params[[t]][i, ] <- new_params
                i <- i + 1
            }

            #quit()
        }

        if (t == 1) {
            weights[[t]] <- rep(1, N_MAX)
        }
        else {
            weights[[t]] <- 1
        }

        #if (!is.na(loss) && loss < accept_margin) {
        #    accept[nrow(accept) + 1, ] <- c(
        #        params[[t]][i, ]$k, params[[t]][i, ]$revert, params[[t]][i, ]$mov_param_1, params[[t]][i, ]$mov_param_2, params[[t]][i, ]$hf_adj_1
        #    )
        #}

        if (t %% 1 == 0) {
            print(
                paste("Iteration", t, "out of", T_MAX)#,
                #"-", nrow(accept), "Accepted")
            )
            plot_dists(params[t], prefix = t)
        }
    }
    print(accept)

    plot_dists(accept)

    return(
        forecast(
            games,
            mean(params[[T_MAX]]$k),
            mean(params[[T_MAX]]$revert),
            mean(params[[T_MAX]]$mov_param_1),
            mean(params[[T_MAX]]$mov_param_2),
            mean(params[[T_MAX]]$hf_adj_1)
        )
    )
}


forecast <- function(games, K, REVERT , MOV_PARAM_1, MOV_PARAM_2, HF_ADJ_1) {
    #Rprof()
    # test stuff
    #games["my_prob1"] <- games["elo_prob1"]

    team_elos_df <- read.csv(
        "nfl-elo-game/data/initial_elos.csv",
        colClasses = c("character", "double")
    )
    team_elos_df["season"] <- rep.int(1920, nrow(team_elos_df))


    team_elos <- new.env(hash=TRUE)

    for (team in team_elos_df$team) {
        team_elos[[team]] <- team_elos_df[team_elos_df == team, ]
    }

    for (i in 1:nrow(games)) {
        for (team_name in list(games$team1[i], games$team2[i])) {
            #print(team)
            #team_index <- team_elos$team == team_name
            if (games$season[i] != team_elos[[team_name]]$season) {
                #print(team_elos[team_elos$team == games$team1[i], ]$elo)
                k <- paste(team_name, games[i, 'season'], sep = "")
                if (!is.null(REVERSIONS[k][[1]])) {
                    #print(REVERSIONS[k][[1]])
                    team_elos[[team_name]]$elo = REVERSIONS[k][[1]]
                }
                else {
                    #print(team_elos[team_elos$team == team_name, ]$elo)
                    team_elos[[team_name]]$elo = 1505.0 * REVERT + team_elos[[team_name]]$elo * (1-REVERT)
                    #print(team_elos[team_elos$team == team_name, ]$elo)
                }
                team_elos[[team_name]]$season = games[i, "season"]
            }
        }

        teamA <- games[i, "team1"]
        teamB <- games[i, "team2"]
        
        base_elo_diff <- as.double(team_elos[[teamB]]$elo) -
                         as.double(team_elos[[teamA]]$elo)

        home_field_adjustment <- if (games[i, "neutral"] == 1) 0 else -1 * HF_ADJ_1 

        elo_diff <- base_elo_diff + home_field_adjustment

        odds <- 1 / (10^(elo_diff / 400) + 1)


        games[i, "my_prob1"] <- odds

        winner_elo_diff <- games$result1[i] - odds

        mov <- log(max(abs(games$score1[i] - games$score2[i]), 1) + 1.0) * MOV_PARAM_2 / (winner_elo_diff * MOV_PARAM_1 + MOV_PARAM_2)

        shift <- K * winner_elo_diff * mov

        #print(team_elos[teamA]$elo)

        team_elos[[teamA]]$elo = team_elos[[teamA]]$elo + shift
        team_elos[[teamB]]$elo = team_elos[[teamB]]$elo - shift

        #print(team_elos[teamA]$elo)
        #quit()
    }

    #print(summaryRprof())

    return(games)
}