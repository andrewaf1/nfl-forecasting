import rpy2.robjects as robjects
import pandas as pd
from rpy2.robjects import pandas2ri
import sys
from rpy2.robjects.conversion import rpy2py
from sklearn.calibration import calibration_curve
import matplotlib.pyplot as plt

sys.path.append("./nfl-elo-game")

from util import *
from forecast import *

# rpy2 stuff
pandas2ri.activate()
r_source = robjects.r["source"]
forecast = r_source("./forecast.R")

# Read historical games from CSV
games = pd.DataFrame(Util.read_games("nfl-elo-game/data/nfl_games.csv"))

test_i = games.season > 2016
games_train = games[~test_i]
games_test = games[test_i]

# Forecast every game using the R code
#result = list(robjects.r("forecast")(games_train))
#result = list(robjects.r("forecast_default_params")(games_train))
#result  = list(robjects.r("forecast_abc")(games_train))
#result  = list(robjects.r("forecast_abc_parallel")(games_train))
result = list(robjects.r("forecast_abc_smc")(games_train))
#result = list(robjects.r("forecast_saved_params")(games_train, "abcsmc-5-accept.csv"))

final_elos, best_params = result[1], result[2:]

"""
BINS = 100

games_df_eval = games_df[games_df.result1 != 0.5]
prob_true, prob_pred_my = calibration_curve(games_df_eval.result1, games_df_eval.my_prob1, n_bins=BINS)
print(math.sqrt(((prob_true - prob_pred_my)**2).mean()))

prob_true, prob_pred_elo = calibration_curve(games_df_eval.result1, games_df_eval.elo_prob1, n_bins=BINS)
print(math.sqrt(((prob_true - prob_pred_elo)**2).mean()))
print(prob_true)

plt.plot(range(len(prob_true)), prob_true, color="black")
plt.plot(range(len(prob_pred_elo)), prob_pred_elo, color="red")
plt.plot(range(len(prob_pred_my)), prob_pred_my, color="blue")
plt.show()
"""

games_df = list(robjects.r("forecast")(pd.DataFrame(games_test), *best_params, final_elos))[0]

games = rpy2py(games_df).to_dict("records")
# quit()

# Evaluate our forecasts against Elo
Util.evaluate_forecasts(games)
