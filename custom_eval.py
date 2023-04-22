import rpy2.robjects as robjects
import pandas as pd
from rpy2.robjects import pandas2ri
import sys

sys.path.append("./nfl-elo-game")

from util import *
from forecast import *

# rpy2 stuff
pandas2ri.activate()
r_source = robjects.r["source"]
forecast = r_source("./forecast.R")

# Read historical games from CSV
games = Util.read_games("nfl-elo-game/data/nfl_games.csv")

# print(games)
# quit()

# Forecast every game using the R code
games_df = robjects.r("forecast")(pd.DataFrame(games))
print(games_df)
games = games_df.to_dict("records")
# quit()

# Evaluate our forecasts against Elo
Util.evaluate_forecasts(games)
