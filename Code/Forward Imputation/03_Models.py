# Libraries ----
import pandas as pd
import numpy as np
from scipy.stats import uniform, randint
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
from sklearn.metrics import mean_squared_error, r2_score

# Data ----
train_set = pd.read_csv("./Data/Forward/train.csv")
test_set  = pd.read_csv("./Data/Forward/test.csv")
emovi     = pd.read_csv("./Data/Forward/emovi_replica.csv")

# Train-Validation Split ----
train_y = train_set[["y"]]
train_X = train_set.iloc[:, 7:-1]

test_y = test_set[["y"]]
test_X = test_set.iloc[:, 7:-1]

emovi_y = emovi[["y"]]
emovi_X = emovi.iloc[:, 8:-3]

X_train, X_vali, y_train, y_vali = train_test_split(train_X, train_y, train_size=0.2, random_state=1)

# OLS ----
from sklearn.linear_model import LinearRegression

params = {}

OLS = LinearRegression()

OLS = GridSearchCV(OLS, 
                   param_grid=params,
                   scoring='r2',
                   cv=5,
                   verbose=1,
                   return_train_score=True)

OLS = OLS.fit(X_train, y_train)

print(f"Best params: {OLS.best_params_}")
print(f"R2: {(OLS.best_score_)}")

OlS = OLS.best_estimator_

y_OLS = OLS.predict(X_vali)
R2_OLS = r2_score(y_vali, y_OLS)
print(f"R2 Validation Set: {R2_OLS}")

y_OLS = OLS.predict(train_X)
pd.DataFrame(y_OLS, train_set['folio']).to_csv("./Data/Forward/OLS_train.csv")

y_OLS = OLS.predict(test_X)
pd.DataFrame(y_OLS, test_set['folio']).to_csv("./Data/Forward/OLS_test.csv")

y_OLS = OLS.predict(emovi_X)
pd.DataFrame(y_OLS).to_csv("./Data/Forward/OLS_emovi.csv")