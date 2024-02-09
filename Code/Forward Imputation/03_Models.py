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
emovi     = pd.read_csv("./Data/Forward/emovi.csv")


# Train-Validation Split ----
train_y = train_set[["y"]]
train_X = train_set.iloc[:, 7:-1]

test_y = test_set[["y"]]
test_X = test_set.iloc[:, 7:-1]

emovi_X = emovi.iloc[:, 6:-1]
emovi_X.fillna(0, inplace=True)

names = train_X.columns.tolist()
test_X.columns = names
emovi_X.columns = names

X_train, X_vali, y_train, y_vali = train_test_split(train_X, train_y, train_size=0.8, random_state=1)

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

# XGBoost ----
from xgboost import XGBRegressor

params = {
  "n_estimators": randint(10, 1000),
  "max_depth": randint(1, 100),
  "eta" : np.linspace(0.1, 1, 100),
  "subsample": uniform(0.6, 0.4),
  "colsample_bytree": uniform(0.8, 0.2),
}

XGB = XGBRegressor()

XGB = RandomizedSearchCV(XGB,
                         param_distributions = params,
                         scoring = 'r2',
                         cv = 5, 
                         verbose = 1,
                         random_state = 123,
                         n_iter = 500)
                           
XGB = XGB.fit(X_train, y_train)

print(f"Best params: {XGB.best_params_}")
print(f"R2: {(XGB.best_score_)}")

XGB = XGB.best_estimator_

y_XGB = XGB.predict(X_vali)
R2_XGB = r2_score(y_vali, y_XGB)
print(f"R2 Validation Set: {R2_XGB}")

y_XGB = XGB.predict(train_X)
pd.DataFrame(y_XGB, train_set['folio']).to_csv("./Data/Forward/XGB_train.csv")

y_XGB = XGB.predict(test_X)
pd.DataFrame(y_XGB, test_set['folio']).to_csv("./Data/Forward/XGB_test.csv")

y_XGB = XGB.predict(emovi_X)
pd.DataFrame(y_XGB).to_csv("./Data/Forward/XGB_emovi.csv")

# KNN ----
from sklearn.neighbors import KNeighborsRegressor

params = {
    'n_neighbors': [1, 5, 10, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100],
    'weights': ["uniform", "distance"],
    'metric': ["manhattan"]
}

KNN = KNeighborsRegressor()
KNN = RandomizedSearchCV(KNN,
                         param_distributions = params,
                         scoring = 'r2',
                         cv = 5, 
                         verbose = 1,
                         random_state = 123,
                         n_iter = 500)
                           
KNN = KNN.fit(X_train, y_train)

print(f"Best params: {KNN.best_params_}")
print(f"R2: {(KNN.best_score_)}")

KNN = KNN.best_estimator_

y_KNN = KNN.predict(X_vali)
R2_KNN = r2_score(y_vali, y_KNN)
print(f"R2 Validation Set: {R2_KNN}")

y_KNN = KNN.predict(train_X)
pd.DataFrame(y_KNN, train_set['folio']).to_csv("./Data/Forward/KNN_train.csv")

y_KNN = KNN.predict(test_X)
pd.DataFrame(y_KNN, test_set['folio']).to_csv("./Data/Forward/KNN_test.csv")

y_KNN = KNN.predict(emovi_X)
pd.DataFrame(y_KNN).to_csv("./Data/Forward/KNN_emovi.csv")