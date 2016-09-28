
## ============================================================= ##
##  Code to create plot showing iterations of gradient boosting  ##
## ============================================================= ##

# from sklearn import data
# from sklearn import ensemble
# from sklearn.ensemble import GradientBoostingClassifier

import matplotlib.pyplot as plt
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.datasets import make_hastie_10_2

# # Create some smaple data
# X,y = make_hastie_10_2(n_samples=10)

# # Fit some regressors over the data
# my_est = GradientBoostingRegressor(n_estimators=2000, max_depth=1).fit(X, y)


# for pred in my_est.staged_predict(X):
#     plt.plot(X[:, 0], pred, color= 'r', alpha=0.1)

# plt.show()


## =================================== ##
##  From Peter Prettenhofer PyCon2014  ##
## =================================== ##

# pylab inline
import numpy as np
from sklearn.cross_validation import train_test_split

FIGSIZE = (11, 7)

def ground_truth(x):
    """Ground truth -- function to approximate"""
    return x * np.sin(x) + np.sin(2 * x)

def gen_data(n_samples=200):
    """generate training and testing data"""
    np.random.seed(15)
    X = np.random.uniform(0, 10, size=n_samples)[:, np.newaxis]
    y = ground_truth(X.ravel()) + np.random.normal(scale=2, size=n_samples)
    train_mask = np.random.randint(0, 2, size=n_samples).astype(np.bool)
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=3)
    return X_train, X_test, y_train, y_test

X_train, X_test, y_train, y_test = gen_data(100)

# plot ground truth
x_plot = np.linspace(0, 10, 500)

def plot_data(alpha=0.4, s=20):
    plt.figure(figsize=FIGSIZE)
    plt.plot(x_plot, ground_truth(x_plot), alpha=alpha, label='ground truth')
    

    # plot training and testing data
    plt.scatter(X_train, y_train, s=s, alpha=alpha)
    plt.scatter(X_test, y_test, s=s, alpha=alpha, color='red')
    plt.xlim((0, 10))
    plt.ylabel('y')
    plt.xlabel('x')
    
annotation_kw = {'xycoords': 'data', 'textcoords': 'data',
                 'arrowprops': {'arrowstyle': '->', 'connectionstyle': 'arc'}}

if __name__== "__main__" :

    plot_data()

