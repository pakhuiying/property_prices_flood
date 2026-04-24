import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import cm

from sklearn import datasets
from sklearn.ensemble import HistGradientBoostingClassifier
from sklearn.gaussian_process import GaussianProcessClassifier
from sklearn.gaussian_process.kernels import RBF
from sklearn.inspection import DecisionBoundaryDisplay
from sklearn.kernel_approximation import Nystroem
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import matthews_corrcoef, accuracy_score, log_loss, roc_auc_score
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import (
    KBinsDiscretizer,
    PolynomialFeatures,
    SplineTransformer,
)
from sklearn.inspection import permutation_importance

def train_classifier_models(X_train, X_test, y_train, y_test):
    # probabilistic classifiers
    classifiers = {
        "Logistic regression\n(C=0.01)": LogisticRegression(C=0.1),
        "Logistic regression\n(C=1)": LogisticRegression(C=100),
        # "Gaussian Process": GaussianProcessClassifier(kernel=1.0 * RBF([1.0, 1.0])),
        "Logistic regression\n(RBF features)": make_pipeline(
            Nystroem(kernel="rbf", gamma=5e-1, n_components=50, random_state=1),
            LogisticRegression(C=10),
        ),
        "Gradient Boosting": HistGradientBoostingClassifier(),
        "Logistic regression\n(binned features)": make_pipeline(
            KBinsDiscretizer(n_bins=2),#, quantile_method="averaged_inverted_cdf"),
            PolynomialFeatures(interaction_only=True),
            LogisticRegression(C=10),
        ),
        "Logistic regression\n(spline features)": make_pipeline(
            SplineTransformer(n_knots=5),
            PolynomialFeatures(interaction_only=True),
            LogisticRegression(C=10),
        ),
    }

    n_classifiers = len(classifiers)
    scatter_kwargs = {
        "s": 25,
        "marker": "o",
        "linewidths": 0.8,
        "edgecolor": "k",
        "alpha": 0.7,
    }
    y_unique = np.unique(y_train)

    evaluation_results = []
    levels = 100
    for classifier_idx, (name, classifier) in enumerate(classifiers.items()):
        y_pred = classifier.fit(X_train, y_train).predict(X_test)
        y_pred_proba = classifier.predict_proba(X_test)
        try:
            coef = classifier.steps[-1][-1].coef_
        except:
            try:
                coef = classifier.coef_
            except:
                try:
                    coef = classifier.feature_importances_
                except:
                    coef = np.full((X_train.shape[1],1),np.nan)
        accuracy_test = accuracy_score(y_test, y_pred)
        roc_auc_test = roc_auc_score(y_test, y_pred_proba[:,1])
        log_loss_test = log_loss(y_test, y_pred_proba)
        results = {
                "name": name.replace("\n", " "),
                "accuracy": accuracy_test,
                "roc_auc": roc_auc_test,
                "log_loss": log_loss_test,
                # "coef": coef.shape
            }
        
        evaluation_results.append(results)

    return pd.DataFrame(evaluation_results)

def log_regression(X,y):
    clf = LogisticRegression(random_state=0, C=10, max_iter=1000).fit(X, y)
    y_hat = clf.predict(X)
    # clf.predict_proba(X[:2, :])
    # clf.score(X, y) # compute accuracy score
    # compute MCC
    mcc = matthews_corrcoef(y, y_hat)
    print(f"MCC score: {mcc:.4f}")
    prob_estimates = clf.predict_proba(X) # probability esimates
    coef = clf.coef_#densify() # convert coefficient matrix 
    return prob_estimates, coef

def plot_permutation_importance(clf, X, y, ax, scoring='roc_auc',random_state=42):
    """ 
    Args:
        scoring (str or callable): metric to evaluate the performance of model. 
        See https://scikit-learn.org/stable/modules/model_evaluation.html#scoring-string-names for more options
    """
    result = permutation_importance(clf, X, y, n_repeats=10, random_state=random_state, n_jobs=2,
                                    scoring=scoring)
    perm_sorted_idx = result.importances_mean.argsort()

    ax.boxplot(result.importances[perm_sorted_idx].T, vert=False, tick_labels=X.columns[perm_sorted_idx])
    ax.axvline(x=0, color="k", linestyle="--")
    return X.columns[perm_sorted_idx]