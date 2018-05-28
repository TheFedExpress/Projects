# -*- coding: utf-8 -*-
"""
Created on Wed May  2 16:03:45 2018

@author: Peter_goodridge
"""

def normalize(df, col):
    df[col] = df[col]/df[col].values[0]
    return df

def append_currency(df, currency):
    from get_currency_info import get_history
    df[currency] = get_history(currency, 2000)['close']
    return df

def make_portfolio(currencies):
    from get_currency_info import get_history
    import pandas as pd
    df = pd.DataFrame(get_history(currencies[0], 2000)['close'])
    df.rename(columns = {'close': currencies[0]}, inplace = True)
    for item in currencies[1:]:
        append_currency(df, item)
    df = df.loc['01/01/2016':]
    
    for item in [currencies]:
        df = normalize(df, item)
    return df

def sum_column(df, currencies, weights):
    sum = 0
    for i in range(len(currencies)):
        sum += df[currencies[i]]*weights[i]
    return sum

def apply_weights(weights, df, currencies):
    import numpy as np    
        
    df['sum'] = sum_column(df, currencies, weights)
    df['returns'] = np.log(df['sum']) - np.log(df['sum'].shift(1))
    df = df.dropna()
    return df.returns.values.sum()*-1


def check_sum(weights):
    tot = 0
    for i in range(len(weights)):
        tot += weights[i]
    return tot - 1


def find_portfolio(currencies):
    from scipy.optimize import minimize
    import numpy as np
    
    
    elements = len(currencies)
    start = 1/elements
    
    b1 = (0.0, 1.0)
    bounds = [b1 for i in range(elements)]
    
    constraints = {'type': 'eq', 'fun': check_sum}
    df = make_portfolio(currencies)
    
    optimal = minimize(apply_weights, np.full(elements, start), args = (df, currencies), 
                       constraints = constraints, method = 'SLSQP', bounds = bounds)
    return optimal['x']


#easy to convert to html table for display
def prepare_opt(currencies):
    import pandas as pd
    
    weights = find_portfolio(currencies)
    
    input_dict = {'Currency': currencies, 'Weight': weights}
    
    df = pd.DataFrame.from_dict(input_dict)
    df['Weight'] = (df['Weight']*100).map('{:.1f}%'.format)
    
    return df