# -*- coding: utf-8 -*-
"""
Created on Sun Apr 22 17:57:00 2018
@author: pgood
"""
import time

def prepare_returns(ticker):
    import numpy as np
    from get_currency_info import get_history
    
    df = get_history(ticker, 2000, 'Kraken')
    df.loc[:, 'log_ret'] = np.log(df.close) - np.log(df.close.shift(1))
    df = df.loc['01-01-2016':]
    df.dropna(inplace = True)
    #returns['log_ret'] = returns['log_ret'].diff()

    return df

def garch_predict(ticker, user):

    from arch import arch_model
    from get_currency_info import get_current
    
    returns = prepare_returns(ticker)['log_ret']*10 #to prevent underflow during fit
        
    am = arch_model(returns, p=1, o=0, q=2, dist='StudentsT')
    res = am.fit(update_freq=5, disp='off')

    change = res.forecast(horizon=1).mean.iloc[-1,0]/10
    
    forecast = get_current(ticker, 'check')*(1 + change)*user.get_mult()
    return '${:,.2f}'.format(forecast)

def forest_prep(prim_ticker):
    import numpy as np
    
    
    if prim_ticker == 'BTC':
        sec_ticker = 'ETH'
    else:
        sec_ticker = 'BTC'
    prim = prepare_returns(prim_ticker)[['close']]
    sec = prepare_returns(sec_ticker)[['close']]
    
    
    df = prepare_returns(prim_ticker)
    
    df.loc[:, 'diff'] = prim - sec
    
    for i in range(1,6):
        df.loc[:, 'log_ret{}'.format(i)] = np.log(df.close) - np.log(df.close.shift(i))
    
    df[['prev{}'.format(i) for i in range(1,6)]] = df.shift(1).loc[:, ['log_ret{}'.format(i) for i in range(1,6)]]
    
    df.dropna(inplace = True)
    
    for i in range(1,5):#for the streak
        df[['prev_ret{}'.format(i)]] = df.shift(i).loc[:,['log_ret1']]
    
    df = df.dropna()
    
    #keeping function definintion here because it's not a "real" funciton
    #its only use is in the apply method
    def calc_streak(df):
        streak = 1
        cols = [df.prev_ret1, df.prev_ret2, df.prev_ret3, df.prev_ret4]
        for i in range(1,4):
            test = cols[i-1] * cols[i] > 0
            if test == False:
                break
            else:
                streak += test
        return streak
    
    df['streak'] = df.apply(calc_streak, axis = 1)        
    X = df[['prev1', 'prev2', 'prev3', 'prev4', 'prev5', 'diff', 'streak']]
    
    y = df['log_ret']
    
    return X, y
    
def forest_train(ticker):
    from sklearn.ensemble import RandomForestRegressor
    
    X,y = forest_prep(ticker)
    rf = RandomForestRegressor(min_samples_leaf = 5, min_samples_split = 10, n_estimators = 100)
    rf.fit(X, y.values.ravel())
    return rf

def forest_predict(model, ticker, user):
    from get_currency_info import get_current
    
    X,y = forest_prep(ticker)
    
    pred = model.predict(X.iloc[-1:, :].values.reshape(1,-1))
    forecast = get_current(ticker, 'check')*(1 + pred[0])*user.get_mult()
    return '${:,.2f}'.format(forecast)
