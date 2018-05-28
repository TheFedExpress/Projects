# -*- coding: utf-8 -*-
"""
Created on Sat Feb 17 16:53:12 2018
@author: pgood
"""

class User:
    
    def __init__(self, db, starting = 100000000.0):
        if db.new_account == 0:
            self.currency = db.db.cur.find_one({'currency' : {'$exists' : True}})['currency']
        else:
            self.currency = 'USDT'
        self.starting_cash = starting
        
    
    #For fungible shares, it doesn't make sense to allow for simultaneous short and long positions
    #This would also require two lines on the P/L, causing confusion
    def evalTransaction(self, tran_type, shares, price, ticker, db, pl, blotter):
       from datetime import datetime
       
       cash = pl.pl.loc['cash', 'market']
       total = shares * price
       
       try:
            prev_shares = pl.pl.loc[ticker, "position"]
            new = 0
       except KeyError:
            new = 1
            prev_shares = 0

       if tran_type in ["buy", "cover"]:
            #User abuse/misuse prevention
            margin = pl.check_margin()

            if tran_type == "cover" and prev_shares >= 0:
                self.message = "You do not have any short sales with this security"
            elif tran_type == "cover" and shares + prev_shares > 0:
                self.message = "You have attempted to cover {} shares.  Please cover {} shares or less"
                self.message = self.message.format(shares, -prev_shares)
                
            elif tran_type == "buy" and cash <= margin * 2 + shares * price:
                self.message = "This transaction requires ${:,.2f} in your account.  You have ${:,.2f}."
                self.message = self.message.format(margin * 2 + shares * price, cash)
                
            elif tran_type == "buy" and prev_shares < 0:
                self.message = "You currently have {} shares of {} shorted.  Please cover those before buying."
                self.message = self.message.format(shares, ticker)
            else:#Process transaction
                date = datetime.now ()
                trans = (cash - total,  ticker, -total, price, shares, tran_type)
                pl.eval_pl(tran_type, shares, price, ticker, total, db, new, prev_shares, 
                           date.strftime('%Y%m%d%H%M%S%f'))
                blotter.eval_blotter(db, date, trans)
                self.message = "Success"
       else:
            if tran_type == "short":
                margin = pl.check_margin()
                                 
            if tran_type == "sell" and prev_shares < 0:
                self.message = "You have a short position in {0} of {1} shares".format(ticker, prev_shares*-1)
                
            elif tran_type == "sell" and prev_shares < shares:
                self.message= "You have {0} shares of {1} in you account.  Please choose a different quantity to sell"
                self.message = self.message.format(prev_shares, ticker)
                
            elif tran_type == "short" and prev_shares > 0:
                self.message = "You have a long position in {} of {} shares.  Please sell before shorting"
                self.message = self.message.format(ticker, prev_shares)
            elif tran_type == "short" and cash <= (margin * 2 + shares * price):
                self.message = "This transaction requires ${:,.2f} in your account.  You have ${:,.2f}."
                self.message = self.message.format(margin * 2 + shares * price, cash)
                
            else:#Process transaction
               date = datetime.now ()
               trans = (cash + total,  ticker, total, price, shares, tran_type)
               pl.eval_pl(tran_type, shares, price, ticker, total, db, new, prev_shares, 
                          date.strftime('%Y%m%d%H%M%S%f'))
               blotter.eval_blotter(db, date, trans)
               self.message = "Success"
    
    def wipe_account(self, db, blotter, pl):
        import pandas as pd
        #reset db
        db.db.pl.delete_many({})
        db.db.pl_hist.delete_many({})
        db.db.blotter.delete_many({})
        db.db.cur.delete_many({})
        
        #reset blotter
        blotter.blotter_rows = 0
        blotter.blotter = pd.DataFrame(columns = [ "cash_balance", "ticker", "net", "price",
         "shares", "tran_type"])
    
        #reset pl
        start = {"cash": {"position": 0, "market": self.starting_cash, "wap" : 0.0, "rpl": 0.0, "upl": 0.0, "tpl": 0.0,
        "allocation_by_shares": 0.0, "allocation_by_dollars": 100.0}}
        pl.pl = pd.DataFrame.from_dict(start, orient = 'index')
        db.pl_insert(pl.pl, 'cash')
        pl.pl_hist = pd.DataFrame(columns = ['ticker', 'wap', 'rpl', 'position', 'upl', 'tpl'])  
        
        
        #reset user
        self.currency = 'USDT'
        db.db.cur.insert_one({'currency' : 'USDT'})
        
    def change_currency(self, ticker, db):
        self.currency = ticker
        db.currency_update(ticker)
        
    def get_mult(self):
        from get_currency_info import get_current
        
        if self.currency != 'USDT':
            cur = get_current(self.currency, 'check')
        else:
            cur = 1
        return 1/cur

class Blotter:
    
    def __init__(self, user, db):
        import pandas as pd
        from datetime import datetime
        
        if db.new_account == 1:
            self.blotter_rows = 0
            self.blotter = pd.DataFrame(columns = [ "cash_balance", "ticker", "net", "price",
                                 "shares", "tran_type"])
    
        else:
            self.blotter_rows = db.db.blotter.count()
            if self.blotter_rows > 0:
                #recreate blotter from last session
                blotter_list = []
                blotter_recs = db.db.blotter.find({})
                for rec in blotter_recs:
                    record_dict = {}
                    for key in rec.keys():
                        if key != '_id':
                            if key == 'date':
                                record_dict[key] = datetime.fromtimestamp(rec[key])
                            else:
                                record_dict[key] = rec[key]
                    blotter_list.append(record_dict)
                self.blotter = pd.DataFrame(blotter_list)
                self.blotter.set_index('date', inplace=True)
                self.blotter = self.blotter[[ "cash_balance", "ticker", "net", "price",
                                 "shares", "tran_type"]]

            else:#new blotter with no rows
                self.blotter = pd.DataFrame(columns = [ "cash_balance", "ticker", "net", "price",
                                 "shares", "tran_type"])
                
    def showBlotter(self, user):
        from get_currency_info import get_current
             
        if self.blotter_rows > 0:

            mult = user.get_mult()
            #prepare for display
            df = self.blotter.copy()
            df.loc[:, ['price', 'net', 'cash_balance']] *= mult
            df["price"] = df["price"].map('${:,.2f}'.format)
            df["net"] = df["net"].map('${:,.2f}'.format)
            df["cash_balance"] = df["cash_balance"].map('${:,.2f}'.format)
            dates = df.index
            df['Transaction Date'] = dates
            
            df = df[['Transaction Date', "ticker", "price", "shares", "tran_type", "net", "cash_balance"]]
            to_string = lambda x: '{:,.8f}'.format(x).rstrip('0').rstrip('.')
            df['shares'] = df['shares'].map(to_string)
            labels = ["Transaction Date", "Currency", "Price", "Shares Traded", "Transaction Type",
                      "Net Cash Flow", "Cash Balance"]
            df.columns = labels
            self.blotter_view = df
            
    def eval_blotter(self, db, date, trans):
           self.blotter.loc[date] = trans
           db.blotter_insert(date, trans)
           self.blotter_rows += 1

            
class PL:
    
    def __init__(self, user, db):
        import pandas as pd
        from price_predictions import forest_train
        
        self.btc_forest = forest_train('btc')
        self.eth_forest = forest_train('eth')
        
        if db.new_account == 1:
            start = {"cash": {"position": 0.0, "market": user.starting_cash, "wap" : 0.0, "rpl": 0.0, "upl": 0.0, 
                              "tpl": 0.0, "allocation_by_shares": 0.0, "allocation_by_dollars": 0.0}}
            self.pl = pd.DataFrame.from_dict(start, orient = 'index')
            db.pl_insert(self.pl, 'cash')
            
            self.pl_hist = pd.DataFrame(columns = ['ticker', 'wap', 'rpl', 'position', 'upl', 'tpl'])           
            
        else: #recreate pl from last session
            pl_recs = db.db.pl.find({})
            pl_dict = {}
            for rec in pl_recs:
                for key in rec.keys():
                    if key != '_id':
                        pl_dict[key] = rec[key]
            self.pl = pd.DataFrame.from_dict(pl_dict, orient = 'index')        
            if db.db.blotter.count() > 0:
                pl_hist = {}
                hist_recs = db.db.pl_hist.find({})
                for rec in hist_recs:
                    for key in rec.keys():
                        if key != '_id':
                            pl_hist[key] = rec[key]

                self.pl_hist = pd.DataFrame.from_dict(pl_hist, orient = 'index')

            else:
                self.pl_hist = pd.DataFrame(columns = ['ticker', 'wap', 'rpl', 'position', 'upl', 'tpl']) 
                
    def check_margin(self):
        from get_currency_info import get_current
        
        margin = 0
        for currency in self.pl.index:
            if self.pl.loc[currency, "position"] < 0:
                margin += self.pl.loc[currency, "position"] * get_current(currency, "buy") * -1
        return margin
    
    def calc_wap(self, prev_shares, shares, new_shares, price, sign, ticker):
        prev_value = prev_shares * self.pl.loc[ticker, "wap"] * sign
        wap = (prev_value + shares * price)/new_shares * sign

        return wap
        
    
    def calc_tpl(self, ticker, date, shares, wap, market, gain):
        import numpy as np
        from datetime import timedelta, datetime
        
        #for an active trader calculating on trades should be best
        #Below would be another option
        #date = datetime.now().strftime('%Y%m%d%H%M%S%f')
        #self.pl_hist.loc[date, :] = ('Portfolio', 0, 0, 0, 0, np.sum(final_df["tpl"]/mult))
        #db.pl_hist_insert(self.pl_hist, date)   
        
        num_date = datetime.strptime(date, '%Y%m%d%H%M%S%f')
        up_to = datetime.strftime(num_date - timedelta(seconds = 1), '%Y%m%d%H%M%S%f')
        market_now = shares * (market - wap)
        
        try:
            last_same = self.pl_hist.loc[self.pl_hist['ticker'] == ticker].loc[:up_to].index.max()
            last_market = self.pl_hist.loc[last_same, 'upl']
            if np.isnan(last_market):
                last_market = 0
        except TypeError:
            last_market = 0
            
        self.pl_hist.loc[date, 'upl'] = market_now
        tpl_delta = market_now - last_market + gain
        try:
            last_all = self.pl_hist.loc[:up_to].index.max()
            last_tpl = self.pl_hist.loc[last_all, 'tpl']
            if np.isnan(last_tpl):
                last_tpl = 0
            self.pl_hist.loc[date, 'tpl'] = last_tpl + tpl_delta
        except TypeError:
            self.pl_hist.loc[date, 'tpl'] = tpl_delta
            
            
    
    def eval_pl(self, tran_type, shares, price, ticker, total, db, new, prev_shares, date):
        #only certain columns that form the "base" data are calculated here.  
        #Columns like upl are only calculated when the user views the pl because 
        #those rely on market value.  This approach should be more efficient
        #in terms of performance and coding
        from get_currency_info import get_current
        
        market = get_current(ticker, 'check')
        if tran_type in ('buy', 'cover'):
            self.pl.loc['cash', 'market'] -= total
            
            if new == 0:
                new_shares = prev_shares + shares
                
                if tran_type == "buy":
                    
                    if self.pl.loc[ticker, "position"] == 0:
                        self.pl.loc[ticker, 'wap'] = price
                        self.pl_hist.loc[date, ['ticker', 'wap', 'position', 'rpl']] = (
                                ticker, price, new_shares, 0)
                        self.calc_tpl(ticker, date, new_shares, price, market, 0)
            
                    else:
                        wap = self.calc_wap(prev_shares, shares, new_shares, price, 1, ticker)
                        self.pl.loc[ticker, 'wap'] = wap

                        self.pl_hist.loc[date, ['ticker', 'wap', 'position', 'rpl']] = (
                                ticker, wap, new_shares, 0)
                        self.calc_tpl(ticker, date, new_shares, wap, market, 0)
                        
                else:#Cover
                    wap = self.pl.loc[ticker, "wap"]
                    gain = wap * shares - total
                    rpl = self.pl.loc[ticker, "rpl"] + gain
                    self.pl.loc[ticker, "rpl"] = rpl
                    self.pl_hist.loc[date, ['ticker', 'rpl', 'position', 'wap']]  = (
                        ticker, rpl, new_shares, wap)
                    self.calc_tpl(ticker, date, new_shares, wap, market, gain)
                    
                    
                #both    
                self.pl.loc[ticker, "position"] = new_shares
                if new_shares == 0:#for aesthetics in the P&L
                    self.pl.loc[ticker, "wap"] = 0
                db.pl_update(self.pl, ticker)
                db.pl_update(self.pl, 'cash')
                db.pl_hist_insert(self.pl_hist, date)
                
            else:#new currencies
                self.pl.loc[ticker, ['position', 'market', 'wap', 'rpl', 'upl', 'tpl', 
                     'allocation_by_dollars', 'allocation_by_shares']] = (shares, 0, price, 0, 0, 0, 0, 0)
                self.pl_hist.loc[date, ['ticker', 'wap', 'rpl', 'position']] = (
                    ticker, price, 0, shares)
                self.calc_tpl(ticker, date, shares, price, market, 0)
                db.pl_insert(self.pl, ticker)
                db.pl_hist_insert(self.pl_hist, date)
                db.pl_update(self.pl, 'cash')
                
        else:#sell/short
           self.pl.loc['cash', 'market'] += total
           
           if new == 0:
               new_shares = self.pl.loc[ticker,'position'] - shares
               
               if tran_type == "sell":
                   wap = self.pl.loc[ticker, 'wap']
                   gain = total -  wap * shares
                   self.pl.loc[ticker, 'rpl']  = self.pl.loc[ticker, "rpl"] + gain
                   self.pl_hist.loc[date, ['rpl', 'ticker', 'position', 'wap']]  = (
                           gain, ticker, new_shares, wap)
                   self.calc_tpl(ticker, date, new_shares, wap, market, gain)
                   
               else:#short

                   if self.pl.loc[ticker, 'position'] == 0:
                       self.pl.loc[ticker, "wap"] = price
                       self.pl_hist.loc[date, ['ticker', 'wap', 'position', 'rpl']] = (
                               ticker, price, new_shares, 0)
                       self.calc_tpl(ticker, date, new_shares, price, market, 0)
                       
                   else:
                       wap = self.calc_wap(prev_shares, shares, new_shares, price, -1, ticker)
                       self.pl.loc[ticker, "wap"] = wap
                       self.pl_hist.loc[date, ['ticker', 'wap', 'position', 'rpl']] = ( 
                           ticker, wap, new_shares, 0)
                       self.calc_tpl(ticker, date, new_shares, wap, market, 0)
               #both
               self.pl.loc[ticker, "position"] = new_shares
               if new_shares == 0:
                   self.pl.loc[ticker, "wap"] = 0   
                   
               db.pl_update(self.pl, ticker)
               db.pl_update(self.pl, 'cash')
               db.pl_hist_insert(self.pl_hist, date)
               
           else:#new currencies
               self.pl.loc[ticker, ['position', 'market', 'wap', 'rpl', 'upl', 'tpl', 
                 'allocation_by_dollars', 'allocation_by_shares']] = (-shares, 0, price, 0, 0, -shares*price, 0, 0)
               self.pl_hist.loc[date, ['ticker', 'wap', 'rpl', 'position']] = (
                       ticker, price, 0, -shares)
               self.calc_tpl(ticker, date, -shares, price, market, 0)
               db.pl_insert(self.pl, ticker)
               db.pl_hist_insert(self.pl_hist, date)
               db.pl_update(self.pl, 'cash')

           
    def showPL(self, user, db):
        import numpy as np
        from get_currency_info import get_current
        from price_predictions import garch_predict, forest_predict
        from datetime import datetime

        #for showing PL in different currencies
        mult = user.get_mult()
            
        final_df = self.pl.copy()[self.pl.index != 'cash']
        #isolate the actual currencies from cash
        markets = []
        garch_predictions = []
        forest_predictions = []
        for position in final_df.index.values:
            market = get_current(position, 'check')
            markets.append(market)
            if position in ('BTC', 'ETH'):
                garch_predictions.append(garch_predict(position, user))
                if position == 'ETH':
                    forest_predictions.append(forest_predict(self.eth_forest, position, user))
                else:
                    forest_predictions.append(forest_predict(self.btc_forest, position, user))
            else:
                garch_predictions.append(0)
                forest_predictions.append(0)
                
        final_df["market"] = markets
        final_df.loc[:, 'price_pred_garch'] = garch_predictions
        final_df.loc[:, 'price_pred_forest'] = forest_predictions
        
        cash = self.pl.loc['cash', 'market'] * mult
        
        #additional columns calculated from "base" data
        final_df['total_value'] = final_df.position * final_df.market
        final_df.loc[:, 'share_weight'] =  abs(final_df.position)/np.sum(abs(final_df.position))
        final_df.loc[:, 'value_weight'] = abs(final_df['total_value'])/np.sum(abs(final_df['total_value']))
        final_df['upl'] = final_df.position * final_df.market - final_df.position * final_df.wap
        final_df['tpl'] = final_df.upl + final_df.rpl     
        
        
        final_df.fillna(0)
        final_df.replace(np.nan, 0, inplace=True)
                        
        currencies = final_df.index
        final_df['currency'] = currencies
        final_df = final_df[['currency', "position", "market", "total_value", 'value_weight', 'share_weight', 
                             "wap", "upl", 'rpl',  "tpl", 'price_pred_garch', 'price_pred_forest']]
        
        final_df.loc[:, ['tpl', 'market', 'rpl', 'total_value', 'wap', 'upl']] *= mult
        
        #total row
        val = cash + np.sum(final_df["total_value"])
        self.tpl =  np.sum(final_df["tpl"])
        totals = ['Total', '', '',  val,'', '', '', np.sum(final_df["upl"]), np.sum(final_df["rpl"]),
                 self.tpl,'','']
             
        
        #format total row
        for item in range(len(totals)):
            if type(totals[item]) is not str:
                totals[item] = "${:,.2f}".format(totals[item]) 
                
        totals = tuple(totals)
        cash_row = ("Cash", '', '', "${:,.2f}".format(cash),'', '',  '', '','', '','','')
        space_row = tuple(['' for i in range(len(final_df.columns))])
        
        #format currency rows
        for item in ["market", "total_value", "wap", "upl", "rpl", 'tpl']:
            final_df[item] = final_df[item].map('${:,.2f}'.format)
        for item in ['value_weight', 'share_weight']:
            final_df[item] = (final_df[item]*100).map('{:.1f}%'.format)
        to_string = lambda x: '{:,.8f}'.format(x).rstrip('0').rstrip('.')
        final_df['position'] = final_df['position'].map(to_string)
            
        rows = len(final_df)
        
        final_df.loc[rows] = cash_row
        final_df.loc[rows + 1] = space_row
        final_df.loc[rows + 2] = totals
            
        cols = ['Currency', "Position", "Market Price", "Total Value", 'Value Weight', 'Share Weight',
                "WAP", "UPL", "RPL", 'Total PL','Price Prediction (GARCH)', 'Price Prediction (Random Forest)']
        
        
        final_df.columns = cols
        self.pl_view = final_df
        
    def pl_tickers(self):
        tickers = self.pl.index.values
        tickers = tickers[tickers != 'cash']
        return tickers
            

class UserDB:
    def __init__(self):
        from pymongo import MongoClient
        
        client = MongoClient('mongodb://cuny:data@ds157639.mlab.com:57639/currency_v2', connectTimeoutMS = 50000)
        self.db = client.get_database('currency_v2')
        
        if self.db.pl.count() > 0:
            self.new_account = 0
        else:#
            self.new_account = 1
            #Count should always be 0, but the app is getting more complex and it's better
            #to be paranoid than sorry
            if self.db.cur.count() == 0:
                self.db.cur.insert_one({'currency' : 'USDT'})
            else:
                self.db.cur.delete_many({})
                self.db.cur.insert_one({'currency' : 'USDT'})
            
        
        
    def pl_insert(self, df, ticker):
        item = df.loc[ticker].to_dict()
        self.db.pl.insert_one({ticker: item})
        
    def pl_hist_insert(self, df, date):
        item = df.loc[date].to_dict()
        self.db.pl_hist.insert_one({date: item})
        
    def pl_update(self, df, ticker):
        item = df.loc[ticker].to_dict()
        self.db.pl.update_one({ticker : {'$exists' : True}}, {'$set': {ticker : item}})
    
    def currency_update(self, currency):
        self.db.cur.update_one({'currency' : {'$exists' : True}}, {'$set': {'currency' : currency}})
        
    def blotter_insert(self, date, row):
         from datetime import datetime
         
         keys = [ "cash_balance", "ticker", "net", "price",
                                 "shares", "tran_type", 'date']
         vals = list(row)
         since_date = datetime(1970, 1, 1, 0, 0, 0)
         seconds = (date - since_date).total_seconds()
         vals.append(seconds)
         doc = dict(zip(keys, vals))
         self.db.blotter.insert_one(doc)