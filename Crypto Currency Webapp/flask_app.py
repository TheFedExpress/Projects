# -*- coding: utf-8 -*-
"""
Created on Sat Mar 17 13:21:03 2018

@author: pgood
"""

from flask import Flask, render_template, request, jsonify, make_response, send_file, Markup
from user_accountv2 import User, Blotter, PL, UserDB

my_db = UserDB()
account = User(my_db, starting = 100000000)
my_pl = PL(account, my_db)
my_blotter = Blotter(account, my_db)

app = Flask(__name__)

#App set up as a landing page with the links to the three functional pages
#Graph page is meant to only be navigated to as a popup

def test_inputs(price, shares):
    
    if type(price) in (int, float):
        price_check = True
    try:
        if round(float(shares), 8) == float(shares):#Was not able to locate a list of crytpo denominations
                                                    #Assume they are all in 100 millionths
            share_check = True
        else:
            share_check = False
    except TypeError:
        share_check = False
    except ValueError:
        share_check = False
    return share_check and price_check
    
@app.route("/")
def index():
    currency = request.args.get('cur')
    wipe = request.args.get('wipe')
    
    if currency != None:
        account.change_currency(currency, my_db)
        
    if wipe == 'yes':
        account.wipe_account(my_db, my_blotter, my_pl)
    return render_template('landing.html')

@app.route("/blotter")
def blotter():
    my_blotter.showBlotter(account)
    if my_blotter.blotter_rows < 1:
        html = "You haven't made any transactions yet!"
    else:
        html = my_blotter.blotter_view.to_html(index = False)
    return render_template('blotter.html', table = html,  currency = account.currency)
    

@app.route("/pl")
def pl():
    my_pl.showPL(account, my_db)
    all_tickers = my_pl.pl_tickers()
    ticker_list = ['<option value="{}"></option>'.format(ticker) for ticker in all_tickers]
    return render_template('pl.html', table = my_pl.pl_view.to_html(index = False), 
                           currency = account.currency, ticker_list = ticker_list)
@app.route('/vwap')
def show_vwap():
    from plotly.offline import plot
    from charts import graph_cols
    
    ticker = request.args.get('ticker')
    
    if ticker != None:
        data = graph_cols(my_pl.pl_hist, ticker, 'wap', 'VWAP History', account)
        my_plot = plot(data, output_type="div", show_link=False)
    else:
        my_plot = 'Ticker not found'
    return render_template('graph.html', my_plot = my_plot)

@app.route('/price')
def show_price():
    from plotly.offline import plot
    from charts import graph_cols
    
    ticker = request.args.get('ticker')
    
    if ticker != None:
        data = graph_cols(my_blotter.blotter, ticker, 'price', 'Trade Price History', account)
        my_plot = plot(data, output_type="div", show_link=False)
    else:
        my_plot = 'Ticker not found'
    return render_template('graph.html', my_plot = my_plot)

@app.route('/cash')
def show_cash():
    from plotly.offline import plot
    from charts import graph_tots
    
    if my_blotter.blotter_rows > 0:
        data = graph_tots(my_blotter.blotter, 'cash_balance', 'Cash Balance', account, my_pl)
        my_plot = plot(data, output_type="div", show_link=False)
    else:
        my_plot = 'No Trading History'
    return render_template('graph.html', my_plot = my_plot)

@app.route('/tpl')
def show_tpl():
    from plotly.offline import plot
    from charts import graph_tots
    
    if my_blotter.blotter_rows > 0:
        data = graph_tots(my_pl.pl_hist, 'tpl', 'Portfolio PL', account, my_pl)
        my_plot = plot(data, output_type="div", show_link=False)
    else:
        my_plot = 'No Trading History'
    return render_template('graph.html', my_plot = my_plot)

@app.route("/trade")
def trade():
    from get_currency_info import find_actives
    ticker_list = ['<option value="{}"></option>'.format(ticker) for ticker in find_actives()]
    
    
    #all currencies that didn't cause problems with algorith
    good_list = ['BTC', 'DASH', 'ETH', 'LTC', 'NXT', 'SC', 'XMR', 'XRP']
    
    cb_string = '<input type = "checkbox" name = "optimize" value="{0}">{0}<br>'
    opt_list = [cb_string.format(ticker) for ticker in good_list]
        
    return render_template('trade.html', ticker_list = ticker_list, opt_list = opt_list)


@app.route('/graph')
def show_graph():
    from plotly.offline import plot
    from get_currency_info import make_chart, find_actives
    
    ticker = request.args.get('ticker')
    
    if ticker != None:
        if ticker.upper() in find_actives():
            data = make_chart(ticker)
            my_plot = plot(data, output_type="div", show_link=False)
            
        else:
            my_plot = 'Ticker not found'
    else:
        my_plot = 'Ticker not found'
    return render_template('graph.html', my_plot = my_plot)

#routes for ajax on trade page

@app.route('/stats', methods = ['POST'])
def show_stats():
    from get_currency_info import get_24, find_actives
    
    ticker =  request.form['ticker']
    
    if ticker.upper() in find_actives():
        mean, stdev, min, max = get_24(ticker)
        return jsonify(stdev=stdev, mean=mean, min=min, max=max)
    
    else:
        return jsonify(mean = 'Ticker not found', stdev = '', min = '', max = '')

@app.route ('/preview', methods = ['POST'])
def preview_trade():
    from get_currency_info import get_current, find_actives
    
    ticker = request.form['ticker']
    trade_type = request.form['type']
    shares = request.form['shares']
    
    if ticker.upper() in find_actives():
        cur = get_current(ticker, trade_type)
        if test_inputs(cur, shares):
            cur_format = "${:,.2f}".format(cur)
            tot = cur * float(shares)
            tot_format = "${:,.2f}".format(tot)
            return (jsonify(current = cur_format, total = tot_format))
        else:
            return jsonify(current = 'Problem with order size or ticker', total = '') 
    
    else:
        return jsonify(current = 'Ticker not found', total = '')

@app.route('/execute', methods = ['POST'])
def executeTrade():
    from get_currency_info import get_current, find_actives
    
    ticker = request.form['ticker'].upper()
    trade_type = request.form['type']
    shares = request.form['shares']
    
    if ticker in find_actives(): #test front end input
        price = get_current(ticker, trade_type)
        if test_inputs(price, shares): #test front end input
            account.evalTransaction(trade_type, float(shares), price, ticker, my_db, my_pl, my_blotter)
            if account.message == 'Success': #test business logic
                return jsonify(message = 'Order Success! {}@${:,.2f}'.format(ticker, price))
            
            else: 
                return(jsonify(message = account.message))
        else:
            return jsonify(message = 'Problem with order size or ticker') 
            
    else:
        return (jsonify(message = 'Ticker not found'))
        
@app.route ('/optimize', methods = ['POST'])
def optimize_portfolio():
    from optimize import prepare_opt
    
    tickers = request.form['tickers'].split(',')
    
    df = prepare_opt(tickers)
    df_html = Markup(df.to_html(index = False))
    return df_html
        
if __name__ == "__main__":
    app.run(host = '0.0.0.0')
    
