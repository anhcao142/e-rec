# -*- coding: utf-8 -*-
"""
Created on Sat Mar 12 22:19:14 2016

@author: PHUONGANH
"""

import pandas as pd
import numpy as np
from sklearn.cross_validation import train_test_split
from sklearn.metrics import mean_squared_error

data_path = '100.csv'
l1 = 25
l2 = 10

def create_train_test(data_path):
    df = pd.read_csv(data_path, header=None, names = ['user', 'item', 'rating'])
    train, test = train_test_split(df, train_size = 0.9)
    train.to_csv('train.csv',index=False)
    test.to_csv('test.csv',index=False)
         
def prepare():
    train = pd.read_csv('train.csv')
    average = train['rating'].mean()
    item = train.groupby(['item'])['rating']
    item_bias = (item.sum() - average*item.count())/(l1+item.count())
    item_bias = pd.DataFrame({'item':item_bias.index, 'bias':item_bias.values})
    item_bias.to_csv('item_bias.csv',index=False)

    grouped = train.groupby('user')
    user_bias = pd.DataFrame(np.nan,index=range(0,len(grouped)),columns=['user','bias'])
    k = 0
    for name, group in grouped:
        s = 0
        print(k)
        for i in range(len(group.index)):
            item = group.iloc[i]['item']
            item_index = item_bias.loc[item_bias['item'] == item].index.tolist()
            if len(item_index)==0:
                bi = 0
            else:
                bi = item_bias.at[item_index[0],'bias']
            s = s + group.iloc[i]['rating'] - average - bi
        bui = s/(l2+len(group.index))
        user_bias.loc[k] = [name, bui]
        k = k+1
    
    user_bias.to_csv('user_bias.csv',index=False)
    return item_bias, user_bias, average 

def bellkor_sim(item_bias, user_bias, average, iid, uid):
    bi = 0
    bu = 0
    item_index = item_bias.loc[item_bias['item'] == iid].index.tolist()
    if len(item_index)!=0:
        bi = item_bias.at[item_index[0],'bias']
    user_index = user_bias.loc[user_bias['user'] == uid].index.tolist()
    if len(user_index)!=0:
        bu = user_bias.at[user_index[0],'bias']
    return average + bi + bu

def bellkor_predictor(item_bias, user_bias, average):
    test = pd.read_csv('test.csv')
    result = pd.DataFrame(np.nan,index=range(0,len(test.index)),columns=['user','item', 'rating', 'predict'])
    #range(len(test.index)    
    for i in range(len(test.index)):
        print(i)
        uid = test.at[i, 'user']
        iid = test.at[i, 'item']
        r = bellkor_sim(item_bias, user_bias, average, iid, uid)
        result.loc[i] = [uid, iid, test.at[i, 'rating'], r]
    result.to_csv('BELLKOR.csv',index=False)
    return result

def evaluate(result):
    RMSE = mean_squared_error(result['rating'], result['predict'])**0.5
    return RMSE
    
def main():
    create_train_test(data_path)
    temp = prepare()
    result = bellkor_predictor(temp[0], temp[1], temp[2])  
    rmse = evaluate(result)
    print rmse

main()
#create_train_test(data_path)



