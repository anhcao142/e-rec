# -*- coding: utf-8 -*-
"""
Created on Sat Mar 12 22:19:14 2016

@author: PHUONGANH
"""

import pandas as pd
import numpy as np
import sys
from sklearn.cross_validation import train_test_split
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error

data_path = sys.argv[1]
minimumRatings = sys.argv[2]

l1 = 25
l2 = 10

def prepare_input_data(data_path, minimumRatings):
    filename = 'least' + minimumRatings + '_data.csv'

    df = pd.read_csv(data_path, header = None, names = ['user', 'item', 'rating'])
    groupedUser = df.groupby(['user'], as_index = False)['item'].count()
    groupedUser = groupedUser[groupedUser.item >= float(minimumRatings)]
    prepared = df[df['user'].isin(groupedUser.user)]
    prepared.to_csv(filename, index = False)
    print 'Prepare input data: DONE'
    return filename


def create_train_test(prefix, data_path):
    trainPath = prefix + '_train.csv'
    testPath = prefix + '_test.csv'

    df = pd.read_csv(data_path)
    train, test = train_test_split(df, train_size = 0.9)
    train.to_csv(trainPath, index = False)
    test.to_csv(testPath, index = False)

    print 'Prepare Train and Test data: DONE'
    return trainPath, testPath

def prepare(prefix, trainPath):
    item_bias_path = prefix + '_item_bias.csv'
    user_bias_path = prefix + '_user_bias.csv'

    train = pd.read_csv(trainPath)
    print train

    average = train['rating'].mean()
    item = train.groupby(['item'])['rating']
    item_bias = (item.sum() - average*item.count())/(l1+item.count())
    item_bias = pd.DataFrame({'item':item_bias.index, 'bias':item_bias.values})
    item_bias.to_csv(item_bias_path, index = False)

    grouped = train.groupby('user')
    user_bias = pd.DataFrame(np.nan,index=range(0,len(grouped)),columns=['user','bias'])
    k = 0
    for name, group in grouped:
        s = 0
        if k == 5000:
            print 'Looped: 5000 times'
            k = 0

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

    user_bias.to_csv(user_bias_path,index=False)

    print 'Prepare data: DONE'
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

def bellkor_predictor(item_bias, user_bias, average, testPath, prefix):
    print 'Begin Bellkor prediction'

    test = pd.read_csv(testPath)
    result = pd.DataFrame(np.nan,index=range(0,len(test.index)),columns=['user','item', 'rating', 'predict'])
    #range(len(test.index)
    for i in range(len(test.index)):
        print(i)
        uid = test.at[i, 'user']
        iid = test.at[i, 'item']
        r = bellkor_sim(item_bias, user_bias, average, iid, uid)
        result.loc[i] = [uid, iid, test.at[i, 'rating'], r]
    result.to_csv(prefix + '_BELLKOR.csv',index=False)
    return result

def evaluate(result):
    print 'Calculating result...'
    RMSE = mean_squared_error(result['rating'], result['predict'])**0.5
    MAE = mean_absolute_error(result['rating'],result['predict'])
    return RMSE, MAE

def main():
    print 'Processing data from ' + data_path + ' with every user must have at least ' + minimumRatings + ' ratings.'

    newDataPath = prepare_input_data(data_path, minimumRatings)
    prefix = 'least' + minimumRatings
    trainPath, testPath = create_train_test(prefix, newDataPath)
    temp = prepare(prefix, trainPath)
    result = bellkor_predictor(temp[0], temp[1], temp[2], testPath, prefix)
    error = evaluate(result)
    print error[0]
    print error[1]
    print '\nDONE'

main()



