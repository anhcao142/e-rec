class DynamicTrainingSVM(object):
    def __init__(self, svm_parameters={}):
        self.svm_parameters = svm_parameters
        self.model = None

    def set_svm_params(self, svm_parameters={}):
        self.svm_parameters = svm_parameters

    def __createRatioIndexData(self, N, m, d, ratio):
        indexSub = []

        x = 0
        P = np.random.permutation(N)

        trainSize = math.ceil(2 * d * ratio)
        testSize = 2 * d - trainSize

        for i in range(m):
            sub = np.array([])

            if i % 2 == 0:
                currentSize = trainSize
            else:
                currentSize = testSize

            if x + currentSize < N:
                sub = P[x : (x + currentSize)]
                x = x + currentSize
            else:
                sub = P[x : N]
                P = np.random.permutation(N)

            indexSub.append(sub)
        return indexSub

    def __get_svc(self, X, y):
        svc = SVC(**self.svm_parameters)
        svc.fit(X,y)
        return svc

    
    def trainWithRatio(self, ratio, xTrain, yTrain, beta):
        if ratio == 0:
            return

        i = 0
        X = xTrain
        Y = yTrain
        N = X.shape[0]
        n = [N]
        
        while True:
            i = i + 1
            d = math.ceil(i*beta*n[i - 1])
            m = math.ceil(n[i - 1]  / d)

            print("i = %d, m = %d, d = %d" %(i, m, d), flush=True)

            trainSize = math.ceil(2 * d * ratio)
            testSize = 2 * d - trainSize

            indexSub = self.__createRatioIndexData(n[i - 1], m, d, ratio)

            trainSVC = None
            nextIndex = []

            for iSub in range(m):
                if (iSub % 2 == 0):
                    trainSVC = self.__get_svc(X[indexSub[iSub],], Y[indexSub[iSub],])
                    nextIndex = np.append(nextIndex, indexSub[iSub][trainSVC.support_])
                else:
                    yExpected = trainSVC.predict(X[indexSub[iSub],])

                    testErrorIndex = np.unique(np.nonzero((yExpected - Y[indexSub[iSub],]) != 0)[0])
                    testSuccessIndex = np.unique(np.nonzero((yExpected - Y[indexSub[iSub],]) == 0)[0])

                    r = len(trainSVC.support_) / trainSize

                    errorRatio = len(testErrorIndex) / (len(testErrorIndex) + len(testSuccessIndex))
                    correctRatio = 1 - errorRatio

                    nextIndex = np.append(nextIndex, indexSub[iSub][testErrorIndex[0 : testSize*errorRatio*r]])
                    nextIndex = np.append(nextIndex, indexSub[iSub][testSuccessIndex[0 : testSize*correctRatio*r]])

            nextIndex = [int(v) for v in nextIndex]

            X = X[nextIndex,]
            Y = Y[nextIndex,]
            n.append(X.shape[0])

            if  m*d*d >= (n[i-1]-n[i])**2:
                break

        svc = SVC(**self.svm_parameters)
        self.model = svc.fit(X,Y)
        return self.model