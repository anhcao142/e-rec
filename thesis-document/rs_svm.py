class RandomizedSamplingSVM(object):
	def __init__(self, svm_parameters={}):
		self.svm_parameters = svm_parameters
		self.model = None

	def set_svm_params(self, svm_parameters={}):
		self.svm_parameters = svm_parameters

	def __union_set(self, samples, new_index):
		index = np.array([],dtype=np.int64)
		N = len(samples)
		for i in range(N):
			sample = samples[i]
			ind = new_index[i]
			index = np.union1d(index, sample[ind])
		return index.tolist()

	def __create_subsamples(self, N, m, k):
		ind = []
		x = 0
		P = np.random.permutation(N)

		for i in range(m):
			sub = np.array([])
			if x + k < N:
				sub = P[x:(x + k)]
				x = x + k
			else:
				sub = P[x:N]
				P = np.random.permutation(N)
				sub = np.append(sub, P[0:k - N + x])
				x = k - N + x
			ind.append(sub)
		return ind
		
	def __get_support_vectors(self, X, y):
		svc = SVC(**self.svm_parameters)
		svc.fit(X, y)
		return svc.support_

	def train(self, X_init, y_init, beta, g):
		c = 1
		i = 0
		X = X_init
		y = y_init
		N = X.shape[0]
		n = [N]
		while True:
			i = i + 1
			k = math.ceil(i*beta*N)
			m = math.ceil(n[i-1] * g / k)
			subsamples = self.__create_subsamples(n[i-1], m, k)
			index = []
			for sample in subsamples:
				index.append(self.__get_support_vectors(X[sample,],y[sample,]))
			new_X_index = self.__union_set(subsamples,index)
			X = X[new_X_index,]
			y = y[new_X_index,]
			n.append(X.shape[0])
			if  g*n[i]*k/c >= (n[i-1]-n[i])**2:
				break
		svc = SVC(**self.svm_parameters)
		self.model = svc.fit(X,y)
		return self.model