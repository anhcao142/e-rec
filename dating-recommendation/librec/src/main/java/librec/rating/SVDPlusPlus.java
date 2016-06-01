// Copyright (C) 2014 Guibing Guo
//
// This file is part of LibRec.
//
// LibRec is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// LibRec is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with LibRec. If not, see <http://www.gnu.org/licenses/>.
//

package librec.rating;

import java.util.List;

import librec.data.DenseMatrix;
import librec.data.DenseVector;
import librec.data.MatrixEntry;
import librec.data.SparseMatrix;
import librec.util.FileIO;
import librec.util.Logs;

/**
 * Yehuda Koren, <strong>Factorization Meets the Neighborhood: a Multifaceted Collaborative Filtering Model.</strong>,
 * KDD 2008.
 * 
 * @author guoguibing
 * 
 */
public class SVDPlusPlus extends BiasedMF {

	protected DenseMatrix Y;
	protected SparseMatrix updateMatrix;

	public SVDPlusPlus(SparseMatrix trainMatrix, SparseMatrix testMatrix, int fold) {
		super(trainMatrix, testMatrix, fold);

		setAlgoName("SVD++");
	}

	@Override
	protected void initModel() throws Exception {
		super.initModel();

		Y = new DenseMatrix(numItems, numFactors);
		Y.init(initMean, initStd);

		userItemsCache = trainMatrix.rowColumnsCache(cacheSpec);
	}
	
	protected void saveModel() throws Exception {
		// make a folder
		Logs.debug("SAVE");
		String dirPath = FileIO.makeDirectory(tempDirPath, algoName);

		// suffix info
		String suffix = foldInfo + ".bin";

		// writing training, test data
		FileIO.serialize(trainMatrix, dirPath + "trainMatrix" + suffix);
		FileIO.serialize(testMatrix, dirPath + "testMatrix" + suffix);

		// write matrices P, Q
		FileIO.serialize(P, dirPath + "userFactors" + suffix);
		FileIO.serialize(Q, dirPath + "itemFactors" + suffix);
		
		FileIO.serialize(Y, dirPath + "itemSecondFactors" + suffix);

		// write vectors
		if (userBias != null)
			FileIO.serialize(userBias, dirPath + "userBiases" + suffix);
		if (itemBias != null)
			FileIO.serialize(itemBias, dirPath + "itemBiases" + suffix);

		Logs.debug("Learned models are saved to folder \"{}\"", dirPath);
	}

	
	protected void loadModel() throws Exception {
		// make a folder
		Logs.debug("LOAD");
		String dirPath = FileIO.makeDirectory(tempDirPath, algoName);

		Logs.debug("A recommender model is loaded from {}", dirPath);

		// suffix info
		String suffix = foldInfo + ".bin";

		//trainMatrix = (SparseMatrix) FileIO.deserialize(dirPath + "trainMatrix" + suffix);
		testMatrix = (SparseMatrix) FileIO.deserialize(dirPath + "testMatrix" + suffix);
		updateMatrix = (SparseMatrix) FileIO.deserialize(dirPath + "trainMatrix" + "update.bin");

		// write matrices P, Q
		P = (DenseMatrix) FileIO.deserialize(dirPath + "userFactors" + suffix);
		Q = (DenseMatrix) FileIO.deserialize(dirPath + "itemFactors" + suffix);
		Y = (DenseMatrix) FileIO.deserialize(dirPath + "itemSecondFactors" + suffix);

		// write vectors
		userBias = (DenseVector) FileIO.deserialize(dirPath + "userBiases" + suffix);
		itemBias = (DenseVector) FileIO.deserialize(dirPath + "itemBiases" + suffix);
		
		userItemsCache = trainMatrix.rowColumnsCache(cacheSpec);
	}


	@Override
	protected void buildModel() throws Exception {

		Logs.debug("BUILD");
		for (int iter = 1; iter <= numIters; iter++) {

			loss = 0;
			for (MatrixEntry me : trainMatrix) {
				int u = me.row(); // user
				int j = me.column(); // item
				double ruj = me.get();

				double pred = predict(u, j);
				double euj = ruj - pred;

				loss += euj * euj;

				List<Integer> items = userItemsCache.get(u);
				double w = Math.sqrt(items.size());

				// update factors
				double bu = userBias.get(u);
				double sgd = euj - regB * bu;
				userBias.add(u, lRate * sgd);

				loss += regB * bu * bu;

				double bj = itemBias.get(j);
				sgd = euj - regB * bj;
				itemBias.add(j, lRate * sgd);

				loss += regB * bj * bj;

				double[] sum_ys = new double[numFactors];
				for (int f = 0; f < numFactors; f++) {
					double sum_f = 0;
					for (int k : items)
						sum_f += Y.get(k, f);

					sum_ys[f] = w > 0 ? sum_f / w : sum_f;
				}

				for (int f = 0; f < numFactors; f++) {
					double puf = P.get(u, f);
					double qjf = Q.get(j, f);

					double sgd_u = euj * qjf - regU * puf;
					double sgd_j = euj * (puf + sum_ys[f]) - regI * qjf;

					P.add(u, f, lRate * sgd_u);
					Q.add(j, f, lRate * sgd_j);

					loss += regU * puf * puf + regI * qjf * qjf;

					for (int k : items) {
						double ykf = Y.get(k, f);
						double delta_y = euj * qjf / w - regU * ykf;
						Y.add(k, f, lRate * delta_y);

						loss += regU * ykf * ykf;
					}
				}

			}
			
			//Logs.debug(i);
			loss *= 0.5;

			if (isConverged(iter))
				break;

		}// end of training

	}
	
//	protected void rebuildModel() throws Exception {
//		Logs.debug("REBUILD ++");
//		for (int iter = 1; iter <= numIters; iter++) {
//
//			loss = 0;
//			for (MatrixEntry me : trainMatrix) {
//
//				int u = me.row(); // user
//				int j = me.column(); // item
//				double ruj = me.get();
//
//				double pred = predict(u, j);
//				double euj = ruj - pred;
//
//				loss += euj * euj;
//
//				List<Integer> items = userItemsCache.get(u);
//				double w = Math.sqrt(items.size());
//
//				// update factors
//				double bu = userBias.get(u);
//				double sgd = euj - regB * bu;
//				userBias.add(u, lRate * sgd);
//
//				loss += regB * bu * bu;
//
//				double bj = itemBias.get(j);
//				sgd = euj - regB * bj;
//				itemBias.add(j, lRate * sgd);
//
//				loss += regB * bj * bj;
//
//				double[] sum_ys = new double[numFactors];
//				for (int f = 0; f < numFactors; f++) {
//					double sum_f = 0;
//					for (int k : items)
//						sum_f += Y.get(k, f);
//
//					sum_ys[f] = w > 0 ? sum_f / w : sum_f;
//				}
//
//				for (int f = 0; f < numFactors; f++) {
//					double puf = P.get(u, f);
//					double qjf = Q.get(j, f);
//
//					double sgd_u = euj * qjf - regU * puf;
//					double sgd_j = euj * (puf + sum_ys[f]) - regI * qjf;
//
//					P.add(u, f, lRate * sgd_u);
//					Q.add(j, f, lRate * sgd_j);
//
//					loss += regU * puf * puf + regI * qjf * qjf;
//
//					for (int k : items) {
//						double ykf = Y.get(k, f);
//						double delta_y = euj * qjf / w - regU * ykf;
//						Y.add(k, f, lRate * delta_y);
//
//						loss += regU * ykf * ykf;
//					}
//				}
//
//			}
//			
//			int i = 0;
//			for (MatrixEntry me : updateMatrix) {
//				++i;
//				int u = me.row(); // user
//				int j = me.column(); // item
//				double ruj = me.get();
//
//				double pred = predict(u, j);
//				double euj = ruj - pred;
//
//				loss += euj * euj;
//
//				List<Integer> items = userItemsCache.get(u);
//				double w = Math.sqrt(items.size());
//
//				// update factors
//				double bu = userBias.get(u);
//				double sgd = euj - regB * bu;
//				userBias.add(u, lRate * sgd);
//
//				loss += regB * bu * bu;
//
//				double bj = itemBias.get(j);
//				sgd = euj - regB * bj;
//				itemBias.add(j, lRate * sgd);
//
//				loss += regB * bj * bj;
//
//				double[] sum_ys = new double[numFactors];
//				for (int f = 0; f < numFactors; f++) {
//					double sum_f = 0;
//					for (int k : items)
//						sum_f += Y.get(k, f);
//
//					sum_ys[f] = w > 0 ? sum_f / w : sum_f;
//				}
//
//				for (int f = 0; f < numFactors; f++) {
//					double puf = P.get(u, f);
//					double qjf = Q.get(j, f);
//
//					double sgd_u = euj * qjf - regU * puf;
//					double sgd_j = euj * (puf + sum_ys[f]) - regI * qjf;
//
//					P.add(u, f, lRate * sgd_u);
//					Q.add(j, f, lRate * sgd_j);
//
//					loss += regU * puf * puf + regI * qjf * qjf;
//
//					for (int k : items) {
//						double ykf = Y.get(k, f);
//						double delta_y = euj * qjf / w - regU * ykf;
//						Y.add(k, f, lRate * delta_y);
//
//						loss += regU * ykf * ykf;
//					}
//				}				
//			}
//			Logs.debug("i ++"+ i);
//			loss *= 0.5;
//
//			if (isConverged(iter))
//				break;
//		}
//	}
//	

	
	
	protected void rebuildModel() throws Exception {
		Logs.debug("REBUILD ++");
		for (int iter = 1; iter <= numIters; iter++) {

			loss = 0;
			int[] userid = new int[updateMatrix.numRows()];
			int[] itemid = new int[updateMatrix.numColumns()];
			int i = 0;
			for (MatrixEntry me : trainMatrix) {

				int u = me.row(); // user
				int j = me.column(); // item
				userid[i] = u;
				itemid[i] = j;
				++i;
				double ruj = me.get();

				double pred = predict(u, j);
				double euj = ruj - pred;

				loss += euj * euj;

				List<Integer> items = userItemsCache.get(u);
				double w = Math.sqrt(items.size());

				// update factors
				double bu = userBias.get(u);
				double sgd = euj - regB * bu;
				userBias.add(u, lRate * sgd);

				loss += regB * bu * bu;

				double bj = itemBias.get(j);
				sgd = euj - regB * bj;
				itemBias.add(j, lRate * sgd);

				loss += regB * bj * bj;

				double[] sum_ys = new double[numFactors];
				for (int f = 0; f < numFactors; f++) {
					double sum_f = 0;
					for (int k : items)
						sum_f += Y.get(k, f);

					sum_ys[f] = w > 0 ? sum_f / w : sum_f;
				}

				for (int f = 0; f < numFactors; f++) {
					double puf = P.get(u, f);
					double qjf = Q.get(j, f);

					double sgd_u = euj * qjf - regU * puf;
					double sgd_j = euj * (puf + sum_ys[f]) - regI * qjf;

					P.add(u, f, lRate * sgd_u);
					Q.add(j, f, lRate * sgd_j);

					loss += regU * puf * puf + regI * qjf * qjf;

					for (int k : items) {
						double ykf = Y.get(k, f);
						double delta_y = euj * qjf / w - regU * ykf;
						Y.add(k, f, lRate * delta_y);

						loss += regU * ykf * ykf;
					}
				}

			}
			
			for (MatrixEntry me : updateMatrix) {

				int u = me.row(); // user
				int j = me.column(); // item
				
				double ruj = me.get();

				double pred = predict(u, j);
				double euj = ruj - pred;

				loss += euj * euj;
				
				if(contains(userid, u)) {
					List<Integer> items = userItemsCache.get(u);
					double w = Math.sqrt(items.size());

					// update factors
					double bu = userBias.get(u);
					double sgd = euj - regB * bu;
					userBias.add(u, lRate * sgd);

					loss += regB * bu * bu;

					double bj = itemBias.get(j);

					loss += regB * bj * bj;

					for (int f = 0; f < numFactors; f++) {
						double puf = P.get(u, f);
						double qjf = Q.get(j, f);

						double sgd_u = euj * qjf - regU * puf;

						P.add(u, f, lRate * sgd_u);

						loss += regU * puf * puf + regI * qjf * qjf;

						for (int k : items) {
							double ykf = Y.get(k, f);

							loss += regU * ykf * ykf;
						}
					}

				}
				else {
					List<Integer> items = userItemsCache.get(u);
					double w = Math.sqrt(items.size());

					// update factors
					double bu = userBias.get(u);
					loss += regB * bu * bu;

					double bj = itemBias.get(j);

					loss += regB * bj * bj;

					double[] sum_ys = new double[numFactors];
					for (int f = 0; f < numFactors; f++) {
						double sum_f = 0;
						for (int k : items)
							sum_f += Y.get(k, f);

						sum_ys[f] = w > 0 ? sum_f / w : sum_f;
					}

					for (int f = 0; f < numFactors; f++) {
						double puf = P.get(u, f);
						double qjf = Q.get(j, f);

						loss += regU * puf * puf + regI * qjf * qjf;

						for (int k : items) {
							double ykf = Y.get(k, f);
							loss += regU * ykf * ykf;
						}
					}

				}
				
			}
			loss *= 0.5;

			if (isConverged(iter))
				break;
		}
	}
	
	@Override
	protected double predict(int u, int j) throws Exception {
		double pred = globalMean + userBias.get(u) + itemBias.get(j) + DenseMatrix.rowMult(P, u, Q, j);

		List<Integer> items = userItemsCache.get(u);
		double w = Math.sqrt(items.size());
		for (int k : items)
			pred += DenseMatrix.rowMult(Y, k, Q, j) / w;

		return pred;
	}
	
	public static boolean contains(final int[] list, final int u) { 
		for (int e : list) { 
			if (e == u ) { 
				return true; 
			} 
		} 
		return false; 
	}

}
