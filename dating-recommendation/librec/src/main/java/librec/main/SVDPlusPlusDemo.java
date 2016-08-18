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

package librec.main;

//import librec.rating.SVDPlusPlus;
import librec.util.Debug;
import librec.util.FileIO;
import librec.util.Logs;
import librec.util.Systems;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Scanner;

/**
 * A demo created for the UMAP'15 demo session, could be useful for other users.
 * 
 * @author Guo Guibing
 *
 */
public class SVDPlusPlusDemo {

	public static void main(String[] args) {
		String dirPath = FileIO.makeDirPath("demo");
		String defaultConfigFile = FileIO.makeDirPath(dirPath, "config") + "SVD++_u.conf";

		String content = "dataset.ratings.wins=.\\\\demo\\\\Datasets\\\\e4\\\\%d-u.txt\n" +
				"dataset.ratings.lins=./demo/Datasets/e4/%d-u.txt\n" +
				"\n" +
				"testset.ratings.wins = .\\\\demo\\\\Datasets\\\\e4\\\\test.txt\n" +
				"testset.ratings.lins = ./demo/Datasets/e4/test.txt\n" +
				"\n" +
				"updateset.ratings.wins = .\\\\demo\\\\Datasets\\\\e4\\\\%d-r.txt\n" +
				"updateset.ratings.lins = ./demo/Datasets/e4/%d-r.txt\n" +
				"\n" +
				"fullset.ratings.wins = .\\\\demo\\\\Datasets\\\\e4\\\\%d-t.txt\n" +
				"fullset.ratings.lins = ./demo/Datasets/e4/%d-t.txt\n" +
				"\n" +
				"\n" +
				"ratings.setup=-columns 0 1 2 -threshold -1\n" +
				"\n" +
				"recommender=SVD++\n" +
				"evaluation.setup=train_test_update -k 5 -p off --rand-seed 1 --test-view all\n" +
				"item.ranking=off -topN -1 -ignore -1\n" +
				"\n" +
				"num.factors=100\n" +
				"num.max.iter=1000\n" +
				"\n" +
				"learn.rate=0.01 -max -1 -bold-driver\n" +
				"reg.lambda=0.1 -u 0.1 -i 0.1 -b 0.1 -s 0.001\n" +
				"\n" +
				"output.setup=on -dir ./demo/Results/";

		try {
			SVDPlusPlusDemo demo = new SVDPlusPlusDemo();
			demo.execute(args, "SVD++_init.conf", false);
		} catch (Exception e) {
			e.printStackTrace();
		}

		for (Integer i = 0; i < 10; i++) {
			try {
				System.out.println(String.format(content, i, i, i, i, i, i));
				PrintWriter writer = new PrintWriter(defaultConfigFile, "UTF-8");
				writer.println(String.format(content, i, i, i, i, i, i));
				writer.close();

				SVDPlusPlusDemo demo = new SVDPlusPlusDemo();
				demo.execute(args, "SVD++_u.conf", true);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	public void execute(String[] args, String configFile, Boolean isUpdate) throws Exception {
        // set the folder path for configuration files
        String dirPath = FileIO.makeDirPath("demo");
        String defaultConfigFile = FileIO.makeDirPath(dirPath, "config") + configFile;

        // Config update state
		Debug.isUpdate = isUpdate;

		// config logger
		Logs.config(dirPath + "log4j.xml", true);

        String inputConfigFile;
//        Scanner reader = new Scanner(System.in);
//        System.out.print("Enter config file's location (default '" + defaultConfigFile + "'): ");
//        inputConfigFile = reader.nextLine();

//        if (inputConfigFile.isEmpty()) {
		System.out.println("Empty input, using default config from (" + defaultConfigFile + ")");
		configFile = defaultConfigFile;
//        } else {
//            System.out.println("Using config file from (" + inputConfigFile + ")");
//            configFile = inputConfigFile;
//        }

		// run algorithm
		LibRec librec = new LibRec();
		librec.setConfigFiles(configFile);
		librec.execute(args);

		// await next command
		Logs.debug();
//		Systems.pause();
	}
}
