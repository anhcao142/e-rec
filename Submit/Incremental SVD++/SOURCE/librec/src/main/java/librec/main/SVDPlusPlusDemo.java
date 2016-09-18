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
		Boolean isUpdate = true;
		
		String configFile = null;
		if(isUpdate == true)
			configFile = "SVD++_u.conf";
		else 
			configFile = "SVD++_init.conf";
		try {
			SVDPlusPlusDemo demo = new SVDPlusPlusDemo();
			demo.execute(args, configFile, isUpdate);
		} catch (Exception e) {
			e.printStackTrace();
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
		librec = null;
		System.gc();
//		Systems.pause();
	}
}
