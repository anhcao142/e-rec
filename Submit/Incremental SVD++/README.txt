SETUP:
- open source code in an IDE (eclipse)
- copy dataset of the test to demo/dataset

RUN:
- SVD++
	- In "src/main/java/librec.main/SVDPlusPlusDemo.java": 
		+ assign "false" to "isUpdate" 
	- In "demo/config/SVD++_init.conf": 
		+ assign file path to "dataset.ratings.wins" or "dataset.ratings.lins" depend on the operating system
		   EX: dataset.ratings.wins=.\\demo\\Datasets\\exp1\\0-t.txt 
		     or dataset.ratings.wins=.\\demo\\Datasets\\exp1\\1-t.txt
		+ assign file path to "testset.ratings.wins" or "testset.ratings.lins" depend on the operating system
		   EX: testset.ratings.wins=.\\demo\\Datasets\\exp1\\test.txt
	
-Incremental SVD++ 
	INIT MODEL
	- In "src/main/java/librec.main/SVDPlusPlusDemo.java": 
		+ assign "false" to "isUpdate" 
	- In "demo/config/SVD++_init.conf": 
		+ assign file path to "dataset.ratings.wins" or "dataset.ratings.lins" depend on the operating system
		   EX: dataset.ratings.wins=.\\demo\\Datasets\\exp1\\init.txt 
		+ assign file path to "testset.ratings.wins" or "testset.ratings.lins" depend on the operating system
		   EX: testset.ratings.wins=.\\demo\\Datasets\\exp1\\test.txt
	
	UPDATE MODEL
	- In "src/main/java/librec.main/SVDPlusPlusDemo.java": 
		+ assign "true" to "isUpdate" 
	- In "demo/config/SVD++_u.conf": 
		+ assign file path to "dataset.ratings.wins" or "dataset.ratings.lins" depend on the operating system
		   EX: dataset.ratings.wins=.\\demo\\Datasets\\exp1\\0-u.txt 
		     or dataset.ratings.wins=.\\demo\\Datasets\\exp1\\1-u.txt 
		+ assign file path to "updateset.ratings.wins" or "updateset.ratings.lins" depend on the operating system
		   EX: updateset.ratings.wins=.\\demo\\Datasets\\exp1\\0-r.txt 
		     or updateset.ratings.wins=.\\demo\\Datasets\\exp1\\1-r.txt 
		+ assign file path to "fullset.ratings.wins" or "fullset.ratings.lins" depend on the operating system
		   EX: fullset.ratings.wins=.\\demo\\Datasets\\exp1\\0-t.txt 
		     or fullset.ratings.wins=.\\demo\\Datasets\\exp1\\1-t.txt 
		+ assign file path to "testset.ratings.wins" or "testset.ratings.lins" depend on the operating system
		   EX: testset.ratings.wins=.\\demo\\Datasets\\exp1\\test.txt

- Result of tests is written in demo/Results