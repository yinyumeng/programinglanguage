package NLP;


import java.io.IOException;

import NLP.FeatureExtraction;

public class sctrain {
	
	private static String word1;
	private static String word2;
	private static String train_file_path;
	private static String model_file_path;
	private static FeatureExtraction feaExt;
	private static LogisticRegression logisticRegression;
	
	
	
	public static void main(String[] args)
    {
		 long startTime = System.currentTimeMillis();
		word1=args[0];
		word2=args[1];
		train_file_path=args[2];
		model_file_path=args[3];
		feaExt = new FeatureExtraction();
		try {
			feaExt.readStopWordsFile();
		} catch (IOException e) {
			e.printStackTrace();
		}
		feaExt.readFileAndFeatureExtraction(train_file_path, word1, word2);
		
		logisticRegression = new LogisticRegression(feaExt.featureCount,model_file_path);
		logisticRegression.train(feaExt.dataList,feaExt.FeatureList);
		
		long endTime = System.currentTimeMillis();
		System.out.println(endTime - startTime);
		
		
    }
}


