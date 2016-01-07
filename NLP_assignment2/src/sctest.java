import java.io.IOException;

public class sctest {
	
	private static String word1;
	private static String word2;
	private static String test_file_path;
	private static String model_file_path;
	private static String answer_file_path;
	private static FeatureExtraction feaExt;
	private static LogisticRegression logisticRegression;
	
	
	public static void main(String[] args)
    {//word1 word2 test_file model_file answer_file
		long startTime = System.currentTimeMillis();
		word1=args[0];
		word2=args[1];
		test_file_path=args[2];
		model_file_path=args[3];
		answer_file_path=args[4];
		feaExt = new FeatureExtraction(model_file_path);
		try {
			feaExt.readStopWordsFile();
		} catch (IOException e) {
			e.printStackTrace();
		}
		feaExt.readFileAndFeatureExtraction(test_file_path, word1, word2);
		
		logisticRegression = new LogisticRegression(word1,word2,feaExt.featureCount,model_file_path,answer_file_path);
		logisticRegression.test(feaExt.dataList, feaExt.FeatureList);
		long endTime = System.currentTimeMillis();
		System.out.println(endTime - startTime);
    }
}


