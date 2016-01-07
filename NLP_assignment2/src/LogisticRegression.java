import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Vector;


public class LogisticRegression {



    /** the learning rate */
    private double learningRate = 0.1;

    /** the weight to learn */
    private double[] weights;

    /** the number of iterations */
    private int maxInterations= 20000;
    private double thresholdDelta = 0.0025 ;
    private String model_path;
    private String answer_path;
    private Integer featureSize;
    private String word1;
    private String word2;
    

    

    public LogisticRegression(Integer featureCount, String model_file_path) {
        this.weights = new double[featureCount];
        this.model_path = model_file_path;
        this.featureSize = featureCount;
    }
    
    public LogisticRegression(String word1, String word2,Integer featureCount, String model_file_path, String answer_file_path) {
        this.weights = new double[featureCount];
        this.model_path = model_file_path;
        this.featureSize = featureCount;
        this.answer_path = answer_file_path;
        this.word1 = word1;
        this.word2 = word2;
        
    }

    private double sigmoid(double z) {
        return 1 / (1 + Math.exp(-z));
    }

    public void train(List<List<Integer>> InputData, Vector<String> FeatureList){
    	try{
	        BufferedWriter bw = new BufferedWriter(new FileWriter(model_path));
//	        System.out.println(FeatureList.subList(2, FeatureList.size()).toString().replaceAll("[^a-zA-Z, ]",""));
	        bw.write(FeatureList.subList(2, FeatureList.size()).toString().replaceAll("[^a-zA-Z, ]",""));
	        
	        
	        for (int n=0; n<maxInterations; n++) {
	            double lik = 0.0;
	         // Store previous weights
	            double[] weightsPrev = new double[weights.length];
	            for (int i=0; i<InputData.size(); i++) {
	            	List<Integer> x = InputData.get(i).subList(2,InputData.get(i).size());
	            	
	                double predicted = classify(x);
	                int label = InputData.get(i).get(0);
//	                System.out.println("the label is "+label+"the predicted is "+predicted);
	                for (int j=0; j<x.size(); j++) {
	                    weights[j] = weights[j] + learningRate * (label - predicted) * x.get(j);
	                    
	                }
	                // not necessary for learning
	                lik += label * Math.log(classify(x)) + (1-label) * Math.log(1- classify(x));
	            }
	            if (n%100 == 0)
	            	System.out.println("iteration: " + n + " "+ " mle: " + lik+" " + Arrays.toString(weights) );
	            
	            double[] delta = new double[weights.length];
	            double sumDelta = 0.0;
	            
	            	for (int i=0; i<InputData.size(); i++) {
	            		List<Integer> x = InputData.get(i).subList(2,InputData.get(i).size());
	            		double predicted = classify(x);
	            		int label = InputData.get(i).get(0);
	            		for (int j=0; j<x.size(); j++) {
							delta[j] = delta[j]+(label - predicted) * x.get(j);
					}

	            }
	            	for(int i =0;i<delta.length;i++){
	            		sumDelta = sumDelta+delta[i]*delta[i];
	            	}

	            
				if (n%100 == 0)
	            	System.out.println("sqrt of sumdelta is : " + Math.sqrt(sumDelta)  );
				if(Math.sqrt(sumDelta) <thresholdDelta) {
					System.out.println("STOP criteria met: Iteration " + n + ", Log-likelihood = " + lik+" sqrt of sumdelta " + delta);
					break;
				}
				
	        }

	        bw.write("\n");
	        for(int i =0;i<featureSize-1;i++){
//	    		System.out.println(weights[i]);
	    		bw.write(Double.toString(weights[i])+", ");
	    	}
	        bw.write(Double.toString(weights[featureSize-1]));
//	        System.out.println("size is: " +  (FeatureList.size()-2));
//	        System.out.println("size is: " + weights.length);
	        bw.close();
		}catch (IOException e) {
		    e.printStackTrace();
		     
		}
    }

    private double classify(List<Integer> x) {
        double y = 0.0;
        for (int i=0; i<weights.length;i++)  {
        	if(i>=x.size()){
        		y += weights[i] * 0;
        	}else{
        		y += weights[i] * x.get(i);
        	}
//            System.out.println("the weight is =  " +weights[i]);
        }
        return sigmoid(y);
    }

    public void test(List<List<Integer>> InputData, Vector<String> FeatureList){
    	try{
			BufferedReader br = new BufferedReader(new FileReader(model_path));
			String line;
			line = br.readLine();
		    if((line = br.readLine())!=null){
		    	String[] weightList = line.split(", ");
		    	for(int i =0;i<featureSize;i++){
		    		weights[i] = Double.parseDouble(weightList[i]);
//		    		System.out.println(weights[i]);
		    	}
		    }
    		
	        BufferedWriter bw = new BufferedWriter(new FileWriter(answer_path));
//	        System.out.println(FeatureList.subList(1, FeatureList.size()).toString().replaceAll("[^a-zA-Z, ]",""));  
	        
            for (int i=0; i<InputData.size(); i++) {
            	List<Integer> x = InputData.get(i).subList(1,InputData.get(i).size());
//            	System.out.println("the 2d array is = " +InputData.get(i));
//            	System.out.println("the 2d array size is = " +x.size());
//            	System.out.println("weight is = " +weights);
                double predicted = classify(x);
//                System.out.println("the probability is = " +predicted);
                String ID = String.format("%04d", InputData.get(i).get(0));
                String symbol = predicted > 0.5 ? this.word2 : this.word1;
                bw.write(ID+"\t"+symbol+"\n");
                
            }

	        bw.close();
		}catch (IOException e) {
		    e.printStackTrace();
		     
		}
    }

}