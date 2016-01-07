package NLP;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.*;

public class FeatureExtraction {
	
	
	private static Set<String> stopWords;
	private static String word1;
	private static String word2;
	private static String mode;// train or test
	
	public static Vector<String> FeatureList = new Vector<String>();
	public static List<List<Integer>> dataList = new ArrayList<List<Integer>>();
	public static Integer featureCount;

	//the constructor for training purpose
	FeatureExtraction(){
		stopWords=new HashSet<>();
		mode = "train";
	}
	//the constructor for testing purpose when there is already model 
	FeatureExtraction(String model_file_path){
		stopWords=new HashSet<>();
		mode = "test";
		try{
			BufferedReader br = new BufferedReader(new FileReader(model_file_path));
			String line;
		    if((line = br.readLine())!=null){
		    	String[] featureList = line.split(", ");
		    	FeatureList.addAll(Arrays.asList(featureList));
		    }
		}catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	    return;
		
	}
	public static boolean isStopWord(String word){
		return stopWords.contains(word);
	}
	public static void readStopWordsFile() throws FileNotFoundException, IOException{
		BufferedReader br = new BufferedReader(new FileReader("/Users/yumengyin/Desktop/stopwd.txt"));
//	    BufferedReader stopWordsFile = new BufferedReader(new FileReader("stopwd.txt"));
	    String line;

	    while((line = br.readLine())!=null){
	        line = line.trim();
	        
	        stopWords.add(line);
	        
	    }
	    br.close();
	    return;
	}
	
	public static void readFileAndFeatureExtraction(String file_path,String word1, String word2){
		try {
			BufferedReader br = new BufferedReader(new FileReader(file_path));
		    String line;
		    Integer count = 0;
		    
		    
		    while((line = br.readLine())!=null){
//		    	List<Integer> list = new ArrayList<Integer>(Arrays.asList(new Integer[FeatureList.size()]));
		    	List<Integer> list;
		    	if(mode.equals("train")){
		    		list = new ArrayList<Integer>(Collections.nCopies(FeatureList.size()+2, 0));
		    	}else{
		    		list = new ArrayList<Integer>(Collections.nCopies(FeatureList.size()+1, 0));
		    	}
		    	if(mode.equals("train")){
			    	String wordSymbol = "";
			    	
			    	Pattern pattern = Pattern.compile(">>(.*?)<<");
			    	Matcher matcher = pattern.matcher(line);
			    	if (matcher.find())
			    	{
			    	    wordSymbol = matcher.group(1).trim().toLowerCase();
	//		    	    System.out.println(wordSymbol);
			    	    Integer target = wordSymbol.equals(word1.toLowerCase()) ? 0 : 1;
			    	    list.set(0, target);  
			    	}
		    	}
		    	Integer ID =  Integer.parseInt(line.split("\t")[0]);

		    	
		    	if(mode.equals("train")){
		    		list.set(1, ID);  
		    	}else{
		    		list.set(0, ID); 
		    	}
//		    	System.out.println(ID);
		    	

		    	line  = line.split("\t")[1].replaceAll("[^a-zA-Z>< ]","").replaceAll("( )+", " ").toLowerCase().trim();

		    	/* get the surrounding word list feature:
		    	 * 
		    	 */	
		    	String[] splictWholeSentence = line.replaceAll("(>>)|(<<)"," ").replaceAll("( )+", " ").split(" ");
		    	for(int i=0 ;i<splictWholeSentence.length;i++){
		    		String word = splictWholeSentence[i].trim();

    				Integer index = FeatureList.indexOf(word);
    				if(index>=2){
    					list.set(index, 1+list.get(index));
    				}else{
    					if(!isStopWord(word)&&(mode.equals("train"))){
	    					FeatureList.add(word);
	    					list.add(1);
    					}
    				}

    			}
//		    	System.out.println(line);	    	
		    	/*get collocation words list feature:
		    	 * 
		    	 */
		    	
		    	String[] splict = line.trim().split("(>>)|(<<)");
//		    	System.out.println("size is "+splict.length);
		    	
		    	
		    	String[][] splictPart = new String[2][];
		    	
		    	
		        switch(splict.length){
		        case 3:
		        	splictPart[0] = splict[0].split(" ");
		        	splictPart[1] = splict[2].split(" ");
		        	break;
		        case 2:
		        	if(splict[0].equals(word1)||splict[0].equals(word2)){
		        		splictPart[1] = splict[1].split(" ");
		        	}else{
		        		splictPart[0] = splict[0].split(" ");
		        	}
		        	break;
		        case 1:
		        	break;
		        }
	    		
		        if(splictPart[0]!=null){
		        	Integer listSize= splictPart[0].length;
		        	if (listSize >=2){
		        		String phrase = splictPart[0][listSize-2].trim()+" "+splictPart[0][listSize-1].trim();
			        	Integer index = FeatureList.indexOf(phrase);
	    				if(index>=2){
	    					list.set(index, 1+list.get(index));
	    				}else{
	    					if(mode.equals("train")){
		    					FeatureList.add(phrase);
		    					list.add(1);
	    					}
	    				}
		        	}
		        }
		        if(splictPart[1]!=null){
		        	Integer listSize= splictPart[1].length;
		        	if (listSize >=2){
		        		String phrase = splictPart[1][listSize-2].trim()+" "+ splictPart[1][listSize-1].trim();
			        	Integer index = FeatureList.indexOf(phrase);
	    				if(index>=2){
	    					list.set(index, 1+list.get(index));
	    				}else{
	    					if(mode.equals("train")){
		    					FeatureList.add(phrase);
		    					list.add(1);
	    					}
	    				}
		        	}
		        }
		    	
		    	dataList.add(list);	    	
//		    	System.out.println(list.size());
//		    	System.out.println("the feature size is"+FeatureList.size()+"\n");
		    	count = count+1;
		    }
		    br.close();
		    
//		    System.out.println(FeatureList);

		    	featureCount = FeatureList.size();
		    
	
//		    System.out.println("the feature size is"+featureCount);

		    
		}catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	    return;
	}
}