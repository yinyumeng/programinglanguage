package read_file;

import java.io.*;
import java.net.SocketTimeoutException;
import java.util.Random;

public class ExtractErrror {
	
	//path be modified by user
	public static String inputPath="/Users/yumengyin/Documents/workspace/read_file/Resource/";
	public static String outputPath="/Users/yumengyin/Documents/workspace/read_file/Resource/output.txt";
    
	public static void main(String[] args){
        int num_NA=0;
        int counter = 0;

        File folder = new File(inputPath);
        File[] listOfFiles = folder.listFiles();
        
        String fileName="";
        try{
	        BufferedWriter bw = new BufferedWriter(new FileWriter(outputPath));
	        for (File file : listOfFiles) {
	            if (file.isFile()&&file.getName().contains(".out")) {
	            	fileName = file.getName();
	                System.out.println(fileName);
	                   
	                try {
	                    BufferedReader br = new BufferedReader( new FileReader(inputPath+fileName));
	                    
	                    String line;
	                    String errorInfoLine="";
	                    String result="";
	
	                    while((line=br.readLine())!=null) {
	                    	if (counter++ % 100 == 0) {
	                            System.out.println("counter = " + counter);
	                        }
	                    	
	                    	if(line.contains("INFO:neon.models.mlp:epoch: ")){
	              
	                    		errorInfoLine += line.split(" ")[4]+"\t";
	                    		 num_NA++;
	                    	}else if(line.contains("Got result ")){
	                    		result = line.split(" ")[2]+"\t";
	                    		 num_NA++;
	                    	}
	                      
	                    }
	                    
	                    System.out.println(fileName.split(".out")[0]+"\t"+result + errorInfoLine);
	                    bw.write(fileName.split(".out")[0]+"\t"+result + errorInfoLine + "\n");
	                    br.close();
	                    
	                } catch (IOException e) {
	                    e.printStackTrace();
	                  
	                }
	            }
	            
	        }
	        bw.close();
	        
        }catch (IOException e) {
            e.printStackTrace();
            
        }
        
        
    }
	
}
