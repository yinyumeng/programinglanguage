import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class CheckTestAccuracy {
	public static void main(String[] args){
        String correctOutputFile = args[0];
        String predictOutputFile = args[1];

        Map<String, String> correctOutput = new HashMap<String, String>();

        try {
            BufferedReader br = new BufferedReader(new FileReader(correctOutputFile));

            String line;
            while((line = br.readLine()) != null) {
                String[] splict = line.split("\t");
                correctOutput.put(splict[0], splict[1]);

            }
            br.close();
        }
        catch(FileNotFoundException e) {
        	e.printStackTrace();
        }
        catch(IOException e) {
        	e.printStackTrace();
        }

        int correctCount = 0, falseCount = 0;

        try {
            BufferedReader br = new BufferedReader(new FileReader(predictOutputFile));

            String line;
            while((line = br.readLine()) != null) {
            	String[] splict = line.split("\t");
                if(correctOutput.get(splict[0]).equals(splict[1]))
                    correctCount++;
                else
                    falseCount++;
            }
            br.close();

        }
        catch(FileNotFoundException e) {
        	e.printStackTrace();
        }
        catch(IOException e) {
        	e.printStackTrace();
        }

        System.out.print(1.0*correctCount/(correctCount+falseCount));

    }
}
