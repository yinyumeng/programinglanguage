//import java.security.Key;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by huihui on 20/10/15.
 */
public class Correctness {

    //java Correctness adapt_adopt.answer adapt_adopt.answer.test
    //java Correctness bought_brought.answer bought_brought.answer.test
    //java Correctness peace_piece.answer peace_piece.answer.test

    public static void main(String[] args){
        String file1 = args[0];
        String file2 = args[1];

        Map<String, String> res = new HashMap<String, String>();

        try {

            FileReader fileReader = new FileReader(file1);
            BufferedReader bufferedReader = new BufferedReader(fileReader);

            String line;
            while((line = bufferedReader.readLine()) != null) {
                

                String[] sen = line.split("\t");
                res.put(sen[0], sen[1]);

            }
            bufferedReader.close();
        }
        catch(FileNotFoundException ex) {
            System.out.println("Unable to open file " + file1);
        }
        catch(IOException ex) {
            System.out.println("Error reading file " + file1);
        }

        int c = 0, w = 0;

        try {

            FileReader fileReader = new FileReader(file2);
            BufferedReader bufferedReader = new BufferedReader(fileReader);

            String line;
            while((line = bufferedReader.readLine()) != null) {
             
                String[] sen = line.split("\t");

                if(res.get(sen[0]).equals(sen[1]))
                    c++;
                else
                    w++;
            }
            bufferedReader.close();

        }
        catch(FileNotFoundException ex) {
            System.out.println("Unable to open file " + file2);
        }
        catch(IOException ex) {
            System.out.println("Error reading file " + file2);
        }

        //System.out.println(c + " " + w);
        System.out.print(1.0*c/(c+w));

    }

}
