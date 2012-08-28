package lexicon.builder;

import java.io.*;

import org.apache.pdfbox.pdmodel.*;
import org.apache.pdfbox.util.*;
import org.apache.poi.hwpf.HWPFDocument;
import org.apache.poi.hwpf.extractor.WordExtractor;
import org.apache.poi.xwpf.model.*;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.poi.xwpf.extractor.*;
import java.util.regex.*;

// TODO: Auto-generated Javadoc
/**
 * The Class TextConvertor.
 */
public class TextConvertor {
	
	/** The pd. */
	PDDocument pd;
	
	/** The dx. */
	XWPFDocument dx;
	
	/** The dc. */
	HWPFDocument dc;
	
	/** The wr. */
	BufferedWriter wr;
	
	/** The stripper. */
	PDFTextStripper stripper;
	
	/** The filetype. */
	String filetype;
	
	
	/**
	 * Constructor sets the input and output file and convert the pdf, docx and doc files to text .
	 *
	 * @param infile the infile
	 * @param outfile the outfile
	 */
	public TextConvertor(String infile, String outfile){
		 try {
		         File input = new File(infile);  // The file from where you would like to extract
		         FileInputStream fis=new FileInputStream(input.getAbsolutePath());
		         int x=fis.read();
		         int y=fis.read();
		         fis.close();
		         fis=null;
		         fis=new FileInputStream(input.getAbsolutePath());
		         int avl=fis.available();
		         System.out.println("Available"+avl);
		         if (avl <100000000){
		         if (x==37 && y==80){
		        	 filetype="pdf";
		        	 System.out.println("It's PDF");
		        	 pd = PDDocument.load(input);
		        	 PDF2TextPreProssesd( outfile);
		         }else if (x==80 && y==75){
		        	 filetype="docx";
		        	 System.out.println("It's DOCX");
		        	  dx =new XWPFDocument(fis);
		        	  DOCX2Text( outfile);
		         }else if (x==208 && y==207){
		        	 filetype="doc";
		        	 System.out.println("It's DOC");
		        	  dc=new HWPFDocument(fis);
		        	  DOC2Text( outfile);
		         } 
		         }
		         pd=null;
		         dx=null;
		         dc=null;
		         fis.close();	         
		 } catch (Exception e){
		         e.printStackTrace();
		 } 
	}


 /**
  * save the converted text (without any processing) to the given file.
  *
  * @param filename the filename
  */
public void DOCX2Text(String filename){
	 try {
       File output = new File(filename); // The text file where you are going to store the extracted data
      
       XWPFWordExtractor ex =new XWPFWordExtractor(dx);
		
       String[] fileData = ex.getText().split(".");
       
       
      wr = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(output)));
      for (String s:fileData){
    	  wr.write(s +".");
      }
      
      wr.close();
     
	 } catch (Exception e){
      e.printStackTrace();
     } 
}

/**
 * save the converted text (without any processing) to the given file.
 *
 * @param filename the filename
 */
public void DOC2Text(String filename){
 try {
   File output = new File(filename); // The text file where you are going to store the extracted data
   wr = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(output)));
   WordExtractor extractor = new WordExtractor(dc);
	String [] fileData = extractor.getParagraphText();
	for(int i=0;i<fileData.length;i++){
	if(fileData[i] != null)
		 wr.write(fileData[i]);
	}
   
  wr.close();
  
 } catch (Exception e){
  e.printStackTrace();
 } 
}
 
 /**
  * save the converted text (without any processing) to the given file.
  *
  * @param filename the filename
  */
 public void PDF2Text(String filename){
	 try {
          File output = new File(filename); // The text file where you are going to store the extracted data
         
          stripper = new PDFTextStripper();
          
          
         wr = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(output)));
         
         stripper.writeText(pd, wr);
         
         if (pd != null) {
             pd.close();
         }
         wr.close();
 } catch (Exception e){
         e.printStackTrace();
        } 
	 
	 
	 
	 
 }
 
 /**
  * PD f2 text pre prossesd.
  *
  * @param filename the filename
  */
 public void PDF2TextPreProssesd(String filename){
	 try {
               
         stripper = new PDFTextStripper();
         stripper.setParagraphStart("&*&"); 
         stripper.setLineSeparator("#%l#");
        stripper.setPageEnd("#%p#");
        
         String fulltxt=stripper.getText(pd) ;
         fulltxt=fulltxt.replaceAll("-#%l#", "");
         fulltxt=fulltxt.replaceAll("#%l#", " ");
        
         String paras[]=fulltxt.split("&*&");
         
         
         File file = new File(filename);
        	try {
        	    BufferedWriter out = new BufferedWriter(new FileWriter(file));
        	   String outstr="";
        	    int i=0;
        	    while (i<paras.length) {
           	  
        	    	   if (paras[i].length()>20){
        	    		   outstr=outstr+paras[i];
        	    	   }
        	    	   i++;
        	    }
        	    outstr=outstr.replaceAll("-#%p#", "");
        	    outstr=outstr.replaceAll("#%p#", " ");
        	    out.write(outstr+"\r\n");
        	    out.close();
        	     
        	} catch (IOException ex) {
        	    ex.printStackTrace();
        	}
         
         
         
         
         if (pd != null) {
             pd.close();
         }
 } catch (Exception e){
         e.printStackTrace();
        } 
	 
	 
	 
	 
 }
 
 /**
  * The main method.
  *
  * @param args the arguments
  */
 public static void main(String[] args){
//	 String fn1="/Users/kapila/Documents/repository/experiment3/test/swan_project.doc";
//	 String fn2="/Users/kapila/Documents/repository/experiment3/test/swan_project.txt";
//	 
//	 TextConvertor tc =new TextConvertor(fn1,fn2);
	 
	 

	 String txtDir="/Users/kapila/Documents/repository/experiment4/economics/txts/";
	 String pdfDir="/Users/kapila/Documents/repository/experiment4/economics/pdfs/";
	 String[] pdffiles=getFiles(pdfDir,pdffilter);
		for (int i=0; i< pdffiles.length;i++){
			 System.out.println("Converting file ..."+pdffiles[i]);
			 TextConvertor tc=new TextConvertor(pdfDir+pdffiles[i], txtDir+"files"+i+".txt");
					
		}
	 
	
	 
	 
	 

}
 
 /** The pdffilter. */
 public static FilenameFilter pdffilter = new FilenameFilter() {
	    public boolean accept(File dir, String name) {
	        return name.endsWith(".pdf");
	    }
	};
	
	/**
	 * Gets the files.
	 *
	 * @param dirname the dirname
	 * @param filter the filter
	 * @return the files
	 */
	public static String[] getFiles(String dirname,FilenameFilter filter ){
		
		File dir = new File(dirname);

		String[] files = dir.list(filter);
		return files;
	}	
}


