/***************************************************************************
  Course    : Programming Languages 1 - Assignment 2 - Exercise 2
  Authors	: Nikolaos Papadis (nikpapadis@gmail.com), Christos Markou (chrs.markx86@gmail.com)
  Date      : August 30, 2014
  Note      : We calculate the two possible sequences of moves (01-12-20-12 repeatedly or 02-21-10-21 repeatedly)
  			  in parallel. When one of the two sequences accomplishies Vg litres in one of the jugs, the program 
  			  stops calculating and prints that sequence. We represent each jug with a "kouvadaki" object.
  -----------
  School of ECE, National Technical University of Athens.
****************************************************************************/


import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.lang.*;
import java.math.BigInteger;

/**
 * @author Nick Papadis and Chris Markou
 * 
 */
public class Kouvadakia {

	/**
	 * @param args
	 */

	public static void main(String[] args) {
		try {

			int V1 = Integer.parseInt(args[0]);
			int V2 = Integer.parseInt(args[1]);
			int Vg = Integer.parseInt(args[2]);
			ArrayList <String> resultSequence1 = new <String> ArrayList();
			ArrayList <String> resultSequence2 = new <String> ArrayList();
			int myGCD = BigInteger.valueOf(V1).gcd(BigInteger.valueOf(V2)).intValue();
						
			int toPrint=0;
						
			Kouvadaki kV1 = new Kouvadaki(V1,0,"kouvas1",1);
			Kouvadaki kV2 = new Kouvadaki(V2,0,"kouvas2",2);
			Kouvadaki kV3 = new Kouvadaki(V1,0,"kouvas3",3);
			Kouvadaki kV4 = new Kouvadaki(V2,0,"kouvas4",4);
						
			//initialize the jugs
			int tempResult = kV1.loadVolume(V1);
			int tempResult2 = kV4.loadVolume(V2);
			
			resultSequence1.add("01");
			resultSequence2.add("02");
					
			//seperate the situations
			if( (Vg>V1) && (Vg>V2) ) System.out.println("impossible");
			else if ((Vg % myGCD )!= 0) System.out.println("impossible");
			else { //do the stuff here!
				
				while(true){

				    // the route 0->1->2->0
					if(kV1.getFilled()==Vg){
						toPrint=1;
						break;
					}

					else if(kV1.getFilled()==0){
					    kV1.loadVolume(V1);
					    resultSequence1.add("01");						
					}

					else if(kV2.getFilled()==V2){
						kV2.unloadVolume(V2);
						resultSequence1.add("20");
					}

					else {
						tempResult = kV2.loadVolume(kV1.getFilled());
						kV1.unloadVolume(V1);
						kV1.loadVolume(tempResult);
						resultSequence1.add("12");
					}
							
					 // the route 0->4->3->0
					if(kV4.getFilled()==Vg){
						toPrint=2;
						break;
					}

					else if(kV4.getFilled()==0){
					    kV4.loadVolume(V2);
					    resultSequence2.add("02");
					}

					else if(kV3.getFilled()==V1){
						kV3.unloadVolume(V1);
						resultSequence2.add("10");
					}

					else{
						tempResult = kV3.loadVolume(kV4.getFilled());
						kV4.unloadVolume(kV4.getFilled());
						kV4.loadVolume(tempResult);
						resultSequence2.add("21");
					}
									
				} // end of while
				
				if((toPrint==1) && (resultSequence1.size()<resultSequence2.size())){
					
					for (int i = 0; i < (resultSequence1.size()-1); i++)
						          System.out.print(resultSequence1.get(i)+"-");
					System.out.print(resultSequence1.get(resultSequence1.size()-1));
				}
				else{
					
					for (int i = 0; i < (resultSequence2.size()-1); i++)
				          System.out.print(resultSequence2.get(i)+"-");
					System.out.print(resultSequence2.get(resultSequence2.size()-1));
				}
			}

		} 
		catch (Exception e) {
			e.printStackTrace();
		}
	}
}