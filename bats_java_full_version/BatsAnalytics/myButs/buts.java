/**
 * 
 */
package myButs;



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

/**
 * @author Nick Papadis and chris
 * 
 */
public class buts {

	/**
	 * @param args
	 */
	static char[][] building;
	static int[][] helpBuilding;
	static double targetX;
	static double targetY;
	public static void main(String[] args) {
		try {

			FileInputStream fstream = new FileInputStream(
					"/home/chris/Desktop/in2.txt");
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));

			// here we do the reading of the input file!
			String line = br.readLine();
			String[] a = line.split(" ");
			int n = Integer.parseInt(a[0]);
			int m = Integer.parseInt(a[1]);
			int k = Integer.parseInt(a[2]);
			building = new char[n][m];
			helpBuilding = new int[n][m];
			int target=0;
			// print the dimensions
			System.out.print(n + " ");
			System.out.print(m + " ");
			System.out.print(k + " ");

			System.out.println();
			// create the whole table filling it with '~'
			for (int i = 0; i < n; i++)
				for (int j = 0; j < m; j++){
					building[i][j] = '~';
					helpBuilding[i][j]=-1;
				}

			ArrayList<interestingPoint> Animals = new ArrayList<interestingPoint>();
			ArrayList<interestingPoint> Walls = new ArrayList<interestingPoint>();

			for (int i = 0; i < k; i++) {
				line = br.readLine();
				String[] temp = line.split(" ");
				int tempI = Integer.parseInt(temp[0]);
				int tempJ = Integer.parseInt(temp[1]);
				char Inside = temp[2].charAt(0);
				

				// store in two lists the animals and the walls!!!
				if (Inside == 'Β') {
					interestingPoint e = new interestingPoint(tempI + 0.5,
							tempJ + 0.5, 'B');
					building[tempI][tempJ] = 'Β';
					Animals.add(e);
				} else if (Inside == 'Α') {
					interestingPoint e = new interestingPoint(tempI + 0.5,
							tempJ + 0.5, 'A');
					Animals.add(e);
					targetX = tempI + 0.5;
					targetY = tempJ + 0.5;
					building[tempI][tempJ] = 'A';
				} else {
					interestingPoint e = new interestingPoint(tempI + 0.5,
							tempJ + 0.5, 'w');
					Walls.add(e);
					building[tempI][tempJ] = 'w';
				}
			}
			// never forget to close the stream!
			in.close();

			// now that we have finished with the reading we are ready to print
			// it!

			for (int i = 0; i < n; i++) {
				for (int j = 0; j < m; j++)
					System.out.print(building[i][j]);
				System.out.println();
			}

			System.out.println("Print the animals:");

			for (int i = 0; i < Animals.size(); i++)
				System.out.println(Animals.get(i).getX() + " "
						+ Animals.get(i).getY() + " "
						+ Animals.get(i).getKind() + " ");

			System.out.println("Print the walls:");

			for (int i = 0; i < Walls.size(); i++)
				System.out.println(Walls.get(i).getX() + " "
						+ Walls.get(i).getY() + " " + Walls.get(i).getKind()
						+ " ");

			// now we are ready to create the lines of sight!!!
			ArrayList<ArrayList> lines_of_sight = new ArrayList<ArrayList>();
			
			for (int i = 0; i < Animals.size(); i++) {
				for(int j=i+1; j < Animals.size(); j++){
				 double StartX = Animals.get(i).getX();
				 double StartY = Animals.get(i).getY();
				 double EndX = Animals.get(j).getX();
				 double EndY = Animals.get(j).getY();
				ArrayList<simplePoint> e = new ArrayList<simplePoint>();
				e = Bresenham_supercover(StartX, StartY,EndX, EndY);
				if (!e.isEmpty()) lines_of_sight.add(e);
				}
			}
			
			System.out.println("Print the lines");
			for (int i = 0; i < lines_of_sight.size(); i++){
				ArrayList<simplePoint> tempList = new ArrayList<simplePoint>();
				tempList = lines_of_sight.get(i);
				for (int j = 0; j < tempList.size(); j++)
					System.out.print(tempList.get(j).getX() + " "
							+ tempList.get(j).getY() + " " 
							+ " ");
				System.out.println();
			}
			// now you need the clear lines!!!
			ArrayList<ArrayList> clear_lines = new ArrayList<ArrayList>();
			clear_lines = lines_of_sight;
			
			
			System.out.println("Print the clear lines");
			for (int i = 0; i < clear_lines.size(); i++){
				ArrayList<simplePoint> tempList = new ArrayList<simplePoint>();
				tempList = clear_lines.get(i);
				for (int j = 0; j < tempList.size(); j++)
					System.out.print(tempList.get(j).getX() + " "
							+ tempList.get(j).getY() + " " 
							+ " ");
				System.out.println();
			}
			// now that you have the clear lines you need to create the edges of the graph.
			ArrayList<Edge> edges = new ArrayList<Edge>();
			ArrayList<Vertex> nodes = new ArrayList<Vertex>();
			int counterNodes=0;
			int [] times = new int [clear_lines.size()];
			for (int i = 0; i < clear_lines.size(); i++) times[i]=0;
			
			
			
			for (int i = 0; i < clear_lines.size(); i++){
				ArrayList<simplePoint> tempList = new ArrayList<simplePoint>();
				tempList = clear_lines.get(i);
				
				
				int size = tempList.size();
				
				
				if (size!=1){
					double x1 = tempList.get(0).getX(); 
					double y1 = tempList.get(0).getY(); 
					double x2 = tempList.get(size-1).getX(); 
					double y2 = tempList.get(size-1).getY();
					int tempX=(int) (x1-0.5);
					int tempY=(int) (y1-0.5);
					int tempX2=(int) (x2-0.5);
				    int tempY2=(int) (y2-0.5);
					
					
					int temp=0;
					
					if(helpBuilding[tempX][tempY]!=-1){ 
						temp=helpBuilding[tempX][tempY];
						}
					else temp=counterNodes;
					
					
					Vertex mySource= new Vertex("VertexNo"+temp,"VertexNo"+temp,x1,y1);

					if (building[tempX][tempY]!='d'){
						building[tempX][tempY]='d';
						int index = helpBuilding[tempX][tempY];
						if (index==-1){
							helpBuilding[tempX][tempY]=counterNodes;}
					
						
						nodes.add(mySource);
						counterNodes++;
						}
					
					
					
					int temp2=0;
					if(helpBuilding[tempX2][tempY2]!=-1) { 
						temp2=helpBuilding[tempX2][tempY2];
					}
					else temp2=counterNodes;
					
					Vertex myDest= new Vertex("VertexNo"+temp2,"VertexNo"+temp2,x2,y2);
					
					
				    if (building[tempX2][tempY2]!='d'){
				    	building[tempX2][tempY2]='d';
						int index = helpBuilding[tempX2][tempY2];
						if (index==-1){helpBuilding[tempX2][tempY2]=counterNodes;}
					
						
						
						nodes.add(myDest);
						counterNodes++;
					}
					//System.out.print(x1 +" "+y1+" "+x2 +" "+y2+" ");
					double dist = Math.sqrt( Math.pow( (x1-x2), 2)+Math.pow(  (y1-y2), 2) );
					Edge tempEdge = new Edge("edgeNo"+i,mySource, myDest,dist);
					
					edges.add(tempEdge);
					System.out.println("The table!!! for "+i);
					for (int b = 0; b < n; b++){
						for (int j = 0; j < m; j++){
							System.out.print(" ");
							System.out.print(helpBuilding[b][j]);
						
						}System.out.println();}
				}
			}
			
			int myX = (int)(targetX-0.5);
			int myY = (int)(targetY-0.5);
			
			target = helpBuilding[myX][myY];  // ti seira mpikes re NEO!!!!!
			System.out.println("Print edges");
			for (int i = 0; i < edges.size(); i++){
					System.out.println("("+edges.get(i).getSource().getX()+","+edges.get(i).getSource().getY() + ") - ("
							+ edges.get(i).getDestination().getX()+","+edges.get(i).getDestination().getY() + ") "+"Distnase="+ Math.round((edges.get(i).getWeight() *100.0))/100.0
							+ " ");
				
			}
			System.out.println("Print edges again!!!");
			for (int i = 0; i < edges.size(); i++){
					System.out.println("("+edges.get(i).getSource()+" "
							+ edges.get(i).getDestination()+" Distnase="+ Math.round((edges.get(i).getWeight() *100.0))/100.0
							+ " ");
				
			}
			
			//now create the appropriate graph!!!
			
			System.out.println("Print nodes");
			for (int i = 0; i < nodes.size(); i++){
				if(   (nodes.get(i).getX()==targetX) && (nodes.get(i).getY()==targetY) ){  
					//target = i;
				}
					System.out.println("Node with Name  "+nodes.get(i).getName()+" and Id  "+nodes.get(i).getId()+" = "+nodes.get(i).getX()+" "+nodes.get(i).getY() );
				
			}
			
			//check all about targets and indexes and all of that!!!!!!!
			System.out.println(edges);
			System.out.println(counterNodes);
			//target = 8;
			System.out.println("My target is node"+(target));
			Graph graph = new Graph(nodes, edges);
		    DijkstraAlgorithm dijkstra = new DijkstraAlgorithm(graph);
		    System.out.println("dijkstra execution...");
		    dijkstra.execute(nodes.get(0));
		    System.out.println("find  ShortestDistance...");
		    double tempResult = dijkstra.getShortestDistance(nodes.get(target));
		    LinkedList<Vertex> path = dijkstra.getPath(nodes.get(target));
		    
			System.out.println("My target is at point("+targetX+","+targetY+")");
			System.out.println("My path is "+path);
			//implement Dijkstra on your graph to find the path!
			double finalResult = Math.round( tempResult*100.0)/100.0;
			System.out.println("My distance is "+finalResult);
			
			/* insert the main code here !!!! */

		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	/*
	 * algorthim from:
	 * http://lifc.univ-fcomte.fr/home/~ededu/projects/bresenham/
	 */
	public static ArrayList Bresenham_supercover(double x1, double y1,
			double x2, double y2) {
		ArrayList<simplePoint> points = new ArrayList<simplePoint>();
		ArrayList<simplePoint> empty = new ArrayList<simplePoint>();
		// ready to implement it from your .txt file!!!

		double i; // loop counter
		double ystep, xstep; // the step on y and x axis
		double error; // the error accumulated during the increment
		double errorprev; // *vision the previous value of the error variable
		double y = y1, x = x1; // the line points
		double ddy, ddx; // compulsory variables: the double values of dy and dx
		double dx = x2 - x1;
		double dy = y2 - y1;
		simplePoint temPoint = new simplePoint(x1, y1); // first point
		points.add(temPoint);
		// here check if the point is a wall!!!
		int myX = (int)(x1-0.5);
		int myY = (int)(y1-0.5);
		
		if(building[myX][myY]=='w'){
			return empty;
		}
		// NB the last point can't be here, because of its previous point (which
		// has to be verified)
		if (dy < 0) {
			ystep = -1;
			dy = -dy;
		} else
			ystep = 1;
		if (dx < 0) {
			xstep = -1;
			dx = -dx;
		} else
			xstep = 1;
		ddy = 2 * dy; // work with double values for full precision
		ddx = 2 * dx;
		if (ddx >= ddy) { // first octant (0 <= slope <= 1)
			// compulsory initialization (even for errorprev, needed when
			// dx==dy)
			errorprev = error = dx; // start in the middle of the square
			for (i = 0; i < dx; i++) { // do not use the first point (already
										// done)
				x += xstep;
				error += ddy;
				if (error > ddx) { // increment y if AFTER the middle ( > )
					y += ystep;
					error -= ddx;
					
					// three cases (octant == right->right-top for directions
					// below):
					if (error + errorprev < ddx) { // bottom square also
						simplePoint temPoint2 = new simplePoint( x , y-ystep); 											
						points.add(temPoint2);
						 myX = (int)(x-0.5);
						 myY = (int)(y-ystep-0.5);
						
						if(building[myX][myY]=='w'){
							return empty;
						}

					} else if (error + errorprev > ddx) { // left square also
						simplePoint temPoint2 = new simplePoint(x - xstep, y); 												
						points.add(temPoint2);
						myX = (int)(x- xstep-0.5);
						myY = (int)(y-0.5);
						
						if(building[myX][myY]=='w'){
							return empty;
						}
					} else { // corner: bottom and left squares also
						simplePoint temPoint2 = new simplePoint( x , y-ystep); 												
						points.add(temPoint2);
						myX = (int)(x-0.5);
						myY = (int)(y-ystep-0.5);
						
						if(building[myX][myY]=='w'){
							return empty;
						}
						simplePoint temPoint3 = new simplePoint(x - xstep, y); 															
						points.add(temPoint3);
						myX = (int)(x-xstep-0.5);
						myY = (int)(y-0.5);
						
						if(building[myX][myY]=='w'){
							return empty;
						}
					}
				}
				simplePoint temPoint4 = new simplePoint(x, y); 
				points.add(temPoint4);
				myX = (int)(x-0.5);
				myY = (int)(y-0.5);
				
				if(building[myX][myY]=='w'){
					return empty;
				}
				errorprev = error;
			}
		} else { // the same as above
			errorprev = error = dy;
			for (i = 0; i < dy; i++) {
				y += ystep;
				error += ddx;
				if (error > ddy) {
					x += xstep;
					error -= ddy;
					
					if (error + errorprev < ddy) {
						simplePoint temPoint2 = new simplePoint(x - xstep, y); 													
						points.add(temPoint2);
						myX = (int)(x- xstep-0.5);
						myY = (int)(y-0.5);
						
						if(building[myX][myY]=='w'){
							return empty;
						}
					} else if (error + errorprev > ddy) {
						simplePoint temPoint2 = new simplePoint(x,y - ystep); 													
						points.add(temPoint2);
						myX = (int)(x-0.5);
						myY = (int)(y-ystep-0.5);
						
						if(building[myX][myY]=='w'){
							return empty;
						}
					} else {
						simplePoint temPoint2 = new simplePoint(x - xstep,y);
						points.add(temPoint2);
						myX = (int)(x-xstep-0.5);
						myY = (int)(y-0.5);
						
						if(building[myX][myY]=='w'){
							return empty;
						}
						simplePoint temPoint3 = new simplePoint(x, y - ystep); 																			
						points.add(temPoint3);
						myX = (int)(x-0.5);
						myY = (int)(y-ystep-0.5);
						
						if(building[myX][myY]=='w'){
							return empty;
						}
					}
				}
				simplePoint temPoint4 = new simplePoint(x, y); // first point
				points.add(temPoint4);
				myX = (int)(x-0.5);
				myY = (int)(y-0.5);
				
				if(building[myX][myY]=='w'){
					return empty;
				}
				errorprev = error;
			}

		}
		
		
		
		return points;
	}
	
	
	
}
