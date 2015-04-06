package myButs;
import java.lang.Math;

	/** here we implement the edge class which shows 
	    an edges's  start,end and weight.
**/
public class Edge  {
	  private final String id; 
	  private final Vertex source;
	  private final Vertex destination;
	  private final double weight; 
	  
	  public Edge(String id, Vertex source, Vertex destination, double weight) {
	    this.id = id;
	    this.source = source;
	    this.destination = destination;
	    this.weight = weight;
	  }
	  
	  public String getId() {
	    return id;
	  }
	  public Vertex getDestination() {
	    return destination;
	  }

	  public Vertex getSource() {
	    return source;
	  }
	  public double getWeight() {
	    return weight;
	  }
	  
	  @Override
	  public String toString() {
	    return source + " " + destination;
	  }
	  
	  
	} 
