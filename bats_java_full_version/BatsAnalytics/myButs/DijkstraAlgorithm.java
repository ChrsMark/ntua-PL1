package myButs;



import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;



public class DijkstraAlgorithm {

  private final ArrayList<Vertex> nodes;
  private final ArrayList<Edge> edges;
  private Set<Vertex> settledNodes;
  private Set<Vertex> unSettledNodes;
  private Map<Vertex, Vertex> predecessors;
  private Map<Vertex, Double> distance;

  public DijkstraAlgorithm(Graph graph) {
    // create a copy of the array so that we can operate on this array
    this.nodes = new ArrayList<Vertex>(graph.getVertexes());
    this.edges = new ArrayList<Edge>(graph.getEdges());
  }

  public void execute(Vertex source) {
    settledNodes = new HashSet<Vertex>();
    unSettledNodes = new HashSet<Vertex>();
    distance = new HashMap<Vertex, Double>();
    predecessors = new HashMap<Vertex, Vertex>();
    distance.put(source, 0.0);
    unSettledNodes.add(source);
    //System.out.println(nodes);
    //System.out.println(edges);
    //System.out.println(settledNodes);
    System.out.println(nodes);
    System.out.println("Edges are :"+edges.size() );
    while (unSettledNodes.size() > 0) {
    	//System.out.println(unSettledNodes);
        //System.out.println(settledNodes);
      Vertex node = getMinimum(unSettledNodes);
      
      settledNodes.add(node);
      unSettledNodes.remove(node);
      
      System.out.println(node+" was added");
      System.out.println(" unSettledNodes are "+ unSettledNodes);
        System.out.println(" SettledNodes are "+settledNodes);
        
      findMinimalDistances(node);
    }
    
  }

  private void findMinimalDistances(Vertex node) {
    ArrayList<Vertex> adjacentNodes = getNeighbors(node);  // here find the neighbors of my current node.
    for (Vertex target : adjacentNodes) {
      if (getShortestDistance(target) > getShortestDistance(node)
          + getDistance(node, target)) {
        distance.put(target, getShortestDistance(node)
            + getDistance(node, target));
        predecessors.put(target, node);
        unSettledNodes.add(target);
      }
    }

  }

  private double getDistance(Vertex node, Vertex target) {
    for (Edge edge : edges) {
      if (edge.getSource().equals(node)
          && edge.getDestination().equals(target)) {
    	  //System.out.println("mpika");
        return edge.getWeight();
      }
      if (edge.getDestination().equals(node)
              && edge.getSource().equals(target)) {
        	  //System.out.println("mpika");
            return edge.getWeight();
          }
    }
    throw new RuntimeException("Should not happen");
  }

  private ArrayList<Vertex> getNeighbors(Vertex node) {
    ArrayList<Vertex> neighbors = new ArrayList<Vertex>();
    for (Edge edge : edges) {
      if (  edge.getSource().equals(node)
          &&   !isSettled(edge.getDestination())  ) {
        neighbors.add(edge.getDestination());
      }
      else if (edge.getDestination().equals(node)
              && !isSettled(edge.getSource())) {
          neighbors.add(edge.getSource());
        }
    }
    System.out.println("Neigbors of node"+node+" are :"+neighbors);
    return neighbors;
  }

  private Vertex getMinimum(Set<Vertex> vertexes) {
    Vertex minimum = null;
    for (Vertex vertex : vertexes) {
      if (minimum == null) {
        minimum = vertex;
      } else {
        if (getShortestDistance(vertex) < getShortestDistance(minimum)) {
          minimum = vertex;
        }
      }
    }
    return minimum;
  }

  private boolean isSettled(Vertex vertex) {
    return settledNodes.contains(vertex);
  }

  public double getShortestDistance(Vertex destination) {
	//System.out.println(destination);  
    Double d = distance.get(destination);
    //System.out.println(d);
    if (d == null) {
      return Double.MAX_VALUE;
    } else {
      return d;
    }
  }

  /*
   * This method returns the path from the source to the selected target and
   * NULL if no path exists
   */
  public LinkedList<Vertex> getPath(Vertex target) {
    LinkedList<Vertex> path = new LinkedList<Vertex>();
    Vertex step = target;
    // check if a path exists
    if (predecessors.get(step) == null) {
      return null;
    }
    path.add(step);
    while (predecessors.get(step) != null) {
      step = predecessors.get(step);
      path.add(step);
    }
    // Put it into the correct order
    Collections.reverse(path);
    return path;
  }

}
