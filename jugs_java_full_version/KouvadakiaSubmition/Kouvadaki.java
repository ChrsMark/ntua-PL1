/* A class that represents a jug */

public class Kouvadaki {
	private  int capacity; 	 
	private  int filled; 
	private  String name;
	private int Id;
	  
	  public Kouvadaki(int cap, int fill,String Name,int id) {
	    this.capacity = cap;
	    this.filled = fill;
	    this.name = Name;
	    this.Id = id;   
	  }
	  
	  public String getName() {
	    return name;
	  }
	  public int getCapacity() {
	    return capacity;
	  }
	  public int getFilled() {
	    return filled;
	  }
	  public int getId() {
		    return Id;
	  }
	 
	  // returns the water that couldn't be loaded
	  public int loadVolume(int Volume) {
		int result =0;  
	    if(Volume+filled<=capacity){
	    	filled+=Volume;
	    	result = 0;
	    }
	    else {
	    	result = Volume-(capacity-filled);
	    	filled=capacity;
	    }
	    return result;
	  }

	  // makes the jug empty of the volume
	  public void unloadVolume(int Volume) {
			  
		    if(Volume<=filled){
		    	filled=filled-Volume;  	
		    }
		    else {
		    	filled=0;
		    }    
		  }	  
}
