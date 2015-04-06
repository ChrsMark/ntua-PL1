(******************************************************************************************************
  Course    : Programming Languages 1 - Assignment 2 - Exercise 1
  Authors   : Nikolaos Papadis (nikpapadis@gmail.com), Christos Markou (chrs.markx86@gmail.com)
  Date      : August 27, 2014
  Note      : We solve the problem in three steps:
              1) After we have parsed the input, we create the map.
              2) We create all valid lines (as edges of a graph, whose verteces are the animals), 
              using the Bresenham supercover algorithm.
              3) We find the shortest path using Dijkstra's Shortest Path Algorithm. 
                 In the end, we print the shortest path's length with precision of two decimal points.
  -----------
  School of ECE, National Technical University of Athens.
******************************************************************************************************)

open Printf
open Str

(* Initialisations *)

let n = ref 0
let m = ref 0
let k = ref 0
let numOfAnimals = ref 0
let target = ref 0
let myMap = ref (Array.make_matrix 0 0 "~");;


(* Implementation of Dijkstra's Algorithm in OCaml taken from http://rosettacode.org/wiki/Dijkstra%27s_algorithm#OCaml *)

type vertex = int
type weight = float
type neighbor = vertex * weight
module VertexSet = Set.Make(struct type t = weight * vertex let compare = compare end)
 
let dijkstra (src:vertex) (adj_list:neighbor list array) : weight array * vertex array =
  let n = Array.length adj_list in
  let min_distance = Array.make n infinity in
  min_distance.(src) <- 0.;
  let previous = Array.make n (-1) in
  let rec aux vertex_queue =
    if not (VertexSet.is_empty vertex_queue) then
      let dist, u = VertexSet.min_elt vertex_queue in
      let vertex_queue' = VertexSet.remove (dist, u) vertex_queue in
      let edges = adj_list.(u) in
      let f vertex_queue (v, weight) =
        let dist_thru_u = dist +. weight in
        if dist_thru_u >= min_distance.(v) then
          vertex_queue
        else begin
          let vertex_queue' = VertexSet.remove (min_distance.(v), v) vertex_queue in
          min_distance.(v) <- dist_thru_u;
          previous.(v) <- u;
          VertexSet.add (min_distance.(v), v) vertex_queue'
        end
      in
      aux (List.fold_left f vertex_queue' edges)
  in
  aux (VertexSet.singleton (min_distance.(src), src));
  min_distance, previous
 
let shortest_path_to (target : vertex) (previous : vertex array) : vertex list =
  let rec aux target acc =
    if target = -1 then
      acc
    else
      aux previous.(target) (target :: acc)
  in
  aux target []

(* End of Dijkstra *)


(* File parsing function *)

let parseFile =
    let filename = Sys.argv.(1) in
    let example myList = Str.split (regexp "[ \n]+") myList in
    let first ((a:string)::(b:string)::(c:string)::[]) = a in
    let second ((a:string)::(b:string)::(c:string)::[]) = b in
    let third ((a:string)::(b:string)::(c:string)::[]) = c in
    let lines = ref [] in   
    let chan = open_in filename in
    let myLine = input_line chan in
    let myLineSplitted = example myLine in
      n := int_of_string(first(myLineSplitted));
      m := int_of_string(second(myLineSplitted));
      k := int_of_string(third(myLineSplitted));
      
    let tempArray = (Array.make_matrix !n !m "~") in 
        myMap := Array.append !myMap tempArray;
        let id = ref 0 in    
    try
        while true; do
            let myLine = input_line chan in 
            let [i;j;ch] = (example myLine) in
            let x = int_of_string(i) and y = int_of_string(j) in
            (!myMap).(x).(y) <- ch;
            
            if ch <> "-" then 
                lines := (!id,x,y,ch) :: (!lines);
            if ch = "A" then target:= !id;
            if ch <> "-" then id := (!id) +1;
        done; []
    with End_of_file ->
      close_in chan;
      numOfAnimals := List.length(!lines);
      numOfAnimals := (!id);
      (List.rev !lines);;


(* Bresenham supercover algorithm taken from http://lifc.univ-fcomte.fr/home/~ededu/projects/bresenham/ and implemented in OCaml by us *) 

let bresenham ((x1:int), (y1:int), (x2:int), (y2:int)) : bool = 
    let dx = ref 0 in
        let dy = ref 0 in
        let ystep = ref 0 in
        let xstep = ref 0 in
        let ddy = ref 0 in
        let ddx = ref 0 in
        let error = ref 0 in
        let result = ref true in
        let x = ref 0 in
        let y = ref 0 in

        let errorprev = ref 0 in
        
        x:=x1;
        y:=y1;
        dx := x2-x1;
        dy := y2-y1;
        
        if !dy<0 then 
        begin
            ystep := -1; 
            dy := -(!dy);
        end
        else begin 
            ystep:=1;
        end;

        if (!dx<0) then 
        begin
            xstep := -1; 
            dx := -(!dx);
        end
        else begin 
            xstep:=1;
        end;
        ddy := 2* (!dy);
        ddx := 2* (!dx);  
        if (!ddx) >= (!ddy) then
        begin 
            errorprev := !dx;
            error := !dx;
            let i = ref 0 in
            while ((!i) < (!dx)) && ((!result)=true) do
                x:= (!x)+(!xstep);
                error:= (!error) + (!ddy);
                if ((!error)>(!ddx)) then
                    begin
                        y:= (!y)+(!ystep);
                        error:= (!error) - (!ddx);

                        if(((!error)+(!errorprev)) < (!ddx)) then
                        begin
                            let tempx = (!x) in
                            let tempy = ((!y)-(!ystep)) in
                            if ((!myMap).(tempx).(tempy) = "-") then
                            begin
                                if (!result) then result := false;
                            end;
                        end

                        else if(((!error)+(!errorprev)) > (!ddx)) then
                        begin
                            let tempx = ((!x) - (!xstep))in
                            let tempy = (!y) in
                            if ((!myMap).(tempx).(tempy) = "-") then
                            begin
                                if (!result) then result := false;
                            end;
                        end

                        else 
                        begin
                            let tempx = ((!x))in
                            let tempy = ((!y)-(!ystep)) in
                            if ((!myMap).(tempx).(tempy) = "-") then
                            begin
                                if (!result) then result := false;
                            end;
                            let tempx = ((!x) - (!xstep)) in
                            let tempy = (!y) in
                            if ((!myMap).(tempx).(tempy) = "-") then
                            begin
                                if (!result) then result := false;
                            end;

                        end;
                    end;
                let tempx = (!x) in
                let tempy = (!y) in
                if ((!myMap).(tempx).(tempy) = "-") then
                begin
                    if (!result) then result := false;
                end;
                if (!result) then errorprev := (!error); 
                i := (!i) + 1;   
            done;
        end
        else
        begin
        
            if (!result) then 
            begin
                errorprev := !dy;
                error := !dy;
                let i = ref 0 in
                while ((!i) < (!dy)) && ((!result)=true) do
                    y:= (!y) + (!ystep);
                    error := (!error) + (!ddx);
                    if ((!error) > (!ddy)) then
                        begin
                            x := (!x) + (!xstep);
                            error := (!error) - (!ddy);

                            if(((!error)+(!errorprev)) < (!ddy)) then
                            begin
                                let tempx = ((!x) - (!xstep)) in
                                let tempy = (!y) in

                                if ((!myMap).(tempx).(tempy) = "-") then
                                begin
                                    if (!result) then result := false;
                                end;
                            end

                            else if(((!error)+(!errorprev)) > (!ddy)) then
                            begin
                                let tempx = (!x) in
                                let tempy = ((!y)-(!ystep)) in
                                if ((!myMap).(tempx).(tempy) = "-") then
                                begin
                                    if (!result) then result := false;
                                end;
                            end

                            else 
                            begin
                                
                                let tempx = ((!x)- (!xstep)) in
                                let tempy = (!y) in
                                if ((!myMap).(tempx).(tempy) = "-") then
                                begin
                                    if (!result) then result := false;
                                end;
                                let tempx = ((!x)) in
                                let tempy = ((!y)-(!ystep)) in
                                if ((!myMap).(tempx).(tempy) = "-") then
                                begin
                                    if (!result) then result := false;
                                end;

                            end;
                        end;
                    let tempx = (!x) in
                    let tempy = (!y) in
                    if ((!myMap).(tempx).(tempy) = "-") then
                    begin
                        if (!result) then result := false;
                    end;
                    if (!result) then errorprev := (!error); 
                    i := (!i) + 1;   
                done;
            end;
        end;
        (!result);;


(* Function that calculates the distance between two points of the plane *)

let calcDistance ((x1,y1):(int*int)) ((x2,y2):(int*int)) = 
    let dx = abs(x1-x2) in
    let dy = abs(y1-y2) in
      (sqrt(float_of_int(dx*dx + dy*dy)) : weight);;


(* Definition of the graph that has the animals as verteces and the valid lines as edges*)

let myG = ref (Array.make !numOfAnimals ([]:neighbor list));;


(* Recusive function that creates all edges of the graph *)

let rec create_all_edges = function
    [] -> ();
  | ((id1,i1,j1,ch1)::ans) ->
        let tempAdj = (ref [] : neighbor list ref) in
        let rec find_my_adjacents = function
            [] -> ();
          | (id2,i2,j2,ch2)::ans ->
                let iAmAdj = bresenham (i1,j1,i2,j2) in
                
                let dist = calcDistance (i1,j1) (i2,j2) in 
                tempAdj := (!myG).(id1);  
                if iAmAdj then tempAdj := (id2,dist)::(!tempAdj);
                if iAmAdj then (!myG).(id1) <- (!tempAdj);
                tempAdj := (!myG).(id2);
                if iAmAdj then tempAdj := (id1,dist)::(!tempAdj);
                if iAmAdj then (!myG).(id2) <- (!tempAdj);           
                    find_my_adjacents ans;  
        in
            find_my_adjacents ans;
            create_all_edges ans;;


(* Main body of the program *)

let () =
    let animals = parseFile in 
    let _ = create_all_edges animals in
    let adj_list =(!myG) in 
    let toGo = !target in   
    let min_distance, previous = dijkstra 0 (adj_list) in
    printf "%0.*f\n" 2 min_distance.(toGo);