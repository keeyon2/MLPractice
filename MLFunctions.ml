Control.Print.printDepth := 100;
Control.Print.printLength := 100;

 (* This datatype is a tree of of any type that has leaves or nodes with as many
  * leaves as they would like *)
 datatype 'a tree = leaf of 'a | node of 'a tree list

(* Given a partician int.  This returns a tuple with (ListLessThanPart,
 * ListGreatern than Part*)

fun PartitianInt x [] (ListLess : int list, ListMore : int list) = 
    (ListLess, ListMore)
 | PartitianInt x (origFirst::origRest) (ListLess : int list, ListMore : int list) = 
     if (x > origFirst) then (PartitianInt x origRest ([origFirst] @ ListLess,
     ListMore))
     else (PartitianInt x origRest (ListLess, ([origFirst] @ ListMore))) 

fun intPartitionSort (x::xs) = 
     let val TupTup = PartitianInt x xs ([],[])
         val (Tup1 : int list) = #1TupTup
         val (Tup2 : int list) = #2TupTup
         val (LowerList : int list) = intPartitionSort Tup1
         val (HigherList : int list)  = intPartitionSort Tup2
         in ((LowerList @ [x]) @ HigherList)
     end
 | intPartitionSort _ = []

(* Takes in operator to make the partician work will all types *)
fun PartitianPoly (op <) x [] (ListLess, ListMore) = 
    (ListLess, ListMore)
 | PartitianPoly (op <) x (origFirst::origRest) (ListLess, ListMore) = 
     if (x < origFirst) then (PartitianPoly (op <) x origRest (ListLess, ([origFirst] @ ListMore)))
     else (PartitianPoly (op <) x origRest ([origFirst] @ ListLess,
     ListMore))

fun partitionSort (op <) (x::xs) = 
     let val TupTup = PartitianPoly (op <) x xs ([],[])
         val Tup1 = #1TupTup
         val Tup2 = #2TupTup
         val LowerList = partitionSort (op <) Tup1
         val HigherList = partitionSort (op <) Tup2
         in ((LowerList @ [x]) @ HigherList)
     end
 | partitionSort (op <) _ = []


(* Sort PolyTree.  Given a 'b list tree, we need go to all the leaves and sort
 * each leaf list *)
fun sortTree (op<) (leaf unsortedList) = leaf (partitionSort (op <)
unsortedList)
 | sortTree (op<) (node listOfTrees) = node (map (fn x => (sortTree (op<) x))
 listOfTrees)

fun functionalMerge (op <) [] [] L3 = L3
 | functionalMerge (op <) [] L2 L3 = L3 @ L2
 | functionalMerge (op <) L1 [] L3 = L3 @ L1
 | functionalMerge (op <) (x::xs) (y::ys) L3 = 
     if (x < y) then (functionalMerge (op <) xs (y::ys) (L3 @ [x]))
     else (functionalMerge (op <) (x::xs) ys (L3 @ [y])) 

fun merge _ [] L2 = L2
 | merge _ L1 [] = L1
 | merge (op <) L1 L2 = functionalMerge (op <) L1 L2 []

