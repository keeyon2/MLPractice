val TestListInt = [5, 1, 6, 13, 5, 6, 7, 76, 4]
val TestListReal = [5.3, 2.6, 1.7, 3.2, 7.8, 10.2, 66.2] 
val TestListListInt = [[1, 9, 3, 6], [1], [2, 4, 6], [5,5]]

(* Given a partician int.  This returns a tuple with (ListLessThanPart,
 * ListGreatern than Part*)

fun ParticianInt x [] (ListLess : int list, ListMore : int list) = 
    (ListLess, ListMore)
 | ParticianInt x (origFirst::origRest) (ListLess : int list, ListMore : int list) = 
     if (x > origFirst) then (ParticianInt x origRest ([origFirst] @ ListLess,
     ListMore))
     else (ParticianInt x origRest (ListLess, ([origFirst] @ ListMore))) 

fun intPartitionSort (x::xs) = 
     let val TupTup = ParticianInt x xs ([],[])
         val (Tup1 : int list) = #1TupTup
         val (Tup2 : int list) = #2TupTup
         val (LowerList : int list) = intPartitionSort Tup1
         val (HigherList : int list)  = intPartitionSort Tup2
         in ((LowerList @ [x]) @ HigherList)
     end
 | intPartitionSort _ = []

(* Takes in operator to make the partician work will all types *)
fun ParticianPoly (op <) x [] (ListLess, ListMore) = 
    (ListLess, ListMore)
 | ParticianPoly (op <) x (origFirst::origRest) (ListLess, ListMore) = 
     if (x < origFirst) then (ParticianPoly (op <) x origRest (ListLess, ([origFirst] @ ListMore)))
     else (ParticianPoly (op <) x origRest ([origFirst] @ ListLess,
     ListMore))

fun partitionSort (op <) (x::xs) = 
     let val TupTup = ParticianPoly (op <) x xs ([],[])
         val Tup1 = #1TupTup
         val Tup2 = #2TupTup
         val LowerList = partitionSort (op <) Tup1
         val HigherList = partitionSort (op <) Tup2
         in ((LowerList @ [x]) @ HigherList)
     end
 | partitionSort (op <) _ = []

 datatype 'a tree = leaf of 'a | node of 'a tree list

val myTree = node [node [node [leaf [4,2,14],leaf [9,83,32],leaf [96,123,4]],
                              node [leaf [47,71,82]],node [leaf [19,27,10],
                                                           leaf [111,77,22,66]],
                              leaf [120,42,16]],
                        leaf [83,13]]
