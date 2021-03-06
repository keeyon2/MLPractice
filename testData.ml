(*To run, make sure you use MLFunctions.ml before you use this file*)

(* To test intPartitionSort, 
 * 1. Run: intPartitionSort TestListInt;
 * *)
val TestListInt = [5, 1, 6, 13, 5, 6, 7, 76, 4]

(* To test polymorphic QuickSort, can run these all
 * 1. Run: partitionSort (op <) [1, 9,3, 6, 7];
 * 2. Run: partitionSort (op <) TestListInt;
 * 3. Run: partitionSort (op <) TestListReal;
 * 4. Run: partitionSort (fun(a,b) => length a < length b) TestListListInt;
 *)

val TestListReal = [5.3, 2.6, 1.7, 3.2, 7.8, 10.2, 66.2] 
val TestListListInt = [[1, 9, 3, 6], [1], [2, 4, 6], [5,5]]

(*This is a variable of tree type I created where leaves hold 'b list and nodes can have as many
 * leaves as they like*)
val myTree = node [node [node [leaf [4,2,14],leaf [9,83,32],leaf [96,123,4]],
                              node [leaf [47,71,82]],node [leaf [19,27,10],
                                                           leaf [111,77,22,66]],
                              leaf [120,42,16]],
                        leaf [83,13]]

(*To Test the sortTree, which Given A 'b list tree of from above, will go to all
 * the leaves and sort their list by running the partitionSort on them all,
 * 1. Run: sortTree (op <) sortTreeTestTree;
 * *)

val sortTreeTestTree = (node [leaf [4,2,3,1], leaf [7,2,5,0]])

(* To Test Merge, we can run either 
 * 1. Run: merge (op <) MergeLessList1 MergeLessList2;
 * 2. Run: merge (fn (a, b) => a > b) MergeGreaterList1 MergeGreaterList2;
 *)

val MergeLessList1 = [1, 4, 5, 7]
val MergeLessList2 = [2, 5, 7, 8]

val MergeGreaterList1 = [8, 6, 4, 2]
val MergeGreaterList2 = [7, 5, 3, 1]

(* To Test Append Tree,
 * 1. Run: appendTree myTree;
 * *)

(* To Test Merge Tree,
 * 1. Run: mergeTree (op <) myTree;
 * *)
