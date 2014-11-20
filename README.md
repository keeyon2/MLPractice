MLPractice
==========

Contains:

- Quicksort for List of ints. 
Named: intPartitionSort

2. Polymorphic Quicksort (give < operator for compares of argument type) (> operator will cause this to return Decending array). 
Named: partitionSort

3. Polymorphic Tree (leaves contain list, nodes contain list of multiple trees, so not just a binary tree with 2 leaves) 
Named: tree

4. Quicksort for Polymorphic Trees.  Builds on items 2 and 3, and returns a tree with all the leaves lists now sorted with given < operator.
Named: sortTree

5. polymorphic merge that takes 2 lists and will append both lists into one depending on your given < operator.  This is much like what is happening in the last step of a mergesort
Named: merge

6.  Polymorphic Tree Append that takes polyTree and appends all lists from the leaves and returns a single list with all lists appended together.
Named: appendTree

7.  Polymorphic Tree Merge takes a polyTree and sorts + merges all the the leaves lists to return one single sorted list.
Named: mergeTree

To test, start sml repl and run.

- use "MLFunctions.ml";
- use "TestData.ml";
- intPartitionSort TestListInt;
- partitionSort (op <) [1, 9,3, 6, 7];
- partitionSort (op <) TestListInt;
- partitionSort (op <) TestListReal;
- partitionSort (fun(a,b) => length a < length b) TestListListInt;
- sortTree (op <) sortTreeTestTree;
- merge (op <) MergeLessList1 MergeLessList2;
- merge (fn (a, b) => a > b) MergeGreaterList1 MergeGreaterList2;
- appendTree myTree;
- mergeTree (op <) myTree;
