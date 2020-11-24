#  CSCI 1103 Computer Science 1 Honors

## Fall 2020

Boston College

---

## Four Implementations of Dictionaries

This directory has 5 subdirectories

```bash
README.md	arrays/		bst/
alist/		balanced/	keys/
```

Four of the directories contain implementations of key/value dictionaries in which the keys are strings.

+ The `alist/` directory contains a simple association-list implementation of dictionaries.
+ The `arrays/` directory contains an implementation of dictionaries using parallel arrays. This representation may be useful only when the keys in the dictionary can be placed in sorted order.
+ The `balanced/` directory contains an implementation of dictionaries using the Standard Library's balanced tree implementation.
+ The `bst/` directory contains a simple binary search tree implementation.
+ The `keys/` directory contains support code for making files with large numbers of unique string keys. In the present setup, the keys are unique strings of nucleotides ranging in length from 8 to 15 nucleotides. 
  + The file `keys/random.txt` contains 100,000 unique nucleotide sequences in random order.
  + The file `keys/sorted.txt` contains the same 100,000 unique nucleotide sequences in sorted order.

The code here is intended to support comparison of the performance of `find` operations in different implementation of maps. 

```bash
> cd alist/src
> time dune exec ../../keys/random.txt 10000
```

Reports the number of seconds required to build a 100,000 key/value map using an association list implementation, with the keys inserted into the map in random order. It then uses the map's `find` operation to lookup the values of 10,000 randomly selected keys.

```bash
> cd alist/src
> time dune exec ../../keys/sorted.txt 10000
```

Like above but the keys are inserted into the map in alphabetical order. This example is useful to show problems with the simple bst implementation.