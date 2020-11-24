#  CSCI 1103 Computer Science 1 Honors



Robert Muller - Boston College

---

## Map Examples: Generating Keys

The `keygen.ml` program generates random strings of nucleotides ranging in length from 8 to 15 nucleotides. The generator program is run as in

```bash
> cd keygen/src
> dune exec bin/main.exe numberOfSequences outputFileName
```

The sequences can be used as keys in a demonstration of maps.

NB: there are two pre-made files with 100,000 unique sequences/keys: `random.txt` and `sorted.txt`. These files are useful inputs for the implementations of maps found in the sibling directories `../alist/`,  `../arrays/`,`../bst/` and `../balanced/`.

