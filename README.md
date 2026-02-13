# xtile
xtile is an A* Search implementation with a focus on solving sliding tile puzzles. The project includes a simple command line interface for crafting searches.
```
xtile - sliding tile puzzle solver
Usage:
	xtile		create random puzzle with 100 shuffles and solve
	xtile n		create random puzzle with n shuffles and solve
	xtile n0 n1 n2 n3 n4 n5 n6 n7 n8
			creates a 3x3 puzzle with n0-n8 as tiles (0 is blank tile)
	xtile trials n
		    runs n trials for each algorithm and write csv list to trials.txt
```

## How to run
[stack](https://docs.haskellstack.org/en/stable/) is required to build and test xtile. It will install GHC and all necessary dependencies.
1. Clone code with ```git clone https://github.com/mgurga/xtile/``` and enter directory ```cd xtile```
2. Build the project with ```stack build```
3. Run the project with ```stack run```, this will solve a random puzzle and print the results
4. Run ```stack run help``` to see usage
5. Run tests with ```stack test```
