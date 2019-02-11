tiger\_parser

`ls testcases/*.tig | while read -r f; do echo $f; ./_build/default/main.exe $f; done > testcases/results`

testcases are from <https://www.cs.princeton.edu/~appel/modern/testcases/>.
