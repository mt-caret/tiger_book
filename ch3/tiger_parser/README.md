tiger\_parser

`ls testcases/*.tig | while read -r f; do echo $f; ./_build/default/tiger_parser.exe $f; done > testcases/result`

testcases are from <https://www.cs.princeton.edu/~appel/modern/testcases/>.
