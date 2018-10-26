tiger\_lexer

`ls testcases/*.tig | while read -r f; do echo $f; ./_build/default/tiger_lexer.exe $f; done > testcases/result`

testcases are from <https://www.cs.princeton.edu/~appel/modern/testcases/>.
