build:
	stack build
build-loop:
	ghcid
run:
	stack run
run-loop:
	ghcid --command="stack repl" --test "Main.main" -W
test:
	stack test
test-loop:
	ghcid -c="stack ghci test/Spec.hs" -T=main
	
