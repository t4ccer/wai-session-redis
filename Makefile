build:
	stack build
build-loop:
	ghcid
run:
	stack run
run-loop:
	ghcid --command="stack repl" --test "Main.main" -W
test:
	docker run --name redis-session-tests -p 6379:6379 -d redis
	stack test
	docker rm -f redis-session-tests
test-loop:
	ghcid -c="stack ghci test/Spec.hs" -T=main
	
