LISP = sbcl

bin/ski:
	mkdir -p bin/
	$(LISP) --eval '(asdf:load-system :ski)' \
		--eval '(in-package :ski)' \
		--eval "(sb-ext:save-lisp-and-die #p\"bin/ski\" :toplevel #'driver-loop :executable t)"

.PHONY: run
run: bin/ski
	@bin/ski

.PHONY: clean
clean:
	rm -rf bin/

.PHONY: test
test:
	$(LISP) --non-interactive \
		--eval '(asdf:test-system :ski)'
