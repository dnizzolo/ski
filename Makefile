LISP = sbcl

ski:
	$(LISP) --eval '(asdf:load-system :ski)' \
		--eval '(in-package :ski)' \
		--eval "(sb-ext:save-lisp-and-die #p\"ski\" :toplevel #'driver-loop :executable t)"

.PHONY: clean
clean:
	rm -f ski

.PHONY: test
test:
	$(LISP) --non-interactive \
		--eval '(asdf:test-system :ski)'
