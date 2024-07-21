LISP = sbcl

ski: main.lisp transformations.lisp ski.lisp lambda.lisp base.lisp syntax.lisp package.lisp
	$(LISP) --eval '(asdf:load-system :ski)' \
		--eval '(in-package :ski)' \
		--eval "(sb-ext:save-lisp-and-die #p\"ski\" :toplevel #'toplevel :executable t)"

.PHONY: clean
clean:
	rm -f ski

.PHONY: test
test:
	$(LISP) --non-interactive \
		--eval '(asdf:test-system :ski)'
