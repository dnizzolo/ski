LISP = sbcl

ski: base.lisp combinators.lisp lambda.lisp main.lisp package.lisp syntax.lisp transformations.lisp ski.asd
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
