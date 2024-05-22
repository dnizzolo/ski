LISP = sbcl

ski:
	mkdir -p bin/
	$(LISP) --eval '(asdf:load-system :ski)' \
		--eval '(in-package :ski)' \
		--eval "(sb-ext:save-lisp-and-die #p\"bin/ski\" :toplevel #'driver-loop :executable t)"

.PHONY: clean
clean:
	rm -rf bin/
