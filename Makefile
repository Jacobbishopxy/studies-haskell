
tree:
	tree --dirsfirst --noreport -I "out|log|logs|images|*.md|*.sty|*.log|commands.tex|LICENSE|Makefile" | sed 's/^//' > tree.md

clean-out:
	rm -rf Ch01*/out && \
	rm -rf Ch02*/out && \
	rm -rf Ch03*/out && \
	rm -rf Ch04*/out && \
	rm -rf Ch05*/out && \
	rm -rf Ch06*/out && \
	rm -rf Ch07*/out && \
	rm -rf Ch08*/out && \
	rm -rf Ch09*/out && \
	rm -rf Ch10*/out && \
	rm -rf Ch11*/out && \
	rm -rf Ch12*/out && \
	rm -rf Ch13*/out && \
	rm -rf Ch14*/out && \
	echo done

cp-pdf:
	cp learn_me_a_haskell/out/main.pdf ./Learn\ Me\ a\ Haskell.pdf

