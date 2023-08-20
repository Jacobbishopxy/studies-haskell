
tree:
	tree --dirsfirst --noreport -I "out|log|logs|images|*.md|*.sty|*.log|commands.tex|LICENSE|Makefile" | sed 's/^//' > tree.md

clean-out:
	rm -rf learn_me_a_haskell/out && \
	rm -rf learn_real_world_haskell/out && \
	echo done

cp-pdf1:
	cp learn_me_a_haskell/out/main.pdf "./Learn Me a Haskell.pdf"

cp-pdf2:
	cp learn_real_world_haskell/out/main.pdf "./Learn Real World Haskell.pdf"

commit:
	git add .
	git commit -m "$m"
	git push -u origin main
