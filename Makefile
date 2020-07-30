all: slides purl copy

slides: gam-intro.Rmd slides.css
	Rscript -e 'library("rmarkdown"); render("gam-intro.Rmd")'

purl: gam-intro.Rmd
	Rscript -e "knitr::purl(\"gam-intro.Rmd\")"

copy: gam-intro.html slides.css macros.js
	cp -R -u gam-intro_files gam-intro.html macros.js slides.css libs resources ~/work/web/jekyll/blog/slides/gam-intro-webinar-2020/
