TARGETS := slides.html overflow.html

all: $(TARGETS)

%.pdf: %.md
	pandoc -s $< -t beamer -o $@

%.html: %.md
	pandoc -s $< -t slidy -o $@

clean:
	rm -f $(TARGETS)
