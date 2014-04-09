TARGETS := slides.pdf slides.html

all: $(TARGETS)

%.pdf: %.md
	pandoc -s $< -t beamer -o $@

%.html: %.md
	pandoc -s $< -t dzslides -o $@

clean:
	rm -f $(TARGETS)
