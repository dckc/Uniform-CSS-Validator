# This is just to make README.html from README .
# Most of the project uses sbt rather than make.

# http://docutils.sourceforge.net/rst.html
RST2HTML=rst2html

README.html: README
	$(RST2HTML) README >$@