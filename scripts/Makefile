
dict_file=dict
image_file=tileset.png

all: generate concat

generate:
	./gen_image.py gen > $(dict_file)

clean:
	./gen_image.py clean
	@rm -f $(image_file)
	@rm -f $(dict_file)

concat:
	convert +append part-*.png $(image_file)

.PHONY: all generate clean concat
