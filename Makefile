TARGET=flp-fun
GHC_FLAGS=-Wall
FILES=$(wildcard ./*.hs)

all: $(TARGET)

run: $(TARGET)
	@./$< -1 tree.txt new_data.txt

train: $(TARGET)
	@./$< -2 train.txt

$(TARGET): $(FILES)
	ghc $(GHC_FLAGS) -o $@ $^

zip:
	zip Makefile README.md $(FILES)

clean:
	rm -rf $(TARGET) *.hi *.o
