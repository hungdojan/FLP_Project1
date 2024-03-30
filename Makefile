TARGET=flp-fun
GHC_FLAGS=-Wall -O2
FILES=$(wildcard ./*.hs)

all: $(TARGET)

run: $(TARGET)
	@./$< -1 tree.txt new_data.txt

train: $(TARGET)
	@./$< -2 train.txt

$(TARGET): $(FILES)
	ghc $(GHC_FLAGS) -o $@ $^

zip: clean
	zip xdohun00 Makefile README.md $(FILES)

clean:
	rm -rf $(TARGET) *.hi *.o *.zip
