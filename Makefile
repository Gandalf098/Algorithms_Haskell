# This is the default target, which will be built when 
# you invoke make
all: mergesort inversioncount

# This rule tells make how to build hello from hello.cpp
mergesort: 1_MergeSort.hs
	ghc 1_MergeSort.hs

inversioncount: 1_InversionCount.hs
	ghc 1_InversionCount.hs

# This rule tells make to delete hello and hello.o
clean:
	rm *.o
	rm *.hi
	rm 1_MergeSort
	rm 1_InversionCount
