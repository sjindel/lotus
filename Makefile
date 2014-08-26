LLVM_VERSION = 3.5
GTEST_DIR = gtest
LLVM_INCLUDE_DIR = /usr/include/llvm-$(LLVM_VERSION)/

.PHONY: all

all: gtest *.cc *.h JPP.hs
	clang++ -std=c++11 -O3 -Wall -Werror -isystem $(GTEST_DIR)/include \
	  -isystem $(LLVM_INCLUDE_DIR) -pthread lpprint_test.cc \
	  $(GTEST_DIR)/src/gtest_main.cc libgtest.a -o lpprint_test \
	  -ferror-limit=2
	ghc -c JPP.hs

clean:
	rm -rf *.o *.a lpprint_test
