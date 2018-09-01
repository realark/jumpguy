ifneq ($(OS),Windows_NT)
# seems to already be set for windows
	OS := $(shell uname)
endif
# Windows_NT Darwin Linux

ifeq ($(OS),Windows_NT)
	APP_SRC = $(powershell Get-ChildItem -Path "src/*.lisp" -Recurse)
	TEST_SRC = $(powershell Get-ChildItem -Path "t/*.lisp" -Recurse)
	RESOURCE_DIRS = $(powershell Get-ChildItem -Name "media/" -Recurse -Directory)
	RESOURCES = $(powershell Get-ChildItem -Name "media/" -Recurse -File)
	LAUNCHERS = $(powershell Get-ChildItem -Name "launchers/" -Recurse -File)
else
	APP_SRC = $(shell find src/ -name \*.lisp)
	TEST_SRC = $(shell find t/ -name \*.lisp)
	RESOURCE_DIRS = $(shell find media/ -type d)
	RESOURCES = $(shell find media/ -type f)
	LAUNCHERS = $(shell find launchers/ -type f -printf "%f\n")
endif

.PHONY: clean build-dirs

zip: build build-resources
	rm build/jumpguy-*.zip
	cd build/ && zip -r jumpguy-`git describe --tags --abbrev=0 || git rev-parse --short HEAD`.zip jumpguy
bundle: prep-release build/vert
	git describe --tags --abbrev=0 || git rev-parse --short HEAD # stop if not in git
	cd build && zip -r vert-`git describe --tags --abbrev=0 || git rev-parse --short HEAD`.zip vert
build: build-dirs $(APP_SRC)
	ros build build-jumpguy.ros
ifeq ($(OS),Darwin)
	mv build-jumpguy build/jumpguy/jumpguy-osx
else # Linux I hope
	mv build-jumpguy build/jumpguy/jumpguy-linux
endif
build-resources: build-dirs $(RESOURCES)
	@rm -rf build/jumpguy/media
	@rm -f build/jumpguy/launcher.sh
	for dir in $(RESOURCE_DIRS) ; do \
		mkdir -p build/jumpguy/$$dir ; \
	done
	for resource in $(RESOURCES) ; do \
		ln $$resource build/jumpguy/$$resource ; \
	done
	for launcher in $(LAUNCHERS) ; do \
		ln launchers/$$launcher build/jumpguy/$$launcher ; \
	done
build-dirs:
	@if [ ! -d build/jumpguy/media ]; then mkdir -p build/jumpguy/media || (echo "Unable to make build dirs" ; exit -1); fi
clean:
	@if [ -d build ]; then rm -rf build; fi
	@echo "Project clean"
