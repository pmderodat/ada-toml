prefix = /usr
DESTDIR = $(prefix)

LIBRARY_TYPES = static static-pic relocatable
BUILD_MODE = prod

PROCESSORS = 0

all: build

build: build-static build-static-pic build-relocatable
build-%:
	gprbuild -Pada_toml \
		-XBUILD_MODE=$(BUILD_MODE) \
		-XLIBRARY_TYPE=$* \
		-p -j$(PROCESSORS)

install: install-static install-static-pic install-relocatable
install-%: build-%
	gprinstall -Pada_toml \
		-XBUILD_MODE=$(BUILD_MODE) \
		-XLIBRARY_TYPE=$* \
		--sources-subdir=include/ada-toml \
		--prefix="$(DESTDIR)" \
		--build-name=$* \
		--build-var=LIBRARY_TYPE \
		--build-var=ADA_TOML_LIBRARY_TYPE \
		-p

clean: clean-static clean-static-pic clean-relocatable
clean-%:
	gprclean -Pada_toml \
		-XBUILD_MODE=$(BUILD_MODE) \
		-XLIBRARY_TYPE=$* \
		-p
