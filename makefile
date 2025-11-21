.PHONY: all clean download compile-esdl2 build-gleam setup-esdl2

all: download compile-esdl2 build-gleam setup-esdl2
	@echo "All done!"
	@echo "You probably want to do 'gleam run' now!"

download:
	@echo "Downloading esdl2..."
	wget https://github.com/ninenines/esdl2/archive/refs/heads/master.zip
	unzip master.zip
	mv esdl2-master esdl2
	rm master.zip

compile-esdl2:
	@echo "Compiling esdl2..."
	cd esdl2 && rebar3 compile

build-gleam:
	@echo "Building the Gleam project..."
	gleam build

setup-esdl2:
	@echo "Doing some hacky magic for esdl2..."
	mkdir -p build/dev/erlang/esdl2/ebin
	mkdir -p build/dev/erlang/esdl2/include
	mkdir -p build/dev/erlang/esdl2/priv
	cp -r esdl2/_build/default/lib/esdl2/ebin/* build/dev/erlang/esdl2/ebin/
	cp -r esdl2/_build/default/lib/esdl2/include/* build/dev/erlang/esdl2/include/
	cp -r esdl2/_build/default/lib/esdl2/priv/* build/dev/erlang/esdl2/priv/

clean:
	@echo "Cleaning up..."
	rm -r esdl2
	rm -r build