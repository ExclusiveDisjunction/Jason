build_dbg = ./build
build_rls = ./build/release
pub_dir = ./bin

clean:
	@rm -rf $(build_dbg)

init:
	@echo "Debug:"
	@cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_MAKE_PROGRAM=ninja -G Ninja -S ./ -B $(build_dbg)
	@echo ""
	@echo "Release:"
	@cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_MAKE_PROGRAM=ninja -G Ninja -S ./ -B $(build_rls)

build:
	@cmake --build $(build_dbg) --target Jason -j 4

build-r:
	@cmake --build $(build_rls) --target Jason -j 4


run:
	@$(MAKE) build
	@$(build_dbg)/Jason

run-r:
	@$(MAKE) build-r
	@$(build_rls)/Jason
publish:
	@$(MAKE) build-r
	@cp $(build_rls)/Jason $(pub_dir)/Jason

