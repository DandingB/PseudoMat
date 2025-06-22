default:
	dune build
	dune exec ./Main.exe test.psu

unit-test:
	dune test --force
	
integration-test:
ifeq ($(OS),Windows_NT)
	@for %%f in (Test_Programs\*.psu) do ( \
		set fname=%%~nxf & \
		echo. & \
		echo Testing !fname! & \
		dune exec ./Main.exe -- "%%f" \
	)
else
	@for file in Test_Programs/*.psu; do \
		filename=$$(basename $$file); \
		echo  "Testing $$filename"; \
		dune exec ./Main.exe -- "$$file"; \
	done
endif

performance-test:
	python3 performance_test/performancePython.py
	dune exec ./Main.exe performance_test/performance.psu
