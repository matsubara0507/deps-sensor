
image:
	stack --docker build --skip spec --copy-bins --local-bin-path=./bin
	docker build -t ${tag} . --build-arg local_bin_path=./bin
