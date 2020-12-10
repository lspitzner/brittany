.PHONY: test
test:
	echo "test"
	stack test

.PHONY: test-all
test-all:
	$(MAKE) test test-8.8.4 test-8.6.5

.PHONY: test-8.8.4
test-8.8.4:
	echo "test 8.8.4"
	stack test --stack-yaml stack-8.8.4.yaml --work-dir .stack-work-8.8.4

.PHONY: test-8.6.5
test-8.6.5:
	echo "test 8.6.5"
	stack test --stack-yaml stack-8.6.5.yaml --work-dir .stack-work-8.6.5
