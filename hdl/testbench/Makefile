# This Makefile can be called by the Continuous Integration (CI) tool to execute all
# testbenches added for CI
#
# Author: Adam Wujek, CERN 2017

TB_DIRS=simple_tb/modelsim

test_results_xml=test_results.xml
.PHONY: $(TB_DIRS)

all: $(TB_DIRS) summary summary_total summary_xml

$(TB_DIRS):
	@echo $@
	@echo "Run HDL-MAKE"
	cd "$@"; \
	$(HDLMAKE_PATH)/hdl-make 2>&1

	@echo "Run make"
	$(MAKE) -C $@ $(TARGET)

	@echo "Run vsim"
	cd "$@" ;\
	d="$@";\
	d=$${d////_}; \
	./run_all.sh | tee transcript."$$d".txt; \
	echo "vsim returned $$?"

summary: $(TB_DIRS)
	@echo "-------------------------------------------------------------------"
	@echo "Summary:"
	@for d in $(TB_DIRS); do \
		d_=$${d////_}; \
		if [ -f $$d/transcript."$$d_".txt ]; then \
			echo "Warnings for $$d:"; \
			cat $$d/transcript."$$d_".txt | grep Warning; \
			if [ $$? -eq 1 ]; then echo "None"; fi ;\
			echo "Errors for $$d:"; \
			cat $$d/transcript."$$d_".txt | grep Error; \
			if [ $$? -eq 1 ]; then echo "None"; fi ;\
			echo "Fatals for $$d:"; \
			cat $$d/transcript."$$d_".txt | grep Fatal; \
			if [ $$? -eq 1 ]; then echo "None"; fi ;\
		else \
			echo "No transcript file for $$d"; \
		fi \
	done

# Run tasks all before summary_total, because if there is a failure summary_total breaks the make execution
summary_total: summary summary_xml
	@echo "-------------------------------------------------------------------"
	@echo ""
	@echo "Summary total:"
	@echo "+---------------------------------------------------+----------+----------+----------+"
	@echo "| Test bench                                        | Warnings |  Errors  |  Fatals  |"
	@echo "+---------------------------------------------------+----------+----------+----------+"
	@is_error=0;\
	for d in $(TB_DIRS); do \
		d_=$${d////_}; \
		if [ -f $$d/transcript."$$d_".txt ]; then \
			printf "| %-50s" $$d; \
			printf "| %8d " `cat $$d/transcript."$$d_".txt | grep Warning | wc -l`; \
			error_n=`cat $$d/transcript."$$d_".txt | grep Error | wc -l`; \
			printf "| %8d " $$error_n;\
			if [ $$error_n -gt 0 ]; then is_error=1; fi ;\
			fatal_n=`cat $$d/transcript."$$d_".txt | grep Fatal | wc -l`; \
			printf "| %8d |\n" $$fatal_n;\
			if [ $$fatal_n -gt 0 ]; then is_error=1; fi ;\
		else \
			printf "| %-30s" $$d; \
			echo "| No transcript file! |"; is_error=1; \
		fi \
	done ;\
	echo "+---------------------------------------------------+----------+----------+----------+";\
	if [ $$is_error -gt 0 ]; then exit 1; fi ;

summary_xml: summary
	@echo '<?xml version="1.0" encoding="UTF-8"?>' > $(test_results_xml)
	@echo '<testsuites tests="0" failures="0" disabled="0" errors="0" time="0" name="AllTests">' >> $(test_results_xml)
	@for d in $(TB_DIRS); do \
		d_=$${d////_}; \
		is_test_error=0;\
		error_n=0;\
		fatal_n=0;\
		echo -n "  <testsuite name=\""$$d"\" tests=\"1\" failures=\"" >> $(test_results_xml) ;\
		if [ -f $$d/transcript."$$d_".txt ]; then \
			error_n=`cat $$d/transcript."$$d_".txt | grep Error | wc -l`; \
			fatal_n=`cat $$d/transcript."$$d_".txt | grep Fatal | wc -l`; \
			if [ $$error_n -gt 0 ] || [ $$fatal_n -gt 0 ]; then is_test_error=1; fi ;\
			echo -n $$is_test_error >> $(test_results_xml);\
		else \
			is_test_error=2; \
			echo -n "1" >> $(test_results_xml); \
		fi; \
		echo "\" disabled=\"0\" errors=\"0\" time=\"0\">" >> $(test_results_xml) ;\
		echo "    <testcase name=\""$$d"\" status=\"run\" time=\"0\" classname=\""Testbench"\">" >> $(test_results_xml) ;\
		if [ $$is_test_error -eq 1 ]; then \
			if [ $$error_n -gt 0 ]; then \
				echo "      <failure message=\"Errors\" type=\"\"><![CDATA[" >> $(test_results_xml) ;\
				cat $$d/transcript."$$d_".txt | grep Error >> $(test_results_xml);\
				echo "      ]]></failure>" >> $(test_results_xml) ;\
			fi;\
			if [ $$fatal_n -gt 0 ]; then \
				echo "      <failure message=\"Fatals\" type=\"\"><![CDATA[" >> $(test_results_xml) ;\
				cat $$d/transcript."$$d_".txt | grep Fatal >> $(test_results_xml);\
				echo "      ]]></failure>" >> $(test_results_xml) ;\
			fi;\
		fi ;\
		if [ $$is_test_error -eq 2 ]; then \
			echo "      <failure message=\"Output file not found\" type=\"\">" >> $(test_results_xml) ;\
			echo "<![CDATA[Output file not found. Testbench didnt run.]]>" >> $(test_results_xml) ;\
			echo "      </failure>" >> $(test_results_xml) ;\
		fi ;\
		echo "    </testcase>" >> $(test_results_xml) ;\
		echo "  </testsuite>" >> $(test_results_xml) ;\
	done ;\
	echo "</testsuites>" >> $(test_results_xml)

clean:
	@for d in $(TB_DIRS); do \
		if [ -f $$d/Makefile ]; then \
			$(MAKE) -C $$d $@; \
			rm -f $$d/Makefile; \
		fi \
	done

