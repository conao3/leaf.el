# echo with color

ECHO_COLOR   = @tput setaf $1; echo $2; tput sgr0
ECHO_BLACK   = $(call ECHO_COLOR, 0, $1)
ECHO_RED     = $(call ECHO_COLOR, 1, $1)
ECHO_GREEN   = $(call ECHO_COLOR, 2, $1)
ECHO_YELLOW  = $(call ECHO_COLOR, 3, $1)
ECHO_BLUE    = $(call ECHO_COLOR, 4, $1)
ECHO_MAGENTA = $(call ECHO_COLOR, 5, $1)
ECHO_CYAN    = $(call ECHO_COLOR, 6, $1)
ECHO_WHITE   = $(call ECHO_COLOR, 7, $1)

colortest:
	$(call ECHO_BLACK, black)
	$(call ECHO_RED, red)
	$(call ECHO_GREEN, green)
	$(call ECHO_YELLOW, yellow)
	$(call ECHO_BLUE, blue)
	$(call ECHO_MAGENTA, magenta)
	$(call ECHO_CYAN, cyan)
	$(call ECHO_WHOTE, white)
