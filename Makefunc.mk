#
# Makefunk.mk
#
# version: v1.8
# last update: 2018/12/04
#
# echo with color


##################################################
#
#  strings utils
#

STRCUT    = $(shell echo $1 | awk -F $2 '{print $3}')
STRCUTREV = $(shell echo $1 | awk -F $2 '{print $$(NF-$3)}')

##################################################
#
#  dictionary
#    (KEY)/(VAL) data class
#

MAKEDIC = $(join $(addsuffix /, $1), $2)
KEY2VAL = $(shell echo $1 | grep -Po '(?<=$2/)[^\s]+')
VAL2KEY = $(shell echo $1 | grep -Po '[^\s]+(?=/$2)')

##################################################
#
#  color
#

ECHO_COLOR    = printf "%b\e[%bm=== %b ===\e[m%b\n" $3 $1 $2 $4
ECHO_COLOR-   = printf "%b\e[%bm%b\e[m%b" $3 $1 $2 $4

ECHO_BLACK    = $(call ECHO_COLOR,"30",$1,$2,$3)
ECHO_RED      = $(call ECHO_COLOR,"31",$1,$2,$3)
ECHO_GREEN    = $(call ECHO_COLOR,"32",$1,$2,$3)
ECHO_YELLOW   = $(call ECHO_COLOR,"33",$1,$2,$3)
ECHO_BLUE     = $(call ECHO_COLOR,"34",$1,$2,$3)
ECHO_MAGENTA  = $(call ECHO_COLOR,"35",$1,$2,$3)
ECHO_CYAN     = $(call ECHO_COLOR,"36",$1,$2,$3)
ECHO_WHITE    = $(call ECHO_COLOR,"37",$1,$2,$3)

ECHO_BLACK-   = $(call ECHO_COLOR-,"30",$1,$2,$3)
ECHO_RED-     = $(call ECHO_COLOR-,"31",$1,$2,$3)
ECHO_GREEN-   = $(call ECHO_COLOR-,"32",$1,$2,$3)
ECHO_YELLOW-  = $(call ECHO_COLOR-,"33",$1,$2,$3)
ECHO_BLUE-    = $(call ECHO_COLOR-,"34",$1,$2,$3)
ECHO_MAGENTA- = $(call ECHO_COLOR-,"35",$1,$2,$3)
ECHO_CYAN-    = $(call ECHO_COLOR-,"36",$1,$2,$3)
ECHO_WHITE    = $(call ECHO_COLOR-,"37",$1,$2,$3)

COLOR_BLACK   = tput setaf 0
COLOR_RED     = tput setaf 1
COLOR_GREEN   = tput setaf 2
COLOR_YELLOW  = tput setaf 3
COLOR_BLUE    = tput setaf 4
COLOR_MAGENTA = tput setaf 5
COLOR_CYAN    = tput setaf 6
COLOR_WHITE   = tput setaf 7
COLOR_DEFAULT = tput sgr0

colortest:
	$(call ECHO_BLACK,   "black"  , "", "")
	$(call ECHO_RED,     "red"    , "", "")
	$(call ECHO_GREEN,   "green"  , "", "")
	$(call ECHO_YELLOW,  "yellow" , "", "")
	$(call ECHO_BLUE,    "blue"   , "", "")
	$(call ECHO_MAGENTA, "magenta", "", "")
	$(call ECHO_CYAN,    "cyan"   , "", "")
	$(call ECHO_WHOTE,   "white"  , "", "")
