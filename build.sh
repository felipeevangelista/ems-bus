#!/bin/bash
#
# Autor: Everton de Vargas Agilar
#
# Objetivo: Faz o build do projeto.
#
# Modo de usar: 
#
#    $ ./build.sh
#
#
#
## Software modification history:
#
# Data       |  Quem           |  Mensagem  
# -----------------------------------------------------------------------------------------------------
# 10/11/2015  Everton Agilar     Initial release script release
# 29/09/2017  Everton Agilar     Reads /etc/default/erlangms-build
# 05/05/2018  Everton Agilar     Build with docker
#
#
#
#
#
#
########################################################################################################

VERSION_SCRIPT="3.0.0"

if [ -z "$ERLANGMS_IN_DOCKER" ]; then
	echo "Build erlangms tool ( Version: $VERSION_SCRIPT   Hostname: `hostname` )"
fi

# Erlang Runtime version required > 20
ERLANG_VERSION=20

# Skip deps before build 
SKIP_DEPS="false"

# Skip clean before build 
SKIP_CLEAN="false"

KEEP_DB="false"

# Profile of build: local or docker
PROFILE="local"


# The settings may be stored in the /etc/default/erlangms-build
CONFIG_ARQ="/etc/default/erlangms-build"

# Prints a message and ends the system
# Parâmetros:
#  $1  - Mensagem que será impressa 
#  $2  - Código de retorno para o comando exit
die () {
    echo $1
    echo
    exit $2
}

# Prints on the screen the command help
help() {
	echo
	echo "How to use: ./build.sh"
	echo ""
	echo "Additional parameters:"
	echo "  --keep-db               -> Does not delete the priv/db folder"
	echo "  --skip-deps             -> Skip rebar deps before build"
	echo "  --skip-clean            -> Skip rebar clean before build"
	echo "  --keep-db=true|false    -> Define if delete the priv/db folder"
	echo "  --skip-dep=true|false   -> Define if skip rebar deps"
	echo "  --skip-clean=true|false -> Define if rebar clean"
	echo "  --clean                 -> Equal to --skip-clean=true"
	echo
	exit 1
}


# Reads a specific configuration file configuration. Accepts default if not set
# Parameters
#   $1 -> Nome da configuração. Ex. REGISTRY
#   $2 -> Valor default
le_setting () {
	KEY=$1
	DEFAULT=$2
	# Reads the setting value, removes leading spaces and makes the unquoted of the double quotation marks
	RESULT=$(egrep "^$KEY" $CONFIG_ARQ | cut -d"=" -f2 | sed -r 's/^ *//' | sed -r 's/^\"?(\<.*\>\$?)\"?$/\1/')
	if [ -z "$RESULT" ] ; then
		echo $DEFAULT
	else
		echo $RESULT
	fi
}	


# Reads the settings for running the default configuration file /etc/default/erlangms-docker
# These configurations can be redefined via the command line
le_all_settings () {
	printf "Verify if exist conf file $CONFIG_ARQ... "
	if [ -f "$CONFIG_ARQ" ]; then
		printf "OK\n"
		echo "Reading settings from $CONFIG_ARQ... OK"
		SKIP_DEPS=$(le_setting 'SKIP_DEPS' "$SKIP_DEPS")
		SKIP_CLEAN=$(le_setting 'SKIP_CLEAN' "$SKIP_CLEAN")
		KEEP_DB=$(le_setting 'KEEP_DB' "$KEEP_DB")
		ERLANG_VERSION=$(le_setting 'ERLANG_VERSION' "$ERLANG_VERSION" | sed 's/[^0-9]//g')
	else
		printf "NO\n"
	fi
}

## Checks if the version of erlang installed is compatible with this script
check_erlang_version(){
	printf "Checking Erlang Runtime version... "
	if [ -n "$ERLANG_VERSION_OS" ]; then
		if [ $ERLANG_VERSION_OS -ge $ERLANG_VERSION ]; then
			printf "OK\n"
		else
			printf "ERROR\n"
			die "Build canceled because the Erlang Runtime installed is incompatible with this software. Expected version: $ERLANG_VERSION"
		fi 
	else
		die "Oops, you should install Erlang Runtime $ERLANG_VERSION first !!!"
	fi
}

# Remove all deps except jiffy
function clean_deps(){
	echo "Clearing the deps folder (except jiffy) before build..."
	find ./deps  -maxdepth 1 -type d -not -name "*jiffy*" | sed '1d' | xargs rm -rf 
}

# ========================== main ==============================

if [ "$1" = "--help" ]; then
	help
fi

le_all_settings

# Read command line parameters
for P in $*; do
	if [[ "$P" =~ ^--.+$ ]]; then
		if [[ "$P" =~ ^--skip[\_-]deps=(true|false)$ ]]; then
			SKIP_DEPS="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--skip[\_-]clean=(true|false)$ ]]; then
			SKIP_CLEAN="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--keep[\_-]db=(true|false)$ ]]; then
			KEEP_DB="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ --skip[\_-]deps?$ ]]; then
			SKIP_DEPS="true"
		elif [[ "$P" =~ --skip[\_-]clean$ ]]; then
			SKIP_CLEAN="true"
		elif [ "$P" = "--clean" ]; then
			SKIP_CLEAN="false"
		elif [[ "$P" =~ --keep[\_-]db$ ]]; then
			KEEP_DB="true"
		elif [[ "$P" =~ ^--profile=(local|docker)$ ]]; then
			PROFILE="$(echo $P | cut -d= -f2)"
		elif [ "$P" = "--help" ]; then
			help
		else
			echo "Invalid parameter: $P"
			help
		fi
	fi
done


if [ "$PROFILE" = "docker" ]; then
	VERSION=$(cat src/ems_bus.app.src | sed -rn  's/^.*\{vsn.*([0-9]{1,2}\.[0-9]{1,2}.[0-9]{1,2}).*$/\1/p')
	IMAGE=erlangms

	sudo docker build -t $IMAGE:$VERSION $(dirname $0)

	ID=$(sudo docker images | grep "$IMAGE" | head -n 1 | awk '{print $3}')

	sudo docker tag "$ID" $IMAGE:latest
	sudo docker tag "$ID" $IMAGE:$VERSION
else
	# Erlang Runtime version installled
	ERLANG_VERSION_OS=`erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell 2> /dev/null | sed 's/[^0-9]//g'`

	# Get linux description
	LINUX_DESCRIPTION=$(awk -F"=" '{ if ($1 == "PRETTY_NAME"){ 
										gsub("\"", "", $2);  print $2 
									 } 
								   }'  /etc/os-release)

	if [ -s "$ERLANGMS_IN_DOCKER" ]; then
		check_erlang_version	
	fi

	echo "============================================================================="
	echo "Distro: $LINUX_DESCRIPTION"
	echo "Erlang version: $ERLANG_VERSION_OS"
	echo "Skip get-deps before build: $SKIP_DEPS" 
	echo "Skip clear before build: $SKIP_CLEAN" 
	echo "Keep database before build: $KEEP_DB" 
	echo "Date: $(date '+%d/%m/%Y %H:%M:%S')"
	echo "============================================================================="

	# Clean somes files
	rm -f *.dump

	if [ "$KEEP_DB" = "false" ]; then
		echo "Clearing the db folder before build..."
		rm -Rf priv/db
	fi	

	echo "Compiling the project erlangms..."
	if [ "$SKIP_DEPS" = "false" ]; then
		clean_deps
		if [ "$SKIP_CLEAN" = "false" ]; then	
			tools/rebar/rebar clean get-deps compile	
		else
			tools/rebar/rebar get-deps compile	
		fi
	else
		if [ "$SKIP_CLEAN" = "false" ]; then	
			tools/rebar/rebar clean compile	
		else
			tools/rebar/rebar compile	
		fi
	fi

	if [ "$?" = "1" ]; then
		echo "Oops, something wrong!"
	else
		echo "Ok!"
	fi

fi
