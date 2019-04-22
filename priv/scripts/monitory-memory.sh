#!/bin/bash
#
# Author: Everton de Vargas Agilar
# Date: 03/07/2017
#
# Goal: Deploy erlangms docker project
#
#
#
## Software modification history:
#
# Data       |  Quem           |  Mensagem  
# -----------------------------------------------------------------------------------------------------
# 28/07/2017  Everton Agilar     Initial release
# 05/03/2018  Everton Agilar     Add url_mask option in conf file
# 10/08/2018  Everton Agilar     Passa variáveis do ambiente rest_base_url e rest_auth_url para a imagem
# 16/10/2018  Everton Agilar     Quando informa --entrypoint, inicia em primeiro plano
#
#
#
########################################################################################################


IP_SERVER=""
LOGIN=""
PASSWORD=""
PORT=""
CONDITION="true"
RESULT="";

# Imprime na tela a ajuda do comando
help() {
	echo "Monitory Memory in server"
	echo "by erlangms: sudo ./monitory-memory.sh"
	echo ""
	echo "Additional parameters:"
	echo "  --ip_server              -> ip server for memory monitoring"
	echo "  --login       -> login for enter in server"
	echo "  --password        -> password for access server"
	echo "  --port       -> port for access server"
	echo "Obs.: Use only if have root access on server!"
	echo "Press control + c to quit this program!"
	exit 1
}


print_info() {
        echo "-----------------------------------------------------------------------------"
        echo "Memory result $RESULT"
        echo "-----------------------------------------------------------------------------"
}

# Se o arquivo for informado com --client_conf, então o arquivo não precisa ser gerado
verify_memory() {
	if [ "$IP_SERVER" != "" ] && [ "$LOGIN" != "" ] && [ "$PASSWORD" != "" ]; then
		while [ "$CONDITION" = "true" ]
		do
		  a="'sshpass -p $PASSWORD ssh -p $PORT  $LOGIN@$IP_SERVER 'free -m''"
	          echo $a	  
		  $a
		  print_info
                  sleep 600		  
		done
	fi
}


######################################## main ########################################

# Não precisa ser root para pedir ajuda
if [ "$1" = "--help" ]; then
	help
fi

# Make sure only root can run our script
if [[ $EUID -ne 0 ]]; then
   die "Only the root user can deploy docker images"
fi


# Read command line parameters
for P in $*; do
	if [[ "$P" =~ ^--.+$ ]]; then
		if [[ "$P" =~ ^--ip_server=.+$ ]]; then
			IP_SERVER="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--login=.+$ ]]; then
			LOGIN="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--password=.+$ ]]; then
			PASSWORD="$(echo $P | cut -d= -f2)"
		elif [[ "$P" =~ ^--port=.+$ ]]; then
			PORT="$(echo $P | cut -d= -f2)"
		else
                        echo "Invalid parameter: $P"
                        help
                fi
	else
		# Se for informado apenas um parâmetro e não começa com -- então é o nome do tarfile
		if [ "$#" = "1" ]; then
			TAR_FILE=$P
		else
			echo "Invalid parameter: $P"
			help
		fi
	fi
done

verify_memory

