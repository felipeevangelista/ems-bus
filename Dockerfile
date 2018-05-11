FROM erlang:20.3.6

MAINTAINER evertonagilar <evertonagilar@gmail.com>

ENV HOME /var/opt/erlangms
ENV ERLANGMS_IN_DOCKER true

WORKDIR $HOME

# Source list
RUN echo deb http://ftp.br.debian.org/debian stretch main contrib non-free             >> /etc/apt/sources.list
RUN echo deb http://security.debian.org/ stretch/updates main contrib non-free         >> /etc/apt/sources.list
RUN echo deb http://ftp.br.debian.org/debian/ stretch-updates main contrib non-free    >> /etc/apt/sources.list

# Atualiza o apt
RUN apt-get update


# Define o locale para pt_BR.UTF-8
RUN apt-get install locales && locale-gen pt_BR.UTF-8  
ENV LANG pt_BR.UTF-8  
ENV LANGUAGE pt_BR:pt:en 
ENV LC_ALL pt_BR.UTF-8


# Define timezone para horario de Brasilia (America/Sao_Paulo)
RUN echo America/Sao_Paulo > /etc/timezone && \
    ln -sf /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime && \
    dpkg-reconfigure --frontend noninteractive tzdata
ENV TZ America/Sao_Paulo


# Alguns softwares uteis para administracao
RUN apt-get update && \
    apt-get install -q -y --no-install-recommends curl wget zip unzip vim
    

# Alguns softwares importantes para o barramento
RUN apt-get install unixodbc tdsodbc freetds-common odbcinst1debian2 odbcinst libcppdb-sqlite3-0 libodbc1 libiodbc2 libcppdb-odbc0 libltdl7 libcppdb0 ldap-utils


RUN cd $HOME && \
	echo "Build ErlangMS from https://github.com/erlangms/ems-bus on $(pwd)" && \
	git clone https://github.com/erlangms/ems-bus && \
	cd ems-bus && \
	git checkout v1.0.25.ldap && \
	./build.sh --profile=local

# Expose the ports we're interested in
EXPOSE 2300 2301 2389 4369

VOLUME ~/.erlangms
VOLUME ~/.odbc.ini

CMD ["/var/opt/erlangms/ems-bus/start.sh"]


